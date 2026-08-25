"""
Compute ALE-based lag importance curves for dynamic features of a trained
water_temp_lstm model, and save the results as a CSV table.

For each feature and each tested lag (days before the target date), an
ALE curve is computed and summarized into a single scalar importance
value (population-weighted standard deviation of the ALE curve around
zero).
"""

import os
os.environ["TF_ENABLE_ONEDNN_OPTS"] = "0"
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

from pathlib import Path

import torch
import numpy as np
import pandas as pd
from torch.utils.data import ConcatDataset

from water_temp_lstm.interpretability.loading import load_cfg, load_model, load_test_datasets, build_loader, clone_batch
from water_temp_lstm.interpretability.utils import get_feature_stats


# ======================================================================
# Configuration
# ======================================================================
RUN_DIR = Path("runs/water_temp_lstm_3006_234701")
EPOCH = 22
N_BINS = 20
LAGS_TO_TEST = list(range(0, 365, 5))  # every 5th day (73 instead of 365 lags)

# Directory where the resulting CSV is stored.
DIR_RESULTS = Path("data/results")
RESULTS_CSV_NAME = "lag_importance.csv"

# English feature titles (stored in the output table for convenience
# when plotting)
FEATURE_TITLES = {
    "Ta_C": "Air Temperature",
    "P_mm": "Precipitation",
    "wind_ms": "Wind Speed",
    "rad_whm2": "Solar Radiation",
    "Q": "Discharge",
    "relhum": "Relative Humidity",
}


# ======================================================================
# Functions
# ======================================================================
def feature_label(feature: str) -> str:
    return FEATURE_TITLES.get(feature, feature)


def load_model_and_data(run_dir: Path, epoch: int):
    """Load config, model, and a single batch containing the full
    (concatenated) test dataset."""
    cfg = load_cfg(run_dir)
    tester = load_model(cfg, run_dir, epoch=epoch)
    model = tester.model

    datasets = load_test_datasets(cfg, tester)
    combined_dataset = ConcatDataset(list(datasets.values()))

    batch_size = len(combined_dataset)
    loader = build_loader(datasets, batch_size)
    full_batch = next(iter(loader))

    return cfg, tester, model, loader, full_batch


def compute_ale_local_effects(feature, data, model, mean, std, target_mean, target_std,
                               n_bins=20, lag=0):
    """
    Compute quantile bins and the per-sample local prediction difference
    for a dynamic feature at a given time step (lag) before the target
    date.

    Parameters
    ----------
    feature : str
        Name of the dynamic feature.
    data : dict
        Full batch (x_d, x_s, y, date).
    model : torch.nn.Module
    mean, std : float
        Scaler stats of the feature.
    target_mean, target_std : float
        Scaler stats of the target variable.
    n_bins : int, optional (default=20)
    lag : int, optional (default=0)
        Number of days before the target date. lag=0 = today (last time
        step), lag=1 = yesterday, etc. Must satisfy 0 <= lag <= seq_length - 1.

    Returns
    -------
    dict (as in the ALE script), plus:
        lag : int
            The lag value, passed through for tracking in aggregations.
    """
    seq_len = data["x_d"][feature].shape[1]
    if not (0 <= lag <= seq_len - 1):
        raise ValueError(f"lag={lag} outside the valid range [0, {seq_len - 1}]")
    t = -lag - 1

    feature_vals_norm = data["x_d"][feature][:, t, :].squeeze().numpy()
    feature_vals_raw = feature_vals_norm * std + mean
    valid_mask = ~np.isnan(feature_vals_raw)

    if not valid_mask.all():
        print(f"{feature} (lag={lag}): excluding {(~valid_mask).sum()} NaN values")

    quantile_probs = np.linspace(0, 1, n_bins + 1)
    bin_edges_raw = np.unique(np.quantile(feature_vals_raw[valid_mask], quantile_probs))
    n_bins_actual = len(bin_edges_raw) - 1

    bin_idx = np.digitize(feature_vals_raw, bin_edges_raw[1:-1], right=False)
    bin_idx = np.clip(bin_idx, 0, n_bins_actual - 1)

    x_lower_raw = bin_edges_raw[bin_idx]
    x_upper_raw = bin_edges_raw[bin_idx + 1]
    x_lower_norm = (x_lower_raw - mean) / std
    x_upper_norm = (x_upper_raw - mean) / std

    def predict_with_value(values_norm):
        batch_manip = clone_batch(data)
        dtype = batch_manip["x_d"][feature].dtype
        batch_manip["x_d"][feature][:, t, :] = torch.tensor(values_norm, dtype=dtype).unsqueeze(1)
        with torch.no_grad():
            output = model(batch_manip)
        return output["y_hat"][:, -1, :].squeeze().numpy() * target_std + target_mean

    preds_lower = predict_with_value(x_lower_norm)
    preds_upper = predict_with_value(x_upper_norm)
    local_effect = preds_upper - preds_lower

    return {
        "bin_edges_raw": bin_edges_raw,
        "bin_idx": bin_idx,
        "local_effect": local_effect,
        "valid_mask": valid_mask & ~np.isnan(local_effect),
        "feature_vals_raw": feature_vals_raw,
        "lag": lag,
    }


def aggregate_ale(local: dict, sample_filter=None) -> dict:
    """
    Aggregate already-computed local effects (from
    compute_ale_local_effects) into a finished ALE curve: bin means,
    cumulative sum, centering.

    Parameters
    ----------
    local : dict
        Return value of compute_ale_local_effects().
    sample_filter : np.ndarray [n_samples], dtype=bool, optional
        Additional boolean mask, combined with the valid mask from
        `local` via AND. None = no additional filtering.

    Returns
    -------
    dict with:
        bin_edges_raw : np.ndarray [n_bins_actual + 1]
        ale_values : np.ndarray [n_bins_actual + 1]
        bin_counts : np.ndarray [n_bins_actual], dtype=int
        feature_vals_raw: np.ndarray [sum(valid_mask & sample_filter)]
    """
    bin_edges_raw = local["bin_edges_raw"]
    bin_idx = local["bin_idx"]
    local_effect = local["local_effect"]
    valid_mask = local["valid_mask"]
    feature_vals_raw = local["feature_vals_raw"]
    n_bins_actual = len(bin_edges_raw) - 1

    if sample_filter is not None:
        valid_mask = valid_mask & sample_filter

    bin_means = np.full(n_bins_actual, np.nan)
    bin_counts = np.zeros(n_bins_actual, dtype=int)
    for b in range(n_bins_actual):
        in_bin = valid_mask & (bin_idx == b)
        bin_counts[b] = in_bin.sum()
        if bin_counts[b] > 0:
            bin_means[b] = local_effect[in_bin].mean()

    bin_means_filled = np.nan_to_num(bin_means, nan=0.0)
    ale_uncentered = np.concatenate([[0.0], np.cumsum(bin_means_filled)])

    bin_midpoint_ale = (ale_uncentered[:-1] + ale_uncentered[1:]) / 2
    weighted_mean = np.average(bin_midpoint_ale, weights=bin_counts) if bin_counts.sum() > 0 else 0.0
    ale_centered = ale_uncentered - weighted_mean

    return {
        "bin_edges_raw": bin_edges_raw,
        "ale_values": ale_centered,
        "bin_counts": bin_counts,
        "feature_vals_raw": feature_vals_raw[valid_mask],
    }


def ale_importance(ale_result: dict) -> float:
    """
    Compute a scalar importance measure from an aggregated ALE curve:
    the population-density-weighted standard deviation of the ALE
    values around zero.

    Parameters
    ----------
    ale_result : dict
        Result of aggregate_ale(). Expects keys "ale_values", "bin_counts".

    Returns
    -------
    float
        Weighted standard deviation of the ALE values, in °C.
    """
    ale_values = ale_result["ale_values"]
    bin_counts = ale_result["bin_counts"]

    if bin_counts.sum() == 0:
        return np.nan

    edge_weights = np.concatenate([
        [bin_counts[0]],
        (bin_counts[:-1] + bin_counts[1:]) / 2,
        [bin_counts[-1]],
    ])

    weighted_mean = np.average(ale_values, weights=edge_weights)
    weighted_var = np.average((ale_values - weighted_mean) ** 2, weights=edge_weights)
    return float(np.sqrt(weighted_var))


def compute_lag_importance(feature, data, model, mean, std, target_mean, target_std,
                            lags, n_bins=20) -> dict:
    """
    For a list of lags, compute the ALE curve and, from it, the ALE
    importance, as a measure of how much that time step influences the
    prediction.

    Parameters
    ----------
    feature : str
    data : dict
    model : torch.nn.Module
    mean, std : float
    target_mean, target_std : float
    lags : list[int] or np.ndarray[int]
    n_bins : int, optional (default=20)

    Returns
    -------
    dict with:
        lags : list[int]
        importance : list[float]
    """
    importance_by_lag = {}
    for lag in lags:
        local = compute_ale_local_effects(feature, data, model, mean, std,
                                           target_mean, target_std, n_bins=n_bins, lag=lag)
        ale_result = aggregate_ale(local)
        importance_by_lag[lag] = ale_importance(ale_result)

    return {
        "lags": list(lags),
        "importance": [importance_by_lag[lag] for lag in lags],
    }


def results_to_dataframe(lag_results_by_feature: dict, feature_titles: dict) -> pd.DataFrame:
    """Convert per-feature lag importance results into a tidy DataFrame:
    one row per (feature, lag)."""
    rows = []
    for feature, lag_result in lag_results_by_feature.items():
        for lag, importance in zip(lag_result["lags"], lag_result["importance"]):
            rows.append({
                "feature": feature,
                "feature_title": feature_titles.get(feature, feature),
                "lag": lag,
                "importance": importance,
            })

    return pd.DataFrame(rows)


def save_results(df: pd.DataFrame, out_dir: Path, filename: str) -> Path:
    """Save the tidy results DataFrame as a CSV."""
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / filename
    df.to_csv(out_path, index=False)
    return out_path


# ======================================================================
# Main
# ======================================================================
def main():
    cfg, tester, model, loader, full_batch = load_model_and_data(RUN_DIR, EPOCH)

    wt_mean, wt_sd = get_feature_stats(tester, "wt")

    lag_results_by_feature = {}
    for feature in cfg.dynamic_inputs:
        mean, std = get_feature_stats(tester, feature)
        lag_results_by_feature[feature] = compute_lag_importance(
            feature, full_batch, model, mean, std,
            wt_mean, wt_sd, lags=LAGS_TO_TEST, n_bins=N_BINS
        )

    df = results_to_dataframe(lag_results_by_feature, FEATURE_TITLES)

    out_path = save_results(df, DIR_RESULTS, RESULTS_CSV_NAME)
    print(f"Done. Saved CSV to: {out_path.resolve()}")


if __name__ == "__main__":
    main()