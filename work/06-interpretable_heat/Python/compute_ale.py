"""
Compute Accumulated Local Effects (ALE) curves - overall and per season -
for dynamic features of a trained water_temp_lstm model, and save the
results as CSV tables.
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
from water_temp_lstm.interpretability.utils import get_feature_stats, collect_target_dates


# ======================================================================
# Configuration
# ======================================================================
RUN_DIR = Path("runs/water_temp_lstm_3006_234701")
EPOCH = 22
N_BINS = 50

# Directory where the resulting CSVs are stored.
DIR_RESULTS = Path("data/results")
CURVES_CSV_NAME = "ale_curves.csv"
RUG_CSV_NAME = "ale_rug.csv"

# English feature titles (stored in the output tables for convenience
# when plotting)
FEATURE_TITLES = {
    "Ta_C": "Air Temperature",
    "P_mm": "Precipitation",
    "wind_ms": "Wind Speed",
    "rad_whm2": "Solar Radiation",
    "Q": "Discharge",
    "relhum": "Relative Humidity",
}

SEASON_MONTHS = {
    "Winter (DJF)": [12, 1, 2],
    "Spring (MAM)": [3, 4, 5],
    "Summer (JJA)": [6, 7, 8],
    "Fall (SON)": [9, 10, 11],
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


def build_season_masks(target_dates) -> dict:
    """Build boolean masks for the four meteorological seasons, based on
    the month of each target date."""
    months = pd.to_datetime(target_dates).month.values
    return {name: np.isin(months, m) for name, m in SEASON_MONTHS.items()}


def compute_ale_local_effects(feature, data, model, mean, std, target_mean, target_std, n_bins=20):
    """
    Compute quantile bins and the per-sample local prediction difference
    for a dynamic feature at the last time step (today). Runs the actual,
    expensive model forward passes (2 per feature: lower and upper bin
    edge). Only needed once per feature, regardless of how often the
    results are aggregated afterwards (global, seasonal, etc.).

    Parameters
    ----------
    feature: str
        Name of the dynamic feature (e.g. "Ta_C")
    data: dict
        One single, complete batch with keys
            "x_d": dict(str, Tensor [n_samples, seq_length, 1]),
            "x_s": Tensor [n_samples, n_static],
            "y": Tensor [n_samples, seq_length, 1],
            "date": np.ndarray [n_samples, seq_length]
    model: torch.nn.Module
        Trained model in eval() mode
    mean, std: float
        Scaler stats (mean/sd) of the feature for (de-)normalization
    target_mean, target_std: float
        Scaler stats of the target variable for back-transforming
        predictions to the original scale (°C)
    n_bins: int, optional (default=20)
        Desired number of quantile bins. May be automatically reduced
        if the values are highly clustered (see console output).

    Returns
    -------
    dict with:
        bin_edges_raw: np.ndarray [n_bins_actual + 1]
            Bin boundaries on the original scale (sorted ascending, unique)
        bin_idx: np.ndarray [n_samples], dtype=int
            Bin index each sample is assigned to (based on feature value)
        local_effect: np.ndarray [n_samples]
            Prediction difference (upper minus lower bin boundary) per
            sample, in °C
        valid_mask: np.ndarray [n_samples], dtype=bool
            True for samples whose feature value AND local prediction
            difference are not NaN (e.g. due to NaN gaps elsewhere in the
            window).
        feature_vals_raw: np.ndarray [n_samples]
            Actual feature value (today) of each sample, original scale.
    """
    data_copy = clone_batch(data)

    feature_vals_norm = data_copy["x_d"][feature][:, -1, :].squeeze().numpy()  # [n_samples]
    feature_vals_raw = feature_vals_norm * std + mean

    valid_mask = ~np.isnan(feature_vals_raw)
    if not valid_mask.all():
        print(f"{feature}: excluding {(~valid_mask).sum()} NaN values at the last time step")

    # 1. Build quantile bins (based on valid values only)
    quantile_probs = np.linspace(0, 1, n_bins + 1)
    bin_edges_raw = np.quantile(feature_vals_raw[valid_mask], quantile_probs)
    bin_edges_raw = np.unique(bin_edges_raw)  # remove duplicate edges (many identical values)
    n_bins_actual = len(bin_edges_raw) - 1

    if n_bins_actual < n_bins:
        print(f"{feature}: only {n_bins_actual} unique bins possible (instead of {n_bins}), "
              f"due to repeated identical quantile values")

    # 2. Assign each sample to its bin
    # np.digitize: rightmost bin edge inclusive, so the maximum isn't dropped
    bin_idx = np.digitize(feature_vals_raw, bin_edges_raw[1:-1], right=False)
    bin_idx = np.clip(bin_idx, 0, n_bins_actual - 1)

    x_lower_raw = bin_edges_raw[bin_idx]
    x_upper_raw = bin_edges_raw[bin_idx + 1]
    x_lower_norm = (x_lower_raw - mean) / std
    x_upper_norm = (x_upper_raw - mean) / std

    # 3. Predictions at lower and upper bin edge
    def predict_with_value(values_norm):
        batch_manip = clone_batch(data)
        dtype = batch_manip["x_d"][feature].dtype
        batch_manip["x_d"][feature][:, -1, :] = torch.tensor(values_norm, dtype=dtype).unsqueeze(1)
        with torch.no_grad():
            output = model(batch_manip)
        return (output["y_hat"][:, -1, :].squeeze().numpy()) * target_std + target_mean

    preds_lower = predict_with_value(x_lower_norm)
    preds_upper = predict_with_value(x_upper_norm)
    local_effect = preds_upper - preds_lower  # [n_samples], °C

    return {
        "bin_edges_raw": bin_edges_raw,
        "bin_idx": bin_idx,
        "local_effect": local_effect,
        "valid_mask": valid_mask & ~np.isnan(local_effect),
        "feature_vals_raw": feature_vals_raw,
    }


def aggregate_ale(local: dict, sample_filter=None) -> dict:
    """
    Aggregate already-computed local effects (from
    compute_ale_local_effects) into a finished ALE curve: bin means,
    cumulative sum, centering. Can be called multiple times with
    different sample_filter masks on the same local effects without
    repeating the forward passes.

    Parameters
    ----------
    local : dict
        Return value of compute_ale_local_effects().
    sample_filter : np.ndarray [n_samples], dtype=bool, optional
        Additional boolean mask (e.g. only samples of a given season),
        combined with the valid mask from `local` via AND. None = no
        additional filtering, all valid samples are used.

    Returns
    -------
    dict with:
        bin_edges_raw : np.ndarray [n_bins_actual + 1]
            Bin edges in original scale (passed through from `local`).
        ale_values : np.ndarray [n_bins_actual + 1]
            Centered ALE values at each bin edge, in °C. One value per
            bin edge (not per bin!).
        bin_counts : np.ndarray [n_bins_actual], dtype=int
            Number of (filtered, valid) samples per bin.
        feature_vals_raw: np.ndarray [sum(local["valid_mask"] & sample_filter)]
            Actual feature value (today) of each sample, original scale.
    """
    bin_edges_raw = local["bin_edges_raw"]
    bin_idx = local["bin_idx"]
    local_effect = local["local_effect"]
    valid_mask = local["valid_mask"]
    feature_vals_raw = local["feature_vals_raw"]
    n_bins_actual = len(bin_edges_raw) - 1

    if sample_filter is not None:
        valid_mask = valid_mask & sample_filter

    # 4. Average within each bin
    bin_means = np.full(n_bins_actual, np.nan)
    bin_counts = np.zeros(n_bins_actual, dtype=int)
    for b in range(n_bins_actual):
        in_bin = valid_mask & (bin_idx == b)
        bin_counts[b] = in_bin.sum()
        if bin_counts[b] > 0:
            bin_means[b] = local_effect[in_bin].mean()

    bin_means_filled = np.nan_to_num(bin_means, nan=0.0)  # empty bins contribute 0 to cumsum

    # 5. Cumulative sum -> uncentered ALE values at the bin edges
    ale_uncentered = np.concatenate([[0.0], np.cumsum(bin_means_filled)])  # [n_bins_actual + 1]

    # 6. Center: subtract the weighted mean
    bin_midpoint_ale = (ale_uncentered[:-1] + ale_uncentered[1:]) / 2
    weighted_mean = np.average(bin_midpoint_ale, weights=bin_counts) if bin_counts.sum() > 0 else 0.0
    ale_centered = ale_uncentered - weighted_mean

    return {
        "bin_edges_raw": bin_edges_raw,
        "ale_values": ale_centered,
        "bin_counts": bin_counts,
        "feature_vals_raw": feature_vals_raw[valid_mask],
    }


def compute_ale(feature, data, model, mean, std, target_mean, target_std, n_bins=20, season_masks=None) -> dict:
    """
    Main entry point for ALE of a single feature. Computes the local
    effects once and aggregates them globally as well as, optionally,
    per given season mask, without repeating the forward passes.

    Parameters
    ----------
    feature : str
        Name of the dynamic feature.
    data : dict
        Full batch, see compute_ale_local_effects().
    model : torch.nn.Module
        Trained model in eval() mode.
    mean, std : float
        Scaler stats of the feature.
    target_mean, target_std : float
        Scaler stats of the target variable.
    n_bins : int, optional (default=20)
        Number of quantile bins.
    season_masks : dict[str, np.ndarray [n_samples] bool] or None, optional
        Optional dict mapping season label to boolean mask (e.g. from
        build_season_masks()). None = no seasonal split.

    Returns
    -------
    dict[str, dict]
        Key "overall" always contains the global ALE curve (same format
        as aggregate_ale()). If season_masks was given, one further key
        per season label with the same structure.
    """
    local = compute_ale_local_effects(feature, data, model, mean, std, target_mean, target_std, n_bins)
    results = {"overall": aggregate_ale(local)}
    if season_masks:
        for label, mask in season_masks.items():
            results[label] = aggregate_ale(local, sample_filter=mask)
    return results


def results_to_curves_dataframe(ale_results_full: dict, feature_titles: dict) -> pd.DataFrame:
    """
    Convert per-feature ALE results into a tidy (long-format) DataFrame
    of ALE curves: one row per (feature, season, bin edge).

    bin_count is associated with the bin to the right of the edge, except
    for the last edge, which reuses the count of the last bin. This lets
    the plotting side reconstruct "which edges have any data" the same
    way the original matplotlib script did (via np.where(bin_counts > 0)).
    """
    rows = []
    for feature, results in ale_results_full.items():
        for season_label, res in results.items():
            bin_edges_raw = res["bin_edges_raw"]
            ale_values = res["ale_values"]
            bin_counts = res["bin_counts"]
            n_bins_actual = len(bin_counts)

            for edge_idx, (x_val, ale_val) in enumerate(zip(bin_edges_raw, ale_values)):
                bin_for_count = min(edge_idx, n_bins_actual - 1)
                rows.append({
                    "feature": feature,
                    "feature_title": feature_titles.get(feature, feature),
                    "season": season_label,
                    "edge_idx": edge_idx,
                    "x_val": x_val,
                    "ale_value": ale_val,
                    "bin_count": bin_counts[bin_for_count],
                })

    return pd.DataFrame(rows)


def results_to_rug_dataframe(ale_results_full: dict, feature_titles: dict) -> pd.DataFrame:
    """
    Convert per-feature ALE results into a tidy (long-format) DataFrame
    of feature values used for the seasonal rug plot: one row per
    (feature, season, sample).
    """
    rows = []
    for feature, results in ale_results_full.items():
        for season_label, res in results.items():
            if season_label == "overall":
                continue
            for val in res["feature_vals_raw"]:
                rows.append({
                    "feature": feature,
                    "feature_title": feature_titles.get(feature, feature),
                    "season": season_label,
                    "value": val,
                })

    return pd.DataFrame(rows)


def save_results(df_curves: pd.DataFrame, df_rug: pd.DataFrame, out_dir: Path,
                  curves_filename: str, rug_filename: str) -> tuple[Path, Path]:
    """Save the curves and rug DataFrames as CSVs."""
    out_dir.mkdir(parents=True, exist_ok=True)
    curves_path = out_dir / curves_filename
    rug_path = out_dir / rug_filename
    df_curves.to_csv(curves_path, index=False)
    df_rug.to_csv(rug_path, index=False)
    return curves_path, rug_path


# ======================================================================
# Main
# ======================================================================
def main():
    cfg, tester, model, loader, full_batch = load_model_and_data(RUN_DIR, EPOCH)

    wt_mean, wt_sd = get_feature_stats(tester, "wt")

    target_dates = collect_target_dates(loader)
    season_masks = build_season_masks(target_dates)

    ale_results_full = {}
    for feature in cfg.dynamic_inputs:
        mean, sd = get_feature_stats(tester, feature)
        ale_results_full[feature] = compute_ale(
            feature, full_batch, model, mean, sd, wt_mean, wt_sd,
            n_bins=N_BINS, season_masks=season_masks
        )

    df_curves = results_to_curves_dataframe(ale_results_full, FEATURE_TITLES)
    df_rug = results_to_rug_dataframe(ale_results_full, FEATURE_TITLES)

    curves_path, rug_path = save_results(df_curves, df_rug, DIR_RESULTS, CURVES_CSV_NAME, RUG_CSV_NAME)
    print(f"Done. Saved CSVs to:\n  {curves_path.resolve()}\n  {rug_path.resolve()}")


if __name__ == "__main__":
    main()