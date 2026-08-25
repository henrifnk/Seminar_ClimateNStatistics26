"""
Compute partial dependence / ICE (offset influence) curves for dynamic
features of a trained water_temp_lstm model and save the results as CSV file.
"""

import os
os.environ["TF_ENABLE_ONEDNN_OPTS"] = "0"
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

from pathlib import Path

import torch
import numpy as np
import pandas as pd

from water_temp_lstm.interpretability.loading import load_cfg, load_model, load_test_datasets, build_loader, clone_batch
from water_temp_lstm.interpretability.inference import run_inference
from water_temp_lstm.interpretability.utils import get_feature_stats


# ======================================================================
# Configuration
# ======================================================================
RUN_DIR = Path("runs/water_temp_lstm_3006_234701")
EPOCH = 22
BATCH_SIZE = 256

# Directory where the resulting CSV is stored.
DIR_RESULTS = Path("data/results")
RESULTS_CSV_NAME = "partial_dependence.csv"

# English feature titles (kept here since they are stored in the output
# table for convenience when plotting)
FEATURE_TITLES = {
    "Ta_C": "Air Temperature (°C)",
    "P_mm": "Precipitation (mm)",
    "wind_ms": "Wind Speed (m/s)",
    "rad_whm2": "Solar Radiation (Wh/m²)",
    "Q": "Discharge (m³/s)",
    "relhum": "Relative Humidity [0-1]",
}

# Offset / factor grids per feature, and the perturbation mode used.
# mode="additive": offset_val in original units (e.g. +2 degrees C)
# mode="multiplicative": offset_val = "a" in x_raw' = x_raw*(1+a)
# mode="multiplicative_clipped": same as multiplicative, but raw value
#                                 clipped to [0, 1] (useful for fractions)
OFFSETS_TA = [-20, -15, -10, -8, -5, -3, -2, -1, -0.9, -0.8, -0.7, -0.6, -0.5,
              0, 0.2, 0.5, 0.7, 0.8, 1, 1.5, 2, 2.5, 3, 5, 8, 10, 15, 20]
FACTORS_RATIO_SCALED = [0.3, 0.5, 0.7, 0.9, 1.0, 1.1, 1.3, 1.5, 2.0, 3.0]
RATIO_SCALED_FEATURES = ["P_mm", "wind_ms", "rad_whm2", "Q"]
FACTORS_RELHUM = [0.7, 0.85, 1.0, 1.15, 1.3]


# ======================================================================
# Functions
# ======================================================================
def load_model_and_data(run_dir: Path, epoch: int, batch_size: int):
    """Load config, model, test datasets, and build the data loader."""
    cfg = load_cfg(run_dir)
    tester = load_model(cfg, run_dir, epoch=epoch)
    model = tester.model

    datasets = load_test_datasets(cfg, tester)
    loader = build_loader(datasets, batch_size)

    return cfg, tester, model, loader


def compute_baseline(loader, model, tester):
    """Run inference and de-normalize predictions (last time step only,
    since predict_last_n=1)."""
    baseline_preds, observations = run_inference(loader, model)
    baseline_preds_last = baseline_preds[:, -1, :]

    wt_mean, wt_sd = get_feature_stats(tester, "wt")
    baseline_denorm = baseline_preds_last.squeeze() * wt_sd + wt_mean  # °C

    return baseline_denorm, wt_mean, wt_sd


def add_offset(batch, feature, offset_val, mode, mean, std):
    """
    Apply an offset to a dynamic feature in a batch.

    mode="additive": offset_val in original units (e.g. +2 degrees C)
                    x_norm' = x_norm + offset_val / std
    mode="multiplicative": offset_val = "a" in x_raw' = x_raw*(1+a)
                    (a is derived internally from the desired factor)
    mode="multiplicative_clipped": same as multiplicative, but raw value
                    clipped to [0, 1] (useful for fractions like relhum)
    """
    batch_copy = clone_batch(batch)
    x_norm = batch_copy["x_d"][feature]  # [n_samples, seq_length, 1]

    if mode == "additive":
        x_new = x_norm + offset_val / std

    elif mode == "multiplicative":
        x_new = x_norm * (1 + offset_val) + mean * offset_val / std

    elif mode == "multiplicative_clipped":
        x_raw = x_norm * std + mean
        x_raw_new = torch.clamp(x_raw * (1 + offset_val), min=0.0, max=1.0)
        x_new = (x_raw_new - mean) / std

    else:
        raise ValueError(f"Unknown mode: {mode}")

    batch_copy["x_d"][feature] = x_new
    return batch_copy


def calc_partial_dependence(feature, loader, model, mean, std, offset_vals,
                             baseline_denorm, target_mean, target_sd, mode="additive"):
    """
    Compute individual conditional expectation (ICE) diffs for a feature
    across a grid of offset values.

    offset_vals: values actually passed to `add_offset` (i.e. for
    "multiplicative" this is already the "a" in (1+a), NOT the factor itself).

    Returns
    -------
    individual_diffs : torch.Tensor [n_samples, n_offsets]
        Delta prediction (°C) per sample per offset.
    """
    diffs_per_offset = []

    for offset_val in offset_vals:
        preds_all = []
        for batch in loader:
            batch_manip = add_offset(batch, feature, offset_val, mode, mean, std)
            with torch.no_grad():
                output = model(batch_manip)
            preds_all.append(output["y_hat"][:, -1, :])

        preds_offset = torch.cat(preds_all, dim=0).squeeze()
        preds_offset_denorm = preds_offset * target_sd + target_mean

        diff = preds_offset_denorm - baseline_denorm
        diffs_per_offset.append(diff)

    individual_diffs = torch.stack(diffs_per_offset, dim=1)  # [n_samples, n_offsets]
    return individual_diffs


def compute_all_features(loader, model, tester, baseline_denorm, target_mean, target_sd):
    """Compute ICE diffs for all configured dynamic features.

    Returns
    -------
    dict[str, dict]
        feature -> {"x_vals": [...], "diffs": Tensor, "unit_label": str, "mode": str}
    """
    results = {}

    # --- Ta_C: additive (interval-scaled, °C has an arbitrary zero point) ---
    diffs_ta = calc_partial_dependence(
        "Ta_C", loader, model, *get_feature_stats(tester, "Ta_C"),
        OFFSETS_TA, baseline_denorm=baseline_denorm,
        target_mean=target_mean, target_sd=target_sd, mode="additive"
    )
    results["Ta_C"] = dict(x_vals=OFFSETS_TA, diffs=diffs_ta,
                            unit_label="Offset [°C]", mode="additive")

    # --- Ratio-scaled features: multiplicative, x-axis = factor ---
    for feature in RATIO_SCALED_FEATURES:
        feature_mean, feature_sd = get_feature_stats(tester, feature)
        a_vals = [f - 1.0 for f in FACTORS_RATIO_SCALED]  # x_raw' = x_raw*(1+a) = x_raw*factor
        diffs = calc_partial_dependence(
            feature, loader, model, feature_mean, feature_sd,
            a_vals, baseline_denorm=baseline_denorm,
            target_mean=target_mean, target_sd=target_sd, mode="multiplicative"
        )
        results[feature] = dict(x_vals=FACTORS_RATIO_SCALED, diffs=diffs,
                                 unit_label="Factor (1.0 = no change)", mode="multiplicative")

    # --- relhum: multiplicative + clipped, x-axis = factor ---
    mean_rh, std_rh = get_feature_stats(tester, "relhum")
    a_vals_rh = [f - 1.0 for f in FACTORS_RELHUM]
    diffs_rh = calc_partial_dependence(
        "relhum", loader, model, mean_rh, std_rh,
        a_vals_rh, baseline_denorm=baseline_denorm,
        target_mean=target_mean, target_sd=target_sd, mode="multiplicative_clipped"
    )
    results["relhum"] = dict(x_vals=FACTORS_RELHUM, diffs=diffs_rh,
                              unit_label="Factor (1.0 = no change)", mode="multiplicative_clipped")

    return results


def results_to_dataframe(results: dict, feature_titles: dict) -> pd.DataFrame:
    """Convert the results dict into a tidy (long-format) DataFrame with
    one row per (feature, x_val, sample). This keeps the individual
    ICE curves available for plotting, not just the PDP mean."""
    rows = []
    for feature, res in results.items():
        matrix = res["diffs"].numpy()  # [n_samples, n_offsets]
        n_samples, n_offsets = matrix.shape

        for offset_idx, x_val in enumerate(res["x_vals"]):
            for sample_idx in range(n_samples):
                rows.append({
                    "feature": feature,
                    "feature_title": feature_titles.get(feature, feature),
                    "unit_label": res["unit_label"],
                    "mode": res["mode"],
                    "x_val": x_val,
                    "sample_id": sample_idx,
                    "diff": matrix[sample_idx, offset_idx],
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
    cfg, tester, model, loader = load_model_and_data(RUN_DIR, EPOCH, BATCH_SIZE)

    baseline_denorm, wt_mean, wt_sd = compute_baseline(loader, model, tester)

    results = compute_all_features(loader, model, tester, baseline_denorm, wt_mean, wt_sd)

    df = results_to_dataframe(results, FEATURE_TITLES)

    out_path = save_results(df, DIR_RESULTS, RESULTS_CSV_NAME)
    print(f"Done. Saved CSV to: {out_path.resolve()}")


if __name__ == "__main__":
    main()