"""
Compute a two-dimensional Partial Dependence (PDP) surface for the joint
effect of two dynamic features (X_j, X_k) on the target, for a trained
water_temp_lstm model, and save the result as a CSV table.

Both features are manipulated simultaneously - each according to its own
scale type (additive for interval-scaled features like Ta_C, or
multiplicative for ratio-scaled features) - while all other features are
left unchanged. Differences are averaged over all n observations in the
test set.
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

# The two (most important) features whose joint effect is examined.
FEATURE_J = "Ta_C"
FEATURE_K = "rad_whm2"

# Manipulation grid + scale type per feature (same convention as the 1D
# PDP script): mode="additive" for interval-scaled features (offset in
# original units, e.g. +2 degrees C), mode="multiplicative" for
# ratio-scaled features (offset_val = "a" in x_raw' = x_raw*(1+a)),
# mode="multiplicative_clipped" for ratio-scaled features bounded to
# [0, 1] (e.g. relative humidity).
OFFSETS_TA = [-10, -8, -5, -3, -2, -1, 0, 1, 2, 3, 5, 8, 10]
MODE_TA = "additive"

FACTORS_RAD = [0.3, 0.5, 0.7, 0.9, 1.0, 1.1, 1.3, 1.5, 2.0]
MODE_RAD = "multiplicative"

# Directory where the resulting CSV is stored.
DIR_RESULTS = Path("data/results")
RESULTS_CSV_NAME = "partial_dependence_2d.csv"

# English feature titles (stored in the output table for convenience
# when plotting)
FEATURE_TITLES = {
    "Ta_C": "Air Temperature (deg C)",
    "P_mm": "Precipitation (mm)",
    "wind_ms": "Wind Speed (m/s)",
    "rad_whm2": "Solar Radiation (Wh/m2)",
    "Q": "Discharge (m3/s)",
    "relhum": "Relative Humidity [0-1]",
}


# ======================================================================
# Functions
# ======================================================================
def load_model_and_data(run_dir, epoch, batch_size):
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
    baseline_denorm = baseline_preds_last.squeeze() * wt_sd + wt_mean  # deg C

    return baseline_denorm, wt_mean, wt_sd


def apply_offset(x_norm, offset_val, mode, mean, std):
    """
    Apply an offset to a single (already-selected) normalized feature
    tensor. Same convention as the 1D PDP script:

    mode="additive": offset_val in original units (e.g. +2 degrees C)
                    x_norm' = x_norm + offset_val / std
    mode="multiplicative": offset_val = "a" in x_raw' = x_raw*(1+a)
                    (a is derived externally from the desired factor)
    mode="multiplicative_clipped": same as multiplicative, but raw value
                    clipped to [0, 1] (useful for fractions like relhum)
    """
    if mode == "additive":
        return x_norm + offset_val / std

    elif mode == "multiplicative":
        return x_norm * (1 + offset_val) + mean * offset_val / std

    elif mode == "multiplicative_clipped":
        x_raw = x_norm * std + mean
        x_raw_new = torch.clamp(x_raw * (1 + offset_val), min=0.0, max=1.0)
        return (x_raw_new - mean) / std

    else:
        raise ValueError(f"Unknown mode: {mode}")


def add_offset_2d(batch, feature_j, offset_j, mode_j, mean_j, std_j,
                   feature_k, offset_k, mode_k, mean_k, std_k):
    """Apply offsets to two dynamic features simultaneously in a batch,
    leaving all other features unchanged."""
    batch_copy = clone_batch(batch)

    x_j_norm = batch_copy["x_d"][feature_j]
    x_k_norm = batch_copy["x_d"][feature_k]

    batch_copy["x_d"][feature_j] = apply_offset(x_j_norm, offset_j, mode_j, mean_j, std_j)
    batch_copy["x_d"][feature_k] = apply_offset(x_k_norm, offset_k, mode_k, mean_k, std_k)

    return batch_copy


def calc_partial_dependence_2d(feature_j, feature_k, loader, model,
                                mean_j, std_j, offset_vals_j, mode_j,
                                mean_k, std_k, offset_vals_k, mode_k,
                                baseline_denorm, target_mean, target_sd):
    """
    Compute the 2D PDP surface (mean prediction difference) for the joint
    manipulation of two dynamic features across a grid of offset value
    combinations.

    Returns
    -------
    np.ndarray [n_offsets_j, n_offsets_k]
        Mean prediction difference (deg C), averaged over all samples,
        for each (offset_j, offset_k) combination.
    """
    n_j = len(offset_vals_j)
    n_k = len(offset_vals_k)
    pdp_grid = np.full((n_j, n_k), np.nan)

    for i, offset_j in enumerate(offset_vals_j):
        for j, offset_k in enumerate(offset_vals_k):
            preds_all = []
            for batch in loader:
                batch_manip = add_offset_2d(
                    batch, feature_j, offset_j, mode_j, mean_j, std_j,
                    feature_k, offset_k, mode_k, mean_k, std_k,
                )
                with torch.no_grad():
                    output = model(batch_manip)
                preds_all.append(output["y_hat"][:, -1, :])

            preds_offset = torch.cat(preds_all, dim=0).squeeze()
            preds_offset_denorm = preds_offset * target_sd + target_mean

            diff = preds_offset_denorm - baseline_denorm
            # Use nanmean, not mean: a single NaN sample (e.g. from a NaN
            # gap in another feature within the window) would otherwise
            # propagate and turn the *entire* grid cell into NaN.
            n_valid = (~torch.isnan(diff)).sum().item()
            if n_valid == 0:
                pdp_grid[i, j] = np.nan
            else:
                pdp_grid[i, j] = torch.nanmean(diff).item()

    return pdp_grid


def pdp_grid_to_dataframe(pdp_grid, feature_j, offset_vals_j, feature_k, offset_vals_k,
                           feature_titles):
    """Convert the 2D PDP grid into a tidy (long-format) DataFrame with
    one row per (x_val_j, x_val_k) combination."""
    rows = []
    for i, x_val_j in enumerate(offset_vals_j):
        for j, x_val_k in enumerate(offset_vals_k):
            rows.append({
                "feature_j": feature_j,
                "feature_j_title": feature_titles.get(feature_j, feature_j),
                "x_val_j": x_val_j,
                "feature_k": feature_k,
                "feature_k_title": feature_titles.get(feature_k, feature_k),
                "x_val_k": x_val_k,
                "mean_diff": pdp_grid[i, j],
            })

    return pd.DataFrame(rows)


def save_results(df, out_dir, filename):
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
    n_nan_baseline = torch.isnan(baseline_denorm).sum().item()
    if n_nan_baseline > 0:
        print(f"Note: {n_nan_baseline} samples have a NaN baseline prediction "
              f"and will be excluded (via nanmean) from every grid cell.")

    mean_j, std_j = get_feature_stats(tester, FEATURE_J)
    mean_k, std_k = get_feature_stats(tester, FEATURE_K)

    pdp_grid = calc_partial_dependence_2d(
        FEATURE_J, FEATURE_K, loader, model,
        mean_j, std_j, OFFSETS_TA, MODE_TA,
        mean_k, std_k, FACTORS_RAD, MODE_RAD,
        baseline_denorm=baseline_denorm, target_mean=wt_mean, target_sd=wt_sd,
    )

    df = pdp_grid_to_dataframe(pdp_grid, FEATURE_J, OFFSETS_TA, FEATURE_K, FACTORS_RAD, FEATURE_TITLES)

    out_path = save_results(df, DIR_RESULTS, RESULTS_CSV_NAME)
    print(f"Done. Saved CSV to: {out_path.resolve()}")


if __name__ == "__main__":
    main()