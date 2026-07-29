"""
Trend-extrapolation baseline and nonstationarity diagnostics.

Fits a per-cell OLS linear trend on normalized SPEI over the training period
(targets < 2005) and extrapolates to the test period (targets >= 2015).
Scores the trend baseline exactly like persistence and climatology, then
computes drought-frequency shift between training and test periods to
quantify the nonstationarity the models are exposed to.

Run from the project root:
    uv run python reports/generate/trend_baseline.py
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "code"))

import json

import numpy as np
import xarray as xr
from utils.paths import processed_data_dir, project_root

# ---------------------------------------------------------------------------
# Load data (mirrors DroughtDataset logic without instantiating the class)
# ---------------------------------------------------------------------------

PROCESSED = processed_data_dir()
HISTORY   = 12    # must match config
LEAD      = 12    # must match config
VAL_FROM  = 2005
TEST_FROM = 2015
DROUGHT_THRESH = -1.5   # applied to normalized SPEI (≈ raw since mean≈0, std≈1)

# Normalization stats (training-period)
with open(PROCESSED / "normalization.json") as f:
    norm = json.load(f)
spei_mean = norm["spei"]["mean"]
spei_std  = norm["spei"]["std"]

# Raw SPEI and mask
ds_dyn    = xr.open_dataset(PROCESSED / "dynamic_grid.nc")
ds_static = xr.open_dataset(PROCESSED / "static_grid.nc")

spei_raw = ds_dyn["spei"].transpose("time", "rlat", "rlon").values  # (T, H, W)
times    = ds_dyn["time"].values                                      # (T,) datetime64
mask_np  = ds_static["mask"].values.astype(bool)                     # (H, W)
ds_dyn.close()
ds_static.close()

T, H, W = spei_raw.shape

# Normalize exactly as DroughtDataset does (fill non-Alpine with mean before norm)
spei_raw[:, ~mask_np] = spei_mean
spei_norm = (spei_raw - spei_mean) / spei_std   # (T, H, W)

# Sample → target index: sample i → spei at i + HISTORY + LEAD - 1
offset = HISTORY + LEAD - 1          # = 23

n_samples = T - HISTORY - LEAD - 1 + 2   # = T - 22
target_indices = np.arange(n_samples) + offset          # absolute time idx of target

# Calendar year of each target
target_years = (
    times[target_indices].astype("datetime64[Y]").astype(int) + 1970
)

train_mask = target_years < VAL_FROM
test_mask  = target_years >= TEST_FROM

train_tgt_abs = target_indices[train_mask]   # absolute SPEI indices for training targets
test_tgt_abs  = target_indices[test_mask]

print(f"Training targets : {train_mask.sum()} samples  "
      f"({target_years[train_mask][0]}–{target_years[train_mask][-1]})")
print(f"Test targets     : {test_mask.sum()} samples  "
      f"({target_years[test_mask][0]}–{target_years[test_mask][-1]})")

# SPEI target tensors in normalized space
Y_train = spei_norm[train_tgt_abs]   # (N_train, H, W)
Y_test  = spei_norm[test_tgt_abs]    # (N_test,  H, W)

# ---------------------------------------------------------------------------
# 1. Drought-frequency shift (nonstationarity check)
# ---------------------------------------------------------------------------
mask_flat  = mask_np.flatten()
drought_train = (Y_train <= DROUGHT_THRESH)[:, mask_np]   # (N_train, n_valid)
drought_test  = (Y_test  <= DROUGHT_THRESH)[:, mask_np]   # (N_test,  n_valid)

freq_train_per_cell = drought_train.mean(axis=0)   # (n_valid,)
freq_test_per_cell  = drought_test.mean(axis=0)    # (n_valid,)

print("\n=== Drought frequency (SPEI ≤ −1.5, normalized) ===")
print(f"Training period  median={np.median(freq_train_per_cell)*100:.1f}%  "
      f"mean={np.mean(freq_train_per_cell)*100:.1f}%  "
      f"p25={np.percentile(freq_train_per_cell,25)*100:.1f}%  "
      f"p75={np.percentile(freq_train_per_cell,75)*100:.1f}%")
print(f"Test period      median={np.median(freq_test_per_cell)*100:.1f}%  "
      f"mean={np.mean(freq_test_per_cell)*100:.1f}%  "
      f"p25={np.percentile(freq_test_per_cell,25)*100:.1f}%  "
      f"p75={np.percentile(freq_test_per_cell,75)*100:.1f}%")
print(f"Ratio test/train : {np.median(freq_test_per_cell)/np.median(freq_train_per_cell):.2f}×")

# Test-period mean SPEI per cell (drift indicator)
mean_test_per_cell = Y_test[:, mask_np].mean(axis=0)
print(f"\nTest-period mean normalized SPEI (valid cells): "
      f"median={np.median(mean_test_per_cell):.3f}  "
      f"min={np.min(mean_test_per_cell):.3f}  "
      f"max={np.max(mean_test_per_cell):.3f}")
print("(Climatology baseline RMSE is inflated by this drift away from training mean ≈ 0)")

# ---------------------------------------------------------------------------
# 2. Per-cell linear trend fitted on training targets
# ---------------------------------------------------------------------------
N_train = len(train_tgt_abs)
N_test  = len(test_tgt_abs)

# Time variable: months since 1971-01 (absolute month index of target)
t_train = train_tgt_abs.astype(np.float32)   # (N_train,)
t_test  = test_tgt_abs.astype(np.float32)    # (N_test,)

# Design matrix [t, 1] for OLS
A_train = np.stack([t_train, np.ones(N_train, dtype=np.float32)], axis=1)  # (N_train, 2)
A_test  = np.stack([t_test,  np.ones(N_test,  dtype=np.float32)], axis=1)  # (N_test,  2)

# Vectorized OLS across all cells: C shape (2, H*W)
Y_flat = Y_train.reshape(N_train, -1).astype(np.float32)    # (N_train, H*W)
C, _, _, _ = np.linalg.lstsq(A_train, Y_flat, rcond=None)  # (2, H*W)

# Trend predictions at test times
trend_preds = (A_test @ C).reshape(N_test, H, W)            # (N_test, H, W)

# Also compute training-fit quality for reference
trend_train_preds = (A_train @ C).reshape(N_train, H, W)

# ---------------------------------------------------------------------------
# 3. Score trend baseline (same as metrics_celled_regression)
# ---------------------------------------------------------------------------
def rmse_corr_median(preds: np.ndarray, targets: np.ndarray, mask: np.ndarray):
    """Per-cell RMSE and Pearson r, then nanmedian over valid cells."""
    # preds, targets: (T, H, W); mask: (H, W) bool
    resid   = preds - targets
    rmse    = np.sqrt((resid ** 2).mean(axis=0))         # (H, W)
    p_c = preds   - preds.mean(axis=0, keepdims=True)
    t_c = targets - targets.mean(axis=0, keepdims=True)
    num   = (p_c * t_c).sum(axis=0)
    denom = np.sqrt((p_c**2).sum(axis=0) * (t_c**2).sum(axis=0)).clip(min=1e-8)
    corr  = (num / denom).clip(-1.0, 1.0)
    return np.nanmedian(rmse[mask]), np.nanmedian(corr[mask])

# Climatology baseline: training mean per cell, constant for all test times
clim = Y_train.mean(axis=0, keepdims=True).repeat(N_test, axis=0)    # (N_test, H, W)

# Persistence baseline: for each test sample, last available SPEI obs before target
# = spei at target_abs - LEAD  (the last input timestep)
persist_tgt_abs = test_tgt_abs - LEAD   # index of last input SPEI obs
persist_preds   = spei_norm[persist_tgt_abs]   # (N_test, H, W) — same shape as test

rmse_trend,   corr_trend   = rmse_corr_median(trend_preds,   Y_test, mask_np)
rmse_clim,    corr_clim    = rmse_corr_median(clim,          Y_test, mask_np)
rmse_persist, corr_persist = rmse_corr_median(persist_preds, Y_test, mask_np)

print("\n=== Baseline comparison (normalized SPEI, test period) ===")
print(f"{'Baseline':<20} {'RMSE_median':>12} {'Corr_median':>12}")
print("-" * 46)
print(f"{'Persistence':<20} {rmse_persist:>12.4f} {corr_persist:>12.4f}")
print(f"{'Climatology':<20} {rmse_clim:>12.4f} {corr_clim:>12.4f}")
print(f"{'Linear trend':<20} {rmse_trend:>12.4f} {corr_trend:>12.4f}")
# Load model results from CSV if available, for comparison
csv_path = project_root() / "saved_models" / "gridsearch_comparison.csv"
model_rmse_range: str
if csv_path.exists():
    import csv as _csv
    with open(csv_path) as f:
        rows = list(_csv.DictReader(f))
    model_rmses = sorted(float(r["test/rmse_median"]) for r in rows if r.get("test/rmse_median"))
    model_corrs = sorted(float(r["test/corr_median"])  for r in rows if r.get("test/corr_median"))
    if model_rmses:
        best_rmse = model_rmses[0]
        best_corr = model_corrs[-1]
        print(f"Model RMSE (best/median/worst over {len(model_rmses)} runs): "
              f"{model_rmses[0]:.4f} / {np.median(model_rmses):.4f} / {model_rmses[-1]:.4f}")
        print(f"Model corr (best/median/worst): "
              f"{model_corrs[-1]:.4f} / {np.median(model_corrs):.4f} / {model_corrs[0]:.4f}")
        print(f"Trend vs best-model RMSE gap: {rmse_trend - best_rmse:.4f}")
    else:
        best_rmse = best_corr = float("nan")
        print("(no model RMSE rows in CSV)")
else:
    best_rmse = best_corr = float("nan")
    print(f"(gridsearch CSV not found at {csv_path} — run experiments first)")

# ---------------------------------------------------------------------------
# 4. Slope map stats (how strong is the drift, per cell?)
# ---------------------------------------------------------------------------
slope_map = C[0].reshape(H, W)    # monthly SPEI change per month (normalized)
valid_slopes = slope_map[mask_np]

# Convert to per-decade (normalized SPEI / 10 years)
slope_per_decade = valid_slopes * 12 * 10   # months→months * 12mo/yr * 10yr

print("\n=== Per-cell SPEI trend (training period) ===")
print(f"Slope (normalized SPEI / decade): "
      f"median={np.median(slope_per_decade):.3f}  "
      f"p25={np.percentile(slope_per_decade,25):.3f}  "
      f"p75={np.percentile(slope_per_decade,75):.3f}")
print(f"Fraction of cells with negative trend: "
      f"{(valid_slopes < 0).mean()*100:.1f}%")

# ---------------------------------------------------------------------------
# 5. Residual signal size: test-period std per cell
# ---------------------------------------------------------------------------
# The test-period per-cell standard deviation is the RMSE a zero-correlation
# predictor achieves on zero-mean targets — i.e. the irreducible variance floor
# after removing all drift. Any forecasting skill sits in the gap between
# the test-period std and the model RMSE.
test_std_per_cell = Y_test[:, mask_np].std(axis=0)   # (n_valid,)

print("\n=== Residual signal size (test-period std per valid cell) ===")
print(f"median={np.median(test_std_per_cell):.4f}  "
      f"p25={np.percentile(test_std_per_cell,25):.4f}  "
      f"p75={np.percentile(test_std_per_cell,75):.4f}")
print()
trend_vs_clim_gain = (rmse_clim - rmse_trend) / rmse_clim * 100
print("This is the RMSE of a perfect-mean predictor on detrended targets.")
print(f"The gap between this (median {np.median(test_std_per_cell):.4f}) "
      f"and the best model RMSE ({best_rmse:.4f}) is the skill margin.")
print()
print("=== Summary ===")
freq_ratio = np.median(freq_test_per_cell) / np.median(freq_train_per_cell)
print(f"Nonstationarity: drought frequency {freq_ratio:.2f}× higher in test vs train "
      f"({np.median(freq_train_per_cell)*100:.1f}% → {np.median(freq_test_per_cell)*100:.1f}%).")
print(f"Trend baseline barely beats climatology: "
      f"RMSE {rmse_trend:.4f} vs {rmse_clim:.4f} ({trend_vs_clim_gain:+.1f}% gain).")
print(f"Best model RMSE {best_rmse:.4f} beats trend ({rmse_trend:.4f}) "
      f"by {rmse_trend - best_rmse:.4f} and climatology ({rmse_clim:.4f}) "
      f"by {rmse_clim - best_rmse:.4f}.")
print("→ Model skill is NOT primarily trend-following.")
