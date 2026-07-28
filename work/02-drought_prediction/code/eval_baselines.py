"""Evaluate persistence, climatology, and linear-trend baselines on the test set.

Computes the same metrics and figures generated for any model checkpoint:
  - 5 spatial heatmaps (RMSE, corr, drought acc, F1, TPR)
  - test_metrics.json with pooled and per-cell-median scalars

All buffers (global_avg, trend_test, mask, rlat, rlon) are derived directly
from the dataset using the configured train/val/test split — no checkpoint needed.

Outputs are written to figures/baselines/{baseline_name}/.

Usage:
    python code/eval_baselines.py
    python code/eval_baselines.py --processed-dir data/processed
"""

import argparse
import json
import logging
import sys
from pathlib import Path

import numpy as np

# triton is Linux/CUDA-compiler-only; this warning is a no-op on our setup.
# Must be set before `import torch` — flop_counter logs it at import time.
logging.getLogger("torch.utils.flop_counter").setLevel(logging.ERROR)

import torch  # noqa: E402
from torch.utils.data import DataLoader  # noqa: E402

ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(Path(__file__).parent))
sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

from dataset import DroughtDataset  # noqa: E402
from utils.metrics import metrics_celled_drought, metrics_celled_regression  # noqa: E402
from utils.paths import processed_data_dir  # noqa: E402
from utils.plotting import make_heatmap  # noqa: E402

# ── Defaults (mirror configs/data/default.yaml) ───────────────────────────────
HISTORY_LENGTH  = 36
LEAD_TIME       = 12
PERIODS_FORWARD = 1
VAL_FROM_YEAR   = 2005
TEST_FROM_YEAR  = 2015
DROUGHT_THR     = -1.5
PROCESSED_DIR   = processed_data_dir()
FIGURES_DIR     = ROOT / "figures" / "baselines"


def _pooled_drought(pred_flat, tgt_flat, thr):
    bt = tgt_flat  <= thr
    bp = pred_flat <= thr
    tp = (bp & bt).float().sum()
    fp = (bp & ~bt).float().sum()
    fn = (~bp & bt).float().sum()
    pr = tp / (tp + fp).clamp(min=1e-8)
    rc = tp / (tp + fn).clamp(min=1e-8)
    return rc, 2 * pr * rc / (pr + rc).clamp(min=1e-8)   # tpr, f1


def _tpr_nanmedian(tpr_arr, mask):
    v = tpr_arr[mask]
    return v[v.isnan().logical_not()].median()


def _roc_nanmedian(roc_arr, mask):
    v = roc_arr[mask]
    return v[v.isnan().logical_not()].median()


def evaluate_baseline(name, preds, targets, mask, rlat, rlon, out_dir, thr=DROUGHT_THR):
    """Compute metrics and write figures for one baseline prediction tensor.

    Args:
        name:    human-readable baseline name (used in figure titles)
        preds:   (T, H, W) float tensor of baseline predictions
        targets: (T, H, W) float tensor of test targets
        mask:    (H, W) bool tensor of valid Alpine cells
        rlat:    1-D numpy array of rotated latitudes
        rlon:    1-D numpy array of rotated longitudes
        out_dir: Path — figures and metrics written here
        thr:     drought SPEI threshold
    """
    out_dir.mkdir(parents=True, exist_ok=True)

    # ── Per-cell regression metrics ───────────────────────────────────────────
    rmse, mae, corr = metrics_celled_regression(targets, preds)

    # ── Per-cell drought metrics ──────────────────────────────────────────────
    acc_d, f1_d, roc_d, tpr_d = metrics_celled_drought(targets, preds, thr, compute_roc=True)
    assert roc_d is not None

    # ── Spatial heatmaps (same 5 as every model) ─────────────────────────────
    kw = dict(mask=mask, rlat=rlat, rlon=rlon)
    make_heatmap(rmse.cpu(), path=out_dir / "test_rmse_spatial.png",
                 title=f"{name} — Test RMSE", cmap="Reds", **kw)
    make_heatmap(corr.cpu(), path=out_dir / "test_corr_spatial.png",
                 title=f"{name} — Test correlation",
                 cmap="RdYlBu_r", vmin=-1.0, vmax=1.0, **kw)
    make_heatmap(acc_d.cpu(), path=out_dir / "test_drought_acc_spatial.png",
                 title=f"{name} — Drought accuracy (SPEI≤{thr})",
                 cmap="RdYlGn", vmin=0.0, vmax=1.0, **kw)
    make_heatmap(f1_d.cpu(), path=out_dir / "test_drought_f1_spatial.png",
                 title=f"{name} — Drought F1 (SPEI≤{thr})",
                 cmap="RdYlGn", vmin=0.0, vmax=1.0, **kw)
    make_heatmap(tpr_d.cpu(), path=out_dir / "test_drought_tpr_spatial.png",
                 title=f"{name} — Drought TPR (SPEI≤{thr})",
                 cmap="RdYlGn", vmin=0.0, vmax=1.0, **kw)

    # ── Pooled scalars ────────────────────────────────────────────────────────
    m = mask
    tgt_flat  = targets[:, m].reshape(-1)
    pred_flat = preds[:, m].reshape(-1)

    rmse_pooled = torch.sqrt(((pred_flat - tgt_flat) ** 2).mean())
    mae_pooled  = (pred_flat - tgt_flat).abs().mean()
    _pc = pred_flat - pred_flat.mean()
    _tc = tgt_flat  - tgt_flat.mean()
    corr_pooled = (
        (_pc * _tc).sum()
        / torch.sqrt((_pc**2).sum() * (_tc**2).sum()).clamp(min=1e-8)
    ).clamp(-1.0, 1.0)

    tpr_pooled, f1_pooled = _pooled_drought(pred_flat, tgt_flat, thr)

    T = targets.shape[0]
    n_valid_cells = int(m.sum().item())

    # ── Per-cell medians ──────────────────────────────────────────────────────
    tpr_med = _tpr_nanmedian(tpr_d, m)
    roc_med = _roc_nanmedian(roc_d, m)

    metrics = {
        # Pooled (headline)
        "rmse_pooled":          rmse_pooled.item(),
        "mae_pooled":           mae_pooled.item(),
        "corr_pooled":          corr_pooled.item(),
        "drought_tpr_pooled":   tpr_pooled.item(),
        "drought_f1_pooled":    f1_pooled.item(),
        "valid_cell_months":    T * n_valid_cells,
        # Per-cell medians
        "rmse_median":          torch.median(rmse[m]).item(),
        "mae_median":           torch.median(mae[m]).item(),
        "corr_median":          torch.median(corr[m]).item(),
        "drought_f1_median":    torch.median(f1_d[m]).item(),
        "drought_acc_median":   torch.median(acc_d[m]).item(),
        "drought_rocauc_median": roc_med.item(),
        "drought_tpr_median":   tpr_med.item(),
    }

    with open(out_dir / "test_metrics.json", "w") as f:
        json.dump(metrics, f, indent=2)

    print(f"  {name:20s}  RMSE={rmse_pooled:.4f}  TPR={tpr_pooled:.4f}  "
          f"F1={f1_pooled:.4f}  corr={corr_pooled:.4f}")
    print(f"  {'':20s}  figures + metrics → {out_dir}")
    return metrics


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--processed-dir", default=str(PROCESSED_DIR),
                        help="Path to preprocessed data directory")
    args = parser.parse_args()

    processed_dir = Path(args.processed_dir)

    # ── Load dataset and splits ───────────────────────────────────────────────
    print("Loading dataset ...")
    ds = DroughtDataset(
        processed_dir,
        history_length=HISTORY_LENGTH,
        lead_time=LEAD_TIME,
        periods_forward=PERIODS_FORWARD,
    )
    train_sub, _, test_sub = ds.split_by_years(val_from=VAL_FROM_YEAR, test_from=TEST_FROM_YEAR)

    mask = ds.mask.bool()
    rlat = ds.rlat if isinstance(ds.rlat, np.ndarray) else np.array(ds.rlat)
    rlon = ds.rlon if isinstance(ds.rlon, np.ndarray) else np.array(ds.rlon)

    # ── Compute global_avg from training targets ──────────────────────────────
    train_indices = train_sub.indices
    t_start = train_indices[0]  + HISTORY_LENGTH
    t_end   = train_indices[-1] + HISTORY_LENGTH + 1
    global_avg = ds.spei[t_start:t_end].mean(dim=0)   # (H, W)

    # ── Compute linear trend on training data, evaluated at test times ────────
    _off     = HISTORY_LENGTH + LEAD_TIME - 1
    _train_t = np.array([i + _off for i in train_indices], dtype=np.float32)
    _test_t  = np.array([i + _off for i in test_sub.indices], dtype=np.float32)
    _A_tr    = np.stack([_train_t, np.ones_like(_train_t)], axis=1)
    _Y_tr    = ds.spei[torch.from_numpy(_train_t.astype(np.int64))].reshape(len(_train_t), -1).numpy()
    _C, _, _, _ = np.linalg.lstsq(_A_tr, _Y_tr, rcond=None)
    _A_te    = np.stack([_test_t, np.ones_like(_test_t)], axis=1)
    trend_test = torch.tensor(
        (_A_te @ _C).reshape(len(_test_t), ds.H, ds.W), dtype=torch.float32
    )

    # ── Collect test targets and persistence predictions ──────────────────────
    # x shape: (B, T, n_vars, H, W); SPEI is channel 0, last timestep is x[:, -1, 0].
    print("Collecting test targets and persistence predictions ...")
    all_targets = []
    all_persist = []
    loader = DataLoader(test_sub, batch_size=8, shuffle=False, num_workers=0)
    for batch in loader:
        x, y = batch[:2]
        all_targets.append(y.squeeze(1))   # (B, H, W)
        all_persist.append(x[:, -1, 0])   # (B, H, W) — last timestep, SPEI channel 0

    targets = torch.cat(all_targets)   # (T, H, W)
    persist = torch.cat(all_persist)   # (T, H, W)

    T = targets.shape[0]
    print(f"Split: train<{VAL_FROM_YEAR}, val={VAL_FROM_YEAR}-{TEST_FROM_YEAR-1}, test>={TEST_FROM_YEAR}")
    print(f"Test set: T={T} months, {int(mask.sum())} valid cells\n")

    clim = global_avg.unsqueeze(0).expand(T, -1, -1).float()

    print("=== Baseline evaluation ===")

    evaluate_baseline(
        "Persistence",
        persist, targets, mask, rlat, rlon,
        FIGURES_DIR / "persistence",
    )

    evaluate_baseline(
        "Climatology",
        clim, targets, mask, rlat, rlon,
        FIGURES_DIR / "climatology",
    )

    evaluate_baseline(
        "Linear trend",
        trend_test.float(), targets, mask, rlat, rlon,
        FIGURES_DIR / "trend",
    )

    print("\nDone.")


if __name__ == "__main__":
    main()
