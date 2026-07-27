"""Verification checks for drought_rmse_pooled.

Run from the project root:
    python code/verify_metrics.py
"""

import sys
from pathlib import Path

import numpy as np
import torch

sys.path.insert(0, str(Path(__file__).parent))
from utils.metrics import drought_rmse_pooled

THR = -1.5


def _np_oracle(tgt_np, pred_np, mask_np, thr=THR):
    """Ground-truth reference implemented in numpy — used only as a test oracle."""
    tgt_v  = tgt_np[:, mask_np]          # (T, n_valid)
    pred_v = pred_np[:, mask_np]
    dm     = tgt_v <= thr
    if dm.sum() == 0:
        return float("nan")
    return float(np.sqrt(((pred_v[dm] - tgt_v[dm]) ** 2).sum() / dm.sum()))


def _check(label, got, expected, tol=1e-5):
    if np.isnan(expected):
        ok = torch.isnan(got).item()
        sym = "NaN ✓" if ok else f"FAIL — expected NaN, got {got.item():.6f}"
    else:
        ok = abs(got.item() - expected) < tol
        sym = f"{got.item():.6f} ✓" if ok else f"FAIL — got {got.item():.6f}, expected {expected:.6f}"
    print(f"  {label:<50s} {sym}")
    if not ok:
        sys.exit(1)


def main():
    print("=" * 65)
    print("VERIFY drought_rmse_pooled")
    print("=" * 65)

    # ── Synthetic data: T=2, H=2, W=2 ────────────────────────────────────
    # Drought mask (target <= -1.5):
    #   sample 0: cell(0,0)=-3.0 ✓  cell(0,1)=0.0 ✗  cell(1,0)=-1.5 ✓  cell(1,1)=-2.0 ✓
    #   sample 1: all targets ≥ 0  → no drought cells
    tgt = torch.tensor([
        [[-3.0,  0.0],
         [-1.5, -2.0]],
        [[ 0.0,  0.5],
         [ 0.2,  0.3]],
    ])  # (2, 2, 2)

    pred = torch.tensor([
        [[-1.0,  0.1],
         [-1.5, -1.0]],   # errors at drought cells: 2.0, 0.0, 1.0  → se: 4.0, 0.0, 1.0
        [[ 0.0,  0.4],
         [ 0.3, -0.4]],
    ])  # (2, 2, 2)

    full_mask = torch.ones(2, 2, dtype=torch.bool)

    # Drought cells (full mask):
    #   (s=0, h=0, w=0): tgt=-3.0 pred=-1.0 → se=4.0
    #   (s=0, h=1, w=0): tgt=-1.5 pred=-1.5 → se=0.0
    #   (s=0, h=1, w=1): tgt=-2.0 pred=-1.0 → se=1.0
    #   sample 1: zero drought cells
    # n_drought=3, sum_se=5.0, drought_rmse=sqrt(5/3)
    expected_full = _np_oracle(tgt.numpy(), pred.numpy(), full_mask.numpy())
    expected_full_hand = (5.0 / 3) ** 0.5   # ≈ 1.29099

    print(f"\nCase 1 — full mask, 3 drought cells from sample 0, 0 from sample 1")
    print(f"  hand-computed: sqrt(5/3) = {expected_full_hand:.6f}")
    print(f"  numpy oracle : {expected_full:.6f}")
    _check("drought_rmse_pooled (full mask)", drought_rmse_pooled(tgt, pred, full_mask), expected_full)

    # ── Case 2: mask excludes cell (0,0) ─────────────────────────────────
    # Only valid cells: (0,1), (1,0), (1,1)
    # Drought cells within valid set:
    #   (s=0, h=1, w=0): tgt=-1.5 pred=-1.5 → se=0.0
    #   (s=0, h=1, w=1): tgt=-2.0 pred=-1.0 → se=1.0
    # n_drought=2, sum_se=1.0, drought_rmse=sqrt(0.5)
    restricted_mask = torch.tensor([[False, True], [True, True]])
    expected_restricted = _np_oracle(tgt.numpy(), pred.numpy(), restricted_mask.numpy())

    print(f"\nCase 2 — mask excludes cell (0,0); drought cells reduce to 2")
    print(f"  hand-computed: sqrt(1/2) = {0.5**0.5:.6f}")
    print(f"  numpy oracle : {expected_restricted:.6f}")
    _check("drought_rmse_pooled (restricted mask)", drought_rmse_pooled(tgt, pred, restricted_mask), expected_restricted)

    # ── Case 3: sample with no drought cells contributes 0, not NaN ──────
    # Already covered above (sample 1 has no drought), but verify explicitly
    # by using only sample 1 as the full dataset
    tgt_nodrt  = tgt[1:2]    # (1, 2, 2) — no drought cells
    pred_nodrt = pred[1:2]
    result_nodrt = drought_rmse_pooled(tgt_nodrt, pred_nodrt, full_mask)

    print(f"\nCase 3 — entire evaluation set has no drought cells → NaN sentinel")
    _check("drought_rmse_pooled (no drought → NaN)", result_nodrt, float("nan"))

    # ── Case 4: perfect prediction on drought cells ───────────────────────
    pred_perfect = pred.clone()
    pred_perfect[0, 0, 0] = -3.0   # perfect at (s=0,h=0,w=0)
    pred_perfect[0, 1, 0] = -1.5   # already -1.5 but be explicit
    pred_perfect[0, 1, 1] = -2.0   # perfect at (s=0,h=1,w=1)
    result_perfect = drought_rmse_pooled(tgt, pred_perfect, full_mask)

    print(f"\nCase 4 — perfect prediction on all drought cells → 0.0")
    _check("drought_rmse_pooled (perfect pred)", result_perfect, 0.0)

    # ── Device check: all computation stays on input device ───────────────
    if torch.backends.mps.is_available():
        dev = torch.device("mps")
        r = drought_rmse_pooled(tgt.to(dev), pred.to(dev), full_mask.to(dev))
        assert r.device.type == "mps", f"Expected mps, got {r.device}"
        print(f"\nCase 5 — MPS device: result on {r.device} ✓")
    else:
        print(f"\nCase 5 — MPS not available, skipping device check")

    print(f"\n{'=' * 65}")
    print(f"All checks passed.")
    print(f"  sqrt(5/3) = {expected_full_hand:.6f}  (full-mask expected value)")
    print(f"  sqrt(1/2) = {0.5**0.5:.6f}  (restricted-mask expected value)")
    print(f"{'=' * 65}")


if __name__ == "__main__":
    main()
