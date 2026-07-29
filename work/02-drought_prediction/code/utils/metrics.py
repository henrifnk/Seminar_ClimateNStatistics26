import torch


def _auroc_cells(score: torch.Tensor, labels: torch.Tensor) -> torch.Tensor:
    """Vectorized per-cell AUROC via the Wilcoxon rank-sum formula.

    Equivalent to the standard trapezoidal AUC but computed for all spatial
    cells simultaneously with no Python loops.

    Args:
        score:  (T, H, W) — higher value = higher drought risk (pass -preds)
        labels: (T, H, W) bool — True = drought event

    Returns:
        (H, W) float in [0, 1]. Cells with no positives or all positives → 0.
    """
    T, H, W = score.shape
    s = score.reshape(T, -1)            # (T, C)
    lb = labels.reshape(T, -1).float()  # (T, C)
    n_pos = lb.sum(0)                   # (C,)
    n_neg = T - n_pos
    # Double argsort gives 1-indexed ascending ranks per cell
    ranks = torch.argsort(torch.argsort(s, dim=0), dim=0).float() + 1
    rank_sum = (ranks * lb).sum(0)
    auroc = (rank_sum - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg).clamp(min=1)
    valid = (n_pos > 0) & (n_pos < T)
    nan = torch.full_like(auroc, float("nan"))
    return torch.where(valid, auroc.clamp(0.0, 1.0), nan).reshape(H, W)


def metrics_celled_regression(all_targets, all_preds):
    """Per-cell regression metrics. Inputs: (T, H, W) float tensors."""
    rmse_table = torch.sqrt(torch.mean((all_preds - all_targets) ** 2, dim=0))
    mae_table = torch.mean(torch.abs(all_preds - all_targets), dim=0)

    preds_c = all_preds - all_preds.mean(dim=0, keepdim=True)
    tgts_c = all_targets - all_targets.mean(dim=0, keepdim=True)
    corr_num = (preds_c * tgts_c).sum(dim=0)
    corr_denom = torch.sqrt(
        (preds_c**2).sum(dim=0) * (tgts_c**2).sum(dim=0)
    ).clamp(min=1e-8)
    corr_table = (corr_num / corr_denom).clamp(-1.0, 1.0)

    return rmse_table, mae_table, corr_table


def metrics_celled_drought(
    all_targets: torch.Tensor,
    all_preds: torch.Tensor,
    threshold: float = -1.5,
    compute_roc: bool = False,
) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor | None, torch.Tensor]:
    """Per-cell severe-drought metrics derived from regression outputs.

    Binarises both tensors at `threshold` (SPEI <= threshold → drought=1).
    For AUROC, uses -pred as the drought score (more negative pred = higher risk).
    compute_roc=False skips the expensive per-cell loop; use True only at test time.

    Returns (acc, f1, roc_or_None, tpr) where tpr (recall / sensitivity) is NaN
    for cells that have no drought events in the evaluated period.
    """
    bin_t = all_targets <= threshold   # (T, H, W) bool
    bin_p = all_preds   <= threshold

    T = float(all_targets.shape[0])
    tp = (bin_p & bin_t).float().sum(0)
    fp = (bin_p & ~bin_t).float().sum(0)
    fn = (~bin_p & bin_t).float().sum(0)
    tn = (~bin_p & ~bin_t).float().sum(0)

    acc_table = (tp + tn) / T
    prec  = tp / (tp + fp).clamp(min=1e-8)
    rec   = tp / (tp + fn).clamp(min=1e-8)
    f1_table = 2 * prec * rec / (prec + rec).clamp(min=1e-8)

    # Cells with no drought events: F1/acc are undefined → zero; TPR → NaN
    has_pos = bin_t.any(dim=0)
    f1_table  = torch.where(has_pos, f1_table,  torch.zeros_like(f1_table))
    acc_table = torch.where(has_pos, acc_table, torch.zeros_like(acc_table))
    tpr_table = rec.clone()
    tpr_table[~has_pos] = float("nan")

    if not compute_roc:
        return acc_table, f1_table, None, tpr_table

    roc_table = _auroc_cells(-all_preds, bin_t).to(all_targets.device)
    return acc_table, f1_table, roc_table, tpr_table


def drought_rmse_pooled(
    all_targets: torch.Tensor,
    all_preds: torch.Tensor,
    mask: torch.Tensor,
    threshold: float = -1.5,
) -> torch.Tensor:
    """RMSE restricted to ground-truth drought cells, pooled across all valid cells × samples.

    Uses the same threshold and inequality as drought_f1_pooled (target <= threshold)
    so both metrics describe the same event set. Spatial validity mask is applied
    first; drought restriction is on top of it, not instead of it.

    Empty-mask sentinel: returns NaN (not 0.0) when no drought cells exist.
    NaN signals "undefined" — 0.0 would falsely read as perfect skill.

    Args:
        all_targets: (T, H, W) float — ground-truth SPEI
        all_preds:   (T, H, W) float — model predictions
        mask:        (H, W) bool  — valid spatial cells (land/Alpine mask)
        threshold:   SPEI drought threshold; default -1.5 matches drought_f1_pooled

    Returns:
        Scalar tensor on the same device/dtype as all_targets.
    """
    tgt_valid    = all_targets[:, mask]          # (T, n_valid)
    pred_valid   = all_preds[:, mask]            # (T, n_valid)
    drought_mask = tgt_valid <= threshold        # bool (T, n_valid); Python scalar comparison preserves device

    n_drought = drought_mask.sum()
    if n_drought == 0:
        return torch.tensor(float("nan"), device=all_targets.device, dtype=all_targets.dtype)

    tgt_d  = tgt_valid[drought_mask]             # (N_drought,)
    pred_d = pred_valid[drought_mask]            # (N_drought,)
    return torch.sqrt(((pred_d - tgt_d) ** 2).sum() / n_drought.to(all_targets.dtype))
