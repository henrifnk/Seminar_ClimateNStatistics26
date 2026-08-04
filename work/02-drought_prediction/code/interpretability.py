"""Eval-time input-ablation harness for the interpretability analyses.

Loads a frozen checkpoint from saved_models/, runs the (already-standardised)
test set with one input feature knocked out at its correct injection site, and
returns pooled metrics comparable to the model's originally logged test
metrics. No training code is touched and no checkpoint is retrained.

Ablation = occlude-to-zero, never shuffle/permute. Every input is z-scored
against training statistics, so 0 is exactly the training climatological mean:
zeroing a feature is a clean, in-distribution "average climate" counterfactual
with no extrapolation. Permuting correlated inputs instead would force the
model off-manifold and inflate importance (Hooker, Mentch & Zhou 2021,
"Unrestricted permutation forces extrapolation", Statistics and Computing).

See CODING_AGENT_PROMPT_INTERPRETABILITY.md for the full brief.
"""

import contextlib
from dataclasses import dataclass
from pathlib import Path

import torch
from dataset import DroughtDataset, MonthAwareSubset, MonthScalarSubset, ScalarSubset
from model import RCNNModule
from torch.utils.data import DataLoader
from utils.paths import processed_data_dir, project_root

# ── Fixed data config (mirrors configs/data/default.yaml) ──────────────────
# No checkpoint's .overrides file touches data.* -- every saved_models/ run
# was trained on this exact split/history/lead, so it is safe to hard-code
# here rather than re-deriving it via Hydra (same approach as eval_baselines.py).
HISTORY_LENGTH = 36
LEAD_TIME = 12
PERIODS_FORWARD = 1
VAL_FROM_YEAR = 2005
TEST_FROM_YEAR = 2015
DROUGHT_THRESHOLD = -1.5
DYNAMIC_VARS = ["wb", "pr", "ps", "tas"]  # + spei, always channel 0

# Observed-SPEI bins for Phase 2 stratified evaluation.
# (label, lo_exclusive, hi_inclusive); the driest bin includes -inf.
SPEI_BINS: list[tuple[str, float, float]] = [
    ("<=-2", float("-inf"), -2.0),
    ("-2..-1.5", -2.0, -1.5),
    ("-1.5..-1", -1.5, -1.0),
    (">-1", -1.0, float("inf")),
]


# ── Checkpoint architecture (read from .overrides, never the filename) ─────

def read_overrides(ckpt_path: Path | str) -> dict[str, str]:
    """Parse a checkpoint's sibling .overrides file into a {key: value} dict."""
    overrides_path = Path(ckpt_path).with_suffix(".overrides")
    if not overrides_path.exists():
        raise FileNotFoundError(
            f"No .overrides file next to {ckpt_path} -- cannot determine architecture. "
            f"Checkpoint filenames are not reliable (see CODING_AGENT_PROMPT_INTERPRETABILITY.md)."
        )
    kv: dict[str, str] = {}
    for line in overrides_path.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        key, _, val = line.partition("=")
        kv[key] = val
    return kv


def architecture_from_overrides(overrides: dict[str, str]) -> tuple[str, str, str]:
    """Returns (static_encoder, global_encoder, med_sst_agg)."""
    static_encoder = overrides.get("model.static_encoder", "none")
    global_encoder = overrides.get("model.global_encoder", "none")
    med_sst_agg = overrides.get("data.med_sst_agg", "grouped")
    return static_encoder, global_encoder, med_sst_agg


# ── Dataset / model construction (mirrors train.py's setup, duplicated here
#    rather than imported so this module never has to touch train.py) ──────

def build_dataset(
    static_encoder: str,
    global_encoder: str,
    med_sst_agg: str = "grouped",
    processed_dir: Path | str | None = None,
) -> DroughtDataset:
    use_naive_static = static_encoder == "naive"
    use_naive_global = global_encoder == "naive"
    use_film = global_encoder == "film"
    return DroughtDataset(
        processed_dir=processed_dir or processed_data_dir(),
        history_length=HISTORY_LENGTH,
        lead_time=LEAD_TIME,
        periods_forward=PERIODS_FORWARD,
        dynamic_vars=list(DYNAMIC_VARS),
        use_global_scalars=use_film or use_naive_global,
        med_sst_agg=med_sst_agg,
        static_injection="naive" if use_naive_static else "none",
        global_injection="naive" if use_naive_global else "none",
    )


def wrap_test_subset(dataset: DroughtDataset, static_encoder: str, global_encoder: str):
    """Test-period Subset, wrapped as train.py._wrap() would wrap it."""
    _, _, test_sub = dataset.split_by_years(val_from=VAL_FROM_YEAR, test_from=TEST_FROM_YEAR)
    seasonal = static_encoder == "seasonal"
    use_film = global_encoder == "film"
    if seasonal and use_film:
        return MonthScalarSubset(test_sub.dataset, test_sub.indices)
    if seasonal:
        return MonthAwareSubset(test_sub.dataset, test_sub.indices)
    if use_film:
        return ScalarSubset(test_sub.dataset, test_sub.indices)
    return test_sub


def load_model(ckpt_path: Path | str, dataset: DroughtDataset, static_encoder: str, global_encoder: str) -> RCNNModule:
    """Load a checkpoint and sanity-check it against the architecture read from
    .overrides.

    `mask` comes back correctly from the checkpoint's own saved buffers, but
    `static_map` does not: RCNNModule.__init__ registers it as a None buffer,
    and torch's state-dict loading silently skips restoring into buffers that
    are still None at load time (the same reason `global_avg`/`trend_test`/
    `rlat`/`rlon` show up as "unexpected keys" on load -- they're saved in the
    checkpoint but never actually assigned back). static_map is a deterministic
    function of the dataset (topography + mask), not a learned quantity, so it
    is simply re-registered here exactly as train.py does after construction.
    """
    ckpt_path = Path(ckpt_path)
    model = RCNNModule.load_from_checkpoint(str(ckpt_path), map_location="cpu")
    if static_encoder in ("single", "seasonal"):
        model.register_buffer("static_map", dataset.static_features)
    model.eval()
    assert model.static_encoder_mode == static_encoder, (
        f"{ckpt_path.name}: checkpoint hparams static_encoder="
        f"{model.static_encoder_mode!r} != .overrides static_encoder={static_encoder!r}"
    )
    assert model.global_encoder_mode == global_encoder, (
        f"{ckpt_path.name}: checkpoint hparams global_encoder="
        f"{model.global_encoder_mode!r} != .overrides global_encoder={global_encoder!r}"
    )
    assert model.hparams.n_extra_channels == dataset.n_naive_channels, (
        f"{ckpt_path.name}: checkpoint n_extra_channels={model.hparams.n_extra_channels} "
        f"!= rebuilt-dataset n_naive_channels={dataset.n_naive_channels}"
    )
    return model


# ── Ablation specs ───────────────────────────────────────────────────────────

@dataclass(frozen=True)
class AblationSpec:
    name: str  # e.g. "spei", "wb", "global", "static"
    kind: str  # "dynamic" | "global" | "static"


def applicable_ablations(static_encoder: str, global_encoder: str, dynamic_var_names: list[str]) -> list[AblationSpec]:
    """The 5 dynamic variables always; global/static groups only where the
    architecture actually uses that pathway.
    """
    specs = [AblationSpec("spei", "dynamic")]
    specs += [AblationSpec(v, "dynamic") for v in dynamic_var_names]
    if global_encoder != "none":
        specs.append(AblationSpec("global", "global"))
    if static_encoder != "none":
        specs.append(AblationSpec("static", "static"))
    return specs


def _dynamic_channel(name: str, dataset: DroughtDataset) -> int:
    if name == "spei":
        return 0
    return 1 + dataset.dynamic_var_names.index(name)


def _naive_static_slice(dataset: DroughtDataset) -> slice | None:
    """x-channel slice of the naive-injected static map, or None if this
    checkpoint's static pathway isn't naive injection.
    """
    if dataset.static_injection != "naive":
        return None
    start = dataset.n_dynamic_vars
    return slice(start, start + dataset.static_features.shape[0])


def _naive_global_slice(dataset: DroughtDataset) -> slice | None:
    """x-channel slice of the naive-injected global scalars, or None if this
    checkpoint's global pathway isn't naive injection. Static channels (if
    naive) come first in x, so this slice starts after them.
    """
    if dataset.global_injection != "naive":
        return None
    start = dataset.n_dynamic_vars
    if dataset.static_injection == "naive":
        start += dataset.static_features.shape[0]
    return slice(start, start + dataset.n_global)


def _scalars_batch_index(static_encoder: str, global_encoder: str) -> int | None:
    """Position of the FiLM scalars tensor in a collated batch tuple, or None
    if this checkpoint doesn't use FiLM. (x, y, [month], [scalars]).
    """
    if global_encoder != "film":
        return None
    return 3 if static_encoder == "seasonal" else 2


def ablate_batch(batch, spec: AblationSpec, dataset: DroughtDataset, static_encoder: str, global_encoder: str):
    """Zero one feature at its correct injection site. Returns a new batch
    tuple; the input batch (and its tensors) are left untouched.
    """
    batch = list(batch)
    if spec.kind == "dynamic":
        x = batch[0].clone()
        x[:, :, _dynamic_channel(spec.name, dataset)] = 0.0
        batch[0] = x
    elif spec.kind == "static":
        sl = _naive_static_slice(dataset)
        if sl is not None:
            x = batch[0].clone()
            x[:, :, sl] = 0.0
            batch[0] = x
        # else: single/seasonal encoder -- ablated via ablate_static_map() around
        # the whole eval pass instead (topography isn't part of a batch).
    elif spec.kind == "global":
        sl = _naive_global_slice(dataset)
        if sl is not None:
            x = batch[0].clone()
            x[:, :, sl] = 0.0
            batch[0] = x
        else:
            idx = _scalars_batch_index(static_encoder, global_encoder)
            if idx is not None:
                batch[idx] = torch.zeros_like(batch[idx])
    else:
        raise ValueError(f"Unknown ablation kind: {spec.kind!r}")
    return tuple(batch)


@contextlib.contextmanager
def ablate_static_map(model: RCNNModule):
    """Temporarily zero the model's static topography buffer (used by the
    single/seasonal static encoders) for the duration of the block, then
    restore the original.
    """
    original = model.static_map
    if original is not None:
        model.static_map = torch.zeros_like(original)
    try:
        yield
    finally:
        model.static_map = original


# ── Eval loop ────────────────────────────────────────────────────────────────

@torch.no_grad()
def run_eval(
    model: RCNNModule,
    loader: DataLoader,
    dataset: DroughtDataset,
    static_encoder: str,
    global_encoder: str,
    spec: AblationSpec | None = None,
    device: str = "cpu",
) -> tuple[torch.Tensor, torch.Tensor]:
    """One deterministic forward pass over `loader`, optionally with `spec`
    ablated. Returns (all_preds, all_targets), each (T_test, H, W).
    """
    model.eval()
    static_ablate = spec is not None and spec.kind == "static" and static_encoder in ("single", "seasonal")
    cm = ablate_static_map(model) if static_ablate else contextlib.nullcontext()

    all_preds, all_targets = [], []
    with cm:
        for batch in loader:
            batch = tuple(t.to(device) for t in batch)
            if spec is not None:
                batch = ablate_batch(batch, spec, dataset, static_encoder, global_encoder)
            _, preds, targets = model.step(batch)
            all_preds.append(preds)
            all_targets.append(targets)
    return torch.cat(all_preds), torch.cat(all_targets)


# ── Metrics ──────────────────────────────────────────────────────────────────

def pooled_metrics(
    targets: torch.Tensor, preds: torch.Tensor, mask: torch.Tensor, threshold: float = DROUGHT_THRESHOLD
) -> dict[str, float]:
    """Pooled RMSE / drought TPR / drought F1 over all valid cell-months.

    Mirrors the pooled-metric formulas in RCNNModule.on_test_epoch_end
    (code/model.py) exactly, so results here are directly comparable to a
    model's originally logged test/rmse_pooled etc.
    """
    tgt_flat = targets[:, mask].reshape(-1)
    pred_flat = preds[:, mask].reshape(-1)
    rmse_pooled = torch.sqrt(((pred_flat - tgt_flat) ** 2).mean())

    bt = tgt_flat <= threshold
    bp = pred_flat <= threshold
    tp = (bp & bt).float().sum()
    fp = (bp & ~bt).float().sum()
    fn = (~bp & bt).float().sum()
    prec = tp / (tp + fp).clamp(min=1e-8)
    rec = tp / (tp + fn).clamp(min=1e-8)
    f1_pooled = 2 * prec * rec / (prec + rec).clamp(min=1e-8)

    return {
        "rmse_pooled": rmse_pooled.item(),
        "drought_tpr_pooled": rec.item(),
        "drought_f1_pooled": f1_pooled.item(),
    }


def stratified_metrics(
    targets: torch.Tensor,
    preds: torch.Tensor,
    mask: torch.Tensor,
    bins: list[tuple[str, float, float]] = SPEI_BINS,
) -> list[dict]:
    """RMSE and mean bias (pred - target) per observed-SPEI bin, pooled across
    all valid cell-months. Used by Phase 2's stratified-error deliverable.
    """
    tgt_flat = targets[:, mask].reshape(-1)
    pred_flat = preds[:, mask].reshape(-1)

    rows = []
    for label, lo, hi in bins:
        sel = (tgt_flat > lo) & (tgt_flat <= hi)
        n = int(sel.sum().item())
        if n == 0:
            rows.append({"bin": label, "n": 0, "rmse": float("nan"), "mean_bias": float("nan")})
            continue
        t_sel = tgt_flat[sel]
        p_sel = pred_flat[sel]
        rmse = torch.sqrt(((p_sel - t_sel) ** 2).mean()).item()
        mean_bias = (p_sel - t_sel).mean().item()
        rows.append({"bin": label, "n": n, "rmse": rmse, "mean_bias": mean_bias})
    return rows


# ── High-level convenience: full ablation sweep for one checkpoint ─────────

def evaluate_checkpoint(
    ckpt_path: Path | str,
    ablations: list[AblationSpec] | None = None,
    device: str = "cpu",
    batch_size: int = 16,
    num_workers: int = 0,
    processed_dir: Path | str | None = None,
) -> dict:
    """Load a checkpoint, run the un-ablated test-set eval plus each requested
    ablation, and return baseline + per-ablation pooled metrics and deltas.

    ablations=None runs every ablation applicable to this checkpoint's
    architecture; pass [] for baseline only.
    """
    ckpt_path = Path(ckpt_path)
    overrides = read_overrides(ckpt_path)
    static_encoder, global_encoder, med_sst_agg = architecture_from_overrides(overrides)

    dataset = build_dataset(static_encoder, global_encoder, med_sst_agg, processed_dir=processed_dir)
    test_subset = wrap_test_subset(dataset, static_encoder, global_encoder)
    loader = DataLoader(test_subset, batch_size=batch_size, shuffle=False, num_workers=num_workers)

    model = load_model(ckpt_path, dataset, static_encoder, global_encoder)
    model.to(device)
    mask = model.mask

    if ablations is None:
        ablations = applicable_ablations(static_encoder, global_encoder, dataset.dynamic_var_names)

    baseline_preds, baseline_targets = run_eval(
        model, loader, dataset, static_encoder, global_encoder, spec=None, device=device
    )
    baseline = pooled_metrics(baseline_targets, baseline_preds, mask)

    results: dict = {
        "checkpoint": ckpt_path.name,
        "architecture": {"static_encoder": static_encoder, "global_encoder": global_encoder},
        "baseline": baseline,
        "ablations": {},
    }
    for spec in ablations:
        preds, targets = run_eval(model, loader, dataset, static_encoder, global_encoder, spec=spec, device=device)
        m = pooled_metrics(targets, preds, mask)
        results["ablations"][spec.name] = {
            **m,
            "delta_rmse": m["rmse_pooled"] - baseline["rmse_pooled"],
            "delta_drought_f1": m["drought_f1_pooled"] - baseline["drought_f1_pooled"],
            "delta_drought_tpr": m["drought_tpr_pooled"] - baseline["drought_tpr_pooled"],
        }
    return results


# ── Phase 1: feature importance across the six Pinball architecture conditions ──
#
# For each of the six best_model_pinball_q0.20_*.ckpt checkpoints, each
# applicable feature is occluded and the metric drop vs. the un-ablated model
# is reported. Reliance is reported separately for point accuracy (RMSE) and
# tail detection (drought F1/TPR), since the two need not be driven by the
# same features.
#
# Framing: these deltas describe *model reliance* (Breiman 2001; Fisher, Rudin
# & Dominici 2019), not feature necessity. Because inputs are correlated (e.g.
# SPEI history and water balance both track moisture deficit), a low delta for
# one feature can mean the model leans on a correlated substitute instead --
# occlusion attributes credit to whichever correlated feature happens to still
# be present, not to the "true" cause (Hooker, Mentch & Zhou 2021).

# Fixed feature -> color mapping, validated as a colorblind-safe categorical
# palette (dataviz skill, adjacent-pair gates: worst CVD dE 9.1, worst
# normal-vision dE 19.6). Order matches applicable_ablations() so a feature
# keeps the same color across every condition's chart.
FEATURE_COLORS = {
    "spei": "#2a78d6",
    "wb": "#eb6834",
    "pr": "#1baf7a",
    "ps": "#eda100",
    "tas": "#e87ba4",
    "global": "#008300",
    "static": "#4a3aa7",
}
FEATURE_ORDER = list(FEATURE_COLORS)


def _style_axes(ax) -> None:
    ax.spines[["top", "right"]].set_visible(False)
    ax.axhline(0, color="#c3c2b7", lw=0.8, zorder=0)
    ax.grid(axis="y", color="#e1e0d9", lw=0.6, zorder=0)
    ax.set_axisbelow(True)
    ax.tick_params(labelsize=8)


def plot_feature_importance(tag: str, rows: list[dict], fig_path: Path) -> None:
    """One figure per architecture condition: dRMSE and Ddrought-F1 as two
    single-axis panels (never a shared/dual y-axis -- the two metrics are on
    different scales), bars colored by feature identity.
    """
    import matplotlib.pyplot as plt

    features = [r["feature"] for r in rows]
    colors = [FEATURE_COLORS[f] for f in features]

    fig, (ax_rmse, ax_f1) = plt.subplots(1, 2, figsize=(max(5.0, 0.9 * len(features)), 3.0))

    ax_rmse.bar(features, [r["delta_rmse"] for r in rows], color=colors, width=0.6)
    ax_rmse.set_ylabel(r"$\Delta$RMSE  (ablated $-$ full)", fontsize=9)
    ax_rmse.set_title("Point accuracy", fontsize=9)

    ax_f1.bar(features, [r["delta_drought_f1"] for r in rows], color=colors, width=0.6)
    ax_f1.set_ylabel(r"$\Delta$drought F1  (ablated $-$ full)", fontsize=9)
    ax_f1.set_title("Drought detection (SPEI$\\leq$-1.5)", fontsize=9)

    for ax in (ax_rmse, ax_f1):
        _style_axes(ax)
        ax.tick_params(axis="x", rotation=30)

    fig.suptitle(f"Occlusion feature importance — {tag}", fontsize=10)
    fig.tight_layout()
    fig_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(fig_path, bbox_inches="tight")
    plt.close(fig)


def run_feature_importance(
    saved_models_dir: Path | None = None,
    csv_path: Path | None = None,
    fig_dir: Path | None = None,
) -> list[dict]:
    """Phase 1 entry point: run occlusion feature importance over all six
    best_model_pinball_q0.20_*.ckpt checkpoints, write the tidy CSV, and save
    one two-panel bar chart per condition. Prints progress per checkpoint and
    per feature as it runs. Returns the CSV rows.
    """
    import csv as csv_mod

    saved_models_dir = saved_models_dir or (project_root() / "saved_models")
    csv_path = csv_path or (project_root() / "reports" / "interpretability" / "feature_importance_pinball.csv")
    fig_dir = fig_dir or (project_root() / "figures" / "interpretability" / "feature_importance")

    ckpts = sorted(saved_models_dir.glob("best_model_pinball_q0.20*.ckpt"))
    assert len(ckpts) == 6, f"Expected the 6 Pinball architecture conditions, found {len(ckpts)}: {ckpts}"

    all_rows: list[dict] = []
    fig_dir.mkdir(parents=True, exist_ok=True)
    csv_path.parent.mkdir(parents=True, exist_ok=True)

    for i, ckpt in enumerate(ckpts, 1):
        print(f"[{i}/{len(ckpts)}] {ckpt.name}")
        result = evaluate_checkpoint(ckpt)
        static_encoder = result["architecture"]["static_encoder"]
        global_encoder = result["architecture"]["global_encoder"]
        tag = f"{static_encoder}_{global_encoder}"
        baseline = result["baseline"]
        print(
            f"    baseline  rmse_pooled={baseline['rmse_pooled']:.4f}  "
            f"drought_f1_pooled={baseline['drought_f1_pooled']:.4f}  "
            f"drought_tpr_pooled={baseline['drought_tpr_pooled']:.4f}"
        )

        rows_for_plot: list[dict] = []
        for feature in FEATURE_ORDER:
            if feature not in result["ablations"]:
                continue
            m = result["ablations"][feature]
            row = {
                "checkpoint": ckpt.name,
                "static_encoder": static_encoder,
                "global_encoder": global_encoder,
                "feature": feature,
                "baseline_rmse_pooled": baseline["rmse_pooled"],
                "baseline_drought_f1_pooled": baseline["drought_f1_pooled"],
                "baseline_drought_tpr_pooled": baseline["drought_tpr_pooled"],
                "ablated_rmse_pooled": m["rmse_pooled"],
                "ablated_drought_f1_pooled": m["drought_f1_pooled"],
                "ablated_drought_tpr_pooled": m["drought_tpr_pooled"],
                "delta_rmse": m["delta_rmse"],
                "delta_drought_f1": m["delta_drought_f1"],
                "delta_drought_tpr": m["delta_drought_tpr"],
            }
            all_rows.append(row)
            rows_for_plot.append(row)
            print(f"    {feature:8s}  dRMSE={m['delta_rmse']:+.4f}  dF1={m['delta_drought_f1']:+.4f}  "
                  f"dTPR={m['delta_drought_tpr']:+.4f}")

        plot_feature_importance(tag, rows_for_plot, fig_dir / f"{tag}.svg")
        print(f"    figure -> {fig_dir / f'{tag}.svg'}")

    fieldnames = list(all_rows[0].keys())
    with open(csv_path, "w", newline="") as f:
        writer = csv_mod.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(all_rows)
    print(f"\n{len(all_rows)} rows -> {csv_path}")
    print(f"{len(ckpts)} figures -> {fig_dir}/")
    return all_rows


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["feature-importance"])
    args = parser.parse_args()

    if args.command == "feature-importance":
        run_feature_importance()
