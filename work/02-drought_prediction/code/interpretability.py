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
import csv as csv_mod
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import torch
from dataset import DroughtDataset, MonthAwareSubset, MonthScalarSubset, ScalarSubset
from model import RCNNModule
from torch.utils.data import DataLoader
from utils.metrics import drought_rmse_pooled
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
    name: str  # e.g. "spei", "wb", "nao", "month", "med_sst", "global", "static"
    kind: str  # "dynamic" | "global" | "static"
    # For global specs: which columns of the global-scalar vector to zero.
    # None = the whole vector (group-level "did conditioning matter" summary /
    # Phase-2 counterfactual). Ignored for dynamic/static specs.
    channels: tuple[int, ...] | None = None


def _global_ablation_specs(med_sst_agg: str, n_global: int) -> list[AblationSpec]:
    """Per-driver global ablations so feature importance can distinguish NAO,
    seasonality, and Mediterranean SST rather than lumping them together.

    Grouped-mode column layout (dataset.py):
        [nao(0), month_sin(1), month_cos(2), western(3), eastern(4), black_sea(5)]
    month_sin/cos jointly encode one thing (seasonality) so they're ablated
    together; the three SST means are ablated together as "med_sst".
    """
    if med_sst_agg == "grouped" and n_global == 6:
        return [
            AblationSpec("nao", "global", (0,)),
            AblationSpec("month", "global", (1, 2)),
            AblationSpec("med_sst", "global", (3, 4, 5)),
        ]
    # Any other layout (e.g. med_sst_agg="none"): fall back to a single
    # whole-vector global ablation rather than guess an unknown column order.
    return [AblationSpec("global", "global")]


def applicable_ablations(
    static_encoder: str,
    global_encoder: str,
    dynamic_var_names: list[str],
    med_sst_agg: str = "grouped",
    n_global: int = 6,
) -> list[AblationSpec]:
    """The 5 dynamic variables always; the global drivers (NAO / month /
    Med SST, individually) and static topography only where the architecture
    actually uses that pathway.
    """
    specs = [AblationSpec("spei", "dynamic")]
    specs += [AblationSpec(v, "dynamic") for v in dynamic_var_names]
    if global_encoder != "none":
        specs += _global_ablation_specs(med_sst_agg, n_global)
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
        # spec.channels selects columns of the global-scalar vector to zero;
        # None = the whole vector.
        sl = _naive_global_slice(dataset)
        if sl is not None:
            x = batch[0].clone()
            if spec.channels is None:
                x[:, :, sl] = 0.0
            else:
                for c in spec.channels:
                    x[:, :, sl.start + c] = 0.0
            batch[0] = x
        else:
            idx = _scalars_batch_index(static_encoder, global_encoder)
            if idx is not None:
                t = batch[idx].clone()
                if spec.channels is None:
                    t.zero_()
                else:
                    t[..., list(spec.channels)] = 0.0
                batch[idx] = t
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
    progress: bool = False,
) -> tuple[torch.Tensor, torch.Tensor]:
    """One deterministic forward pass over `loader`, optionally with `spec`
    ablated. Returns (all_preds, all_targets), each (T_test, H, W).

    progress=True prints a live "batch i/N" counter on one line (each full
    pass over the real test set can take minutes on CPU, so silence here
    reads as a hang -- this is the only per-run feedback available between
    the caller's own before/after status lines).
    """
    model.eval()
    static_ablate = spec is not None and spec.kind == "static" and static_encoder in ("single", "seasonal")
    cm = ablate_static_map(model) if static_ablate else contextlib.nullcontext()

    n_batches = len(loader)
    all_preds, all_targets = [], []
    with cm:
        for i, batch in enumerate(loader, 1):
            if progress:
                print(f"\r      batch {i}/{n_batches}", end="", flush=True)
            batch = tuple(t.to(device) for t in batch)
            if spec is not None:
                batch = ablate_batch(batch, spec, dataset, static_encoder, global_encoder)
            _, preds, targets = model.step(batch)
            all_preds.append(preds)
            all_targets.append(targets)
    if progress:
        print(f"\r      batch {n_batches}/{n_batches} done", flush=True)
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
    progress: bool = False,
) -> dict:
    """Load a checkpoint, run the un-ablated test-set eval plus each requested
    ablation, and return baseline + per-ablation pooled metrics and deltas.

    ablations=None runs every ablation applicable to this checkpoint's
    architecture; pass [] for baseline only.

    progress=True prints a live status line before/after the baseline and
    each ablation (plus a per-batch counter within each pass, via
    run_eval(progress=True)) -- a full pass over the real test set can take
    minutes on CPU, and each checkpoint here does 1 + len(ablations) of them,
    so this is the difference between visible progress and an apparent hang.
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
        ablations = applicable_ablations(
            static_encoder, global_encoder, dataset.dynamic_var_names,
            med_sst_agg=med_sst_agg, n_global=dataset.n_global,
        )

    if progress:
        print(f"    baseline (un-ablated)  [{len(loader)} batches]")
    baseline_preds, baseline_targets = run_eval(
        model, loader, dataset, static_encoder, global_encoder, spec=None, device=device, progress=progress
    )
    baseline = pooled_metrics(baseline_targets, baseline_preds, mask)
    if progress:
        print(
            f"    baseline  rmse_pooled={baseline['rmse_pooled']:.4f}  "
            f"drought_f1_pooled={baseline['drought_f1_pooled']:.4f}  "
            f"drought_tpr_pooled={baseline['drought_tpr_pooled']:.4f}"
        )

    results: dict = {
        "checkpoint": ckpt_path.name,
        "architecture": {"static_encoder": static_encoder, "global_encoder": global_encoder},
        "baseline": baseline,
        "ablations": {},
    }
    for j, spec in enumerate(ablations, 1):
        if progress:
            print(f"    [{j}/{len(ablations)}] ablating {spec.name}  [{len(loader)} batches]")
        preds, targets = run_eval(
            model, loader, dataset, static_encoder, global_encoder, spec=spec, device=device, progress=progress
        )
        m = pooled_metrics(targets, preds, mask)
        if progress:
            print(
                f"    [{j}/{len(ablations)}] {spec.name:8s}  "
                f"dRMSE={m['rmse_pooled'] - baseline['rmse_pooled']:+.4f}  "
                f"dF1={m['drought_f1_pooled'] - baseline['drought_f1_pooled']:+.4f}  "
                f"dTPR={m['drought_tpr_pooled'] - baseline['drought_tpr_pooled']:+.4f}"
            )
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

# Fixed feature -> color mapping: the 5 dynamic vars + static keep the
# documented 8-slot categorical palette (dataviz skill; this subsequence
# validates clean -- worst adjacent CVD dE 8.0, worst normal-vision dE 17.4).
# nao/month/med_sst are sub-components of one parent feature (the global-
# scalar group), not independent nominal categories, so per the skill's
# categorical-vs-ordinal rule they take a single-hue light->dark ordinal
# ramp (green) instead of three more ad-hoc hues -- inventing new undocumented
# hex values would violate "documented palette only" and, when checked,
# failed CVD separation outright (month/med_sst read near-gray and sat next
# to nao at dE 3.6, well under the floor). The ramp passes --ordinal
# (monotone L, adjacent dL>=0.06, light-end contrast 2.42:1); the dark step's
# FAIL under the *categorical* validator is expected and not a real issue --
# it's an intentional dark ordinal step, not a mistuned categorical slot.
FEATURE_COLORS = {
    "spei": "#2a78d6",
    "wb": "#eb6834",
    "pr": "#1baf7a",
    "ps": "#eda100",
    "tas": "#e87ba4",
    "nao": "#5cb85c",       # global driver: North Atlantic Oscillation (light green)
    "month": "#008300",     # global driver: seasonality, month sin/cos (mid green)
    "med_sst": "#004d00",   # global driver: Mediterranean SST, 3 regional means (dark green)
    "static": "#4a3aa7",
    "global": "#008300",    # fallback: whole global vector (non-grouped layouts; mutually
                             # exclusive with nao/month/med_sst, so reusing "month"'s hue is safe)
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


def _write_csv(path: Path, rows: list[dict]) -> None:
    if not rows:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="") as f:
        writer = csv_mod.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


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
    saved_models_dir = saved_models_dir or (project_root() / "saved_models")
    csv_path = csv_path or (project_root() / "reports" / "interpretability" / "feature_importance_pinball.csv")
    fig_dir = fig_dir or (project_root() / "figures" / "interpretability" / "feature_importance")

    ckpts = sorted(saved_models_dir.glob("best_model_pinball_q0.20*.ckpt"))
    assert len(ckpts) == 6, f"Expected the 6 Pinball architecture conditions, found {len(ckpts)}: {ckpts}"

    all_rows: list[dict] = []
    fig_dir.mkdir(parents=True, exist_ok=True)
    csv_path.parent.mkdir(parents=True, exist_ok=True)

    for i, ckpt in enumerate(ckpts, 1):
        print(f"\n[{i}/{len(ckpts)}] {ckpt.name}")
        result = evaluate_checkpoint(ckpt, progress=True)
        static_encoder = result["architecture"]["static_encoder"]
        global_encoder = result["architecture"]["global_encoder"]
        tag = f"{static_encoder}_{global_encoder}"
        baseline = result["baseline"]

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

        plot_feature_importance(tag, rows_for_plot, fig_dir / f"{tag}.svg")
        print(f"    figure -> {fig_dir / f'{tag}.svg'}")

    _write_csv(csv_path, all_rows)
    print(f"\n{len(all_rows)} rows -> {csv_path}")
    print(f"{len(ckpts)} figures -> {fig_dir}/")
    return all_rows


# ── Phase 2: does FiLM benefit the extremes? (all four losses) ─────────────
#
# For each loss, compares three checkpoints -- naive/naive (base), naive/film
# (isolates FiLM), seasonal/film (full model) -- with three deliverables:
#   1. Stratified error: RMSE and mean bias per observed-SPEI bin.
#   2. A worked 2022 case study (worst Alpine month, naive/naive vs naive/film).
#   3. A genuineness counterfactual: does the FiLM models' extreme-bin skill
#      collapse when the global scalars are zeroed (Phase 0's harness, reused
#      verbatim -- see interpretability.md's Phase 0 note that this same
#      zeroing is what Phase 2's counterfactual uses)?

FILM_LOSSES = ["mse", "pinball_q0.20", "wmse_w1_hinge", "wmse_w5_hinge"]
FILM_CONDITIONS = [("naive", "naive"), ("naive", "film"), ("seasonal", "film")]
CONDITION_LABELS = {cond: f"{cond[0]}/{cond[1]}" for cond in FILM_CONDITIONS}

# First 3 slots of the same validated categorical palette used in Phase 1 --
# that subset also clears the stricter *all-pairs* CVD gate, not just the
# adjacent-pair one, so it's safe here even though this legend has no fixed
# visual adjacency (bars can reorder across panels).
CONDITION_COLORS = {
    "naive/naive": "#2a78d6",
    "naive/film": "#eb6834",
    "seasonal/film": "#1baf7a",
}

# Columns of DroughtDataset.global_scalars in med_sst_agg="grouped" mode
# (dataset.py: [nao, month_sin, month_cos, western, eastern, black_sea]) --
# every saved_models/ checkpoint uses "grouped" (no .overrides sets
# data.med_sst_agg), so this ordering is safe to hard-code.
DRIVER_LABELS = {0: "NAO", 3: "W. Med SST", 4: "E. Med SST", 5: "Black Sea SST"}

# Rotated-pole CRS for the AL domain -- same parameters as visualization.py /
# utils/plotting.py (each already duplicates this locally rather than sharing
# a private import across modules).
_CRS_ROTATED_MODULE = None  # lazily created (needs cartopy.crs), see _crs()


def _crs():
    import cartopy.crs as ccrs

    global _CRS_ROTATED_MODULE
    if _CRS_ROTATED_MODULE is None:
        _CRS_ROTATED_MODULE = (
            ccrs.RotatedPole(pole_longitude=-162.0, pole_latitude=39.25),
            ccrs.PlateCarree(),
        )
    return _CRS_ROTATED_MODULE


def _rotated_extent(rlat: np.ndarray, rlon: np.ndarray, pad: float = 0.5) -> list[float]:
    rotated, plate = _crs()
    xs = [rlon.min(), rlon.max(), rlon.min(), rlon.max()]
    ys = [rlat.min(), rlat.min(), rlat.max(), rlat.max()]
    pts = plate.transform_points(rotated, np.array(xs), np.array(ys))
    return [pts[:, 0].min() - pad, pts[:, 0].max() + pad, pts[:, 1].min() - pad, pts[:, 1].max() + pad]


@dataclass
class _ConditionRun:
    preds: torch.Tensor
    targets: torch.Tensor
    mask: torch.Tensor
    dataset: DroughtDataset
    model: RCNNModule
    loader: DataLoader
    static_encoder: str
    global_encoder: str


def find_loss_checkpoint(saved_models_dir: Path, loss_tag: str, static_encoder: str, global_encoder: str) -> Path:
    """Find the one best_model_{loss_tag}_*.ckpt whose .overrides matches the
    requested architecture. Never infers architecture from the filename.
    """
    matches = []
    for ckpt in sorted(saved_models_dir.glob(f"best_model_{loss_tag}_*.ckpt")):
        se, ge, _ = architecture_from_overrides(read_overrides(ckpt))
        if se == static_encoder and ge == global_encoder:
            matches.append(ckpt)
    assert len(matches) == 1, (
        f"Expected exactly 1 checkpoint for loss={loss_tag!r} static={static_encoder!r} "
        f"global={global_encoder!r}, found {len(matches)}: {matches}"
    )
    return matches[0]


def test_target_dates(dataset: DroughtDataset) -> np.ndarray:
    """Target dates for the test split, aligned with run_eval()'s returned
    tensors along dim 0 (both derive from the same shuffle=False test Subset).
    """
    _, _, test_sub = dataset.split_by_years(val_from=VAL_FROM_YEAR, test_from=TEST_FROM_YEAR)
    return dataset.target_times[test_sub.indices]


def worst_2022_month_index(dates: np.ndarray, targets: torch.Tensor, mask: torch.Tensor) -> int:
    """Index (into `dates`/`targets`) of the 2022 target month with the lowest
    domain-mean observed SPEI over valid Alpine cells.
    """
    years = dates.astype("datetime64[Y]").astype(int) + 1970
    idx_2022 = np.where(years == 2022)[0]
    assert len(idx_2022) > 0, "No 2022 target months found in the test period"
    domain_mean = targets[idx_2022][:, mask].mean(dim=1)
    worst_local = int(torch.argmin(domain_mean).item())
    return int(idx_2022[worst_local])


def plot_stratified_comparison(loss_tag: str, per_condition: dict[str, list[dict]], fig_path: Path) -> None:
    """One two-panel figure per loss: RMSE-by-bin and bias-by-bin, grouped
    bars colored by condition (never a shared/dual y-axis).
    """
    import matplotlib.pyplot as plt

    bin_labels = [b[0] for b in SPEI_BINS]
    conditions = list(per_condition.keys())
    x = np.arange(len(bin_labels))
    width = 0.8 / len(conditions)

    fig, (ax_rmse, ax_bias) = plt.subplots(1, 2, figsize=(9.0, 3.2))
    for i, cond in enumerate(conditions):
        rows = per_condition[cond]
        offset = (i - (len(conditions) - 1) / 2) * width
        color = CONDITION_COLORS[cond]
        ax_rmse.bar(x + offset, [r["rmse"] for r in rows], width=width, color=color, label=cond)
        ax_bias.bar(x + offset, [r["mean_bias"] for r in rows], width=width, color=color, label=cond)

    ax_rmse.set_xticks(x)
    ax_rmse.set_xticklabels(bin_labels, fontsize=8)
    ax_rmse.set_ylabel("RMSE", fontsize=9)
    ax_rmse.set_title("Error by observed-SPEI bin", fontsize=9)

    ax_bias.set_xticks(x)
    ax_bias.set_xticklabels(bin_labels, fontsize=8)
    ax_bias.set_ylabel("Mean bias  (pred $-$ obs)", fontsize=9)
    ax_bias.set_title("Bias by observed-SPEI bin", fontsize=9)

    for ax in (ax_rmse, ax_bias):
        _style_axes(ax)
    ax_bias.legend(fontsize=7, frameon=False, loc="best")

    fig.suptitle(f"Stratified error — {loss_tag}", fontsize=10)
    fig.tight_layout()
    fig_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(fig_path, bbox_inches="tight")
    plt.close(fig)


def plot_case_study(
    loss_tag: str,
    date: np.datetime64,
    obs: torch.Tensor,
    pred_naive: torch.Tensor,
    pred_film: torch.Tensor,
    mask: torch.Tensor,
    rlat: np.ndarray,
    rlon: np.ndarray,
    driver_values: dict[str, float],
    fig_path: Path,
) -> None:
    """Multi-panel worked example: observed SPEI, naive/naive and naive/film
    predictions (shared colour scale), their error maps (separate shared
    colour scale), and the global driver state FiLM conditioned on.
    """
    import cartopy.feature as cfeature
    import matplotlib.pyplot as plt

    rotated, plate = _crs()
    m = mask.cpu().numpy().astype(bool)

    def _masked(t: torch.Tensor) -> np.ndarray:
        a = t.detach().cpu().numpy().copy()
        a[~m] = np.nan
        return a

    obs_np, pn_np, pf_np = _masked(obs), _masked(pred_naive), _masked(pred_film)
    en_np, ef_np = pn_np - obs_np, pf_np - obs_np

    vabs_spei = float(np.nanmax(np.abs(np.stack([obs_np, pn_np, pf_np]))))
    vabs_err = float(np.nanmax(np.abs(np.stack([en_np, ef_np]))))

    fig = plt.figure(figsize=(11.0, 6.5), layout="constrained")
    map_axes = [fig.add_subplot(2, 3, i, projection=plate) for i in (1, 2, 3, 4, 5)]
    ax_bar = fig.add_subplot(2, 3, 6)

    panels = [
        (obs_np, "Observed SPEI", "RdBu", -vabs_spei, vabs_spei),
        (pn_np, "naive/naive prediction", "RdBu", -vabs_spei, vabs_spei),
        (pf_np, "naive/film prediction", "RdBu", -vabs_spei, vabs_spei),
        (en_np, "naive/naive error (pred$-$obs)", "PuOr", -vabs_err, vabs_err),
        (ef_np, "naive/film error (pred$-$obs)", "PuOr", -vabs_err, vabs_err),
    ]
    spei_im = err_im = None
    for ax, (data, title, cmap, vmin, vmax) in zip(map_axes, panels, strict=True):
        im = ax.pcolormesh(
            rlon, rlat, data, transform=rotated, cmap=cmap, vmin=vmin, vmax=vmax, shading="auto", rasterized=True
        )
        ax.add_feature(cfeature.COASTLINE.with_scale("10m"), linewidth=0.5, edgecolor="black")
        ax.add_feature(cfeature.BORDERS.with_scale("10m"), linewidth=0.4, edgecolor="black", linestyle="--")
        ax.set_extent(_rotated_extent(rlat, rlon), crs=plate)
        ax.set_title(title, fontsize=9)
        if cmap == "RdBu":
            spei_im = im
        else:
            err_im = im

    fig.colorbar(spei_im, ax=map_axes[:3], orientation="horizontal", fraction=0.05, pad=0.1, shrink=0.7, label="SPEI")
    fig.colorbar(
        err_im, ax=map_axes[3:5], orientation="horizontal", fraction=0.05, pad=0.1, shrink=0.7, label="error"
    )

    names = list(driver_values.keys())
    vals = [driver_values[n] for n in names]
    ax_bar.bar(names, vals, color="#4a3aa7", width=0.6)
    ax_bar.axhline(0, color="#c3c2b7", lw=0.8)
    ax_bar.set_ylabel("z-score (FiLM input)", fontsize=8)
    ax_bar.set_title("Global driver state", fontsize=9)
    ax_bar.tick_params(axis="x", labelsize=7, rotation=30)
    ax_bar.tick_params(axis="y", labelsize=7)
    ax_bar.spines[["top", "right"]].set_visible(False)

    fig.suptitle(f"{loss_tag} — worst 2022 Alpine month: {str(date)[:7]}", fontsize=11)
    fig_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(fig_path, bbox_inches="tight")
    plt.close(fig)


def plot_counterfactual(loss_tag: str, rows: list[dict], fig_path: Path) -> None:
    """One bar per FiLM condition: how much extreme-bin (SPEI<=-1.5) RMSE
    changes when the global scalars are zeroed vs. the true-scalar run.
    """
    import matplotlib.pyplot as plt

    labels = [r["condition"] for r in rows]
    colors = [CONDITION_COLORS[c] for c in labels]
    deltas = [r["delta_extreme_rmse"] for r in rows]

    fig, ax = plt.subplots(figsize=(4.0, 3.2))
    ax.bar(labels, deltas, color=colors, width=0.5)
    ax.set_ylabel(r"$\Delta$RMSE, SPEI$\leq$-1.5  (zeroed $-$ true scalars)", fontsize=8)
    ax.set_title(f"Genuineness counterfactual — {loss_tag}", fontsize=9)
    _style_axes(ax)
    ax.tick_params(axis="x", rotation=15)
    fig.tight_layout()
    fig_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(fig_path, bbox_inches="tight")
    plt.close(fig)


def run_film_extremes(
    saved_models_dir: Path | None = None,
    csv_dir: Path | None = None,
    fig_dir: Path | None = None,
) -> dict:
    """Phase 2 entry point: for each of the 4 losses, run the stratified
    comparison, the 2022 case study, and the genuineness counterfactual.
    Prints progress per loss/condition as it runs. Writes 3 CSVs and
    3 figures per loss (12 figures total) and returns the raw rows.
    """
    saved_models_dir = saved_models_dir or (project_root() / "saved_models")
    csv_dir = csv_dir or (project_root() / "reports" / "interpretability")
    fig_dir = fig_dir or (project_root() / "figures" / "interpretability" / "film_extremes")
    csv_dir.mkdir(parents=True, exist_ok=True)
    fig_dir.mkdir(parents=True, exist_ok=True)

    stratified_rows: list[dict] = []
    case_study_rows: list[dict] = []
    counterfactual_rows: list[dict] = []

    for li, loss_tag in enumerate(FILM_LOSSES, 1):
        print(f"\n[{li}/{len(FILM_LOSSES)}] loss={loss_tag}")

        runs: dict[str, _ConditionRun] = {}
        strat_by_condition: dict[str, list[dict]] = {}
        for static_encoder, global_encoder in FILM_CONDITIONS:
            label = CONDITION_LABELS[(static_encoder, global_encoder)]
            ckpt = find_loss_checkpoint(saved_models_dir, loss_tag, static_encoder, global_encoder)
            dataset = build_dataset(static_encoder, global_encoder)
            model = load_model(ckpt, dataset, static_encoder, global_encoder)
            loader = DataLoader(
                wrap_test_subset(dataset, static_encoder, global_encoder), batch_size=16, shuffle=False
            )
            print(f"    [{label}] baseline eval  [{len(loader)} batches]")
            preds, targets = run_eval(model, loader, dataset, static_encoder, global_encoder, progress=True)
            runs[label] = _ConditionRun(
                preds, targets, model.mask, dataset, model, loader, static_encoder, global_encoder
            )

            strat = stratified_metrics(targets, preds, model.mask)
            strat_by_condition[label] = strat
            for row in strat:
                stratified_rows.append({"loss": loss_tag, "condition": label, **row})
            print(f"    [{label}] " + "  ".join(f"{r['bin']}:rmse={r['rmse']:.3f}" for r in strat))

        stratified_fig = fig_dir / f"{loss_tag}_stratified.svg"
        plot_stratified_comparison(loss_tag, strat_by_condition, stratified_fig)
        print(f"    stratified figure -> {stratified_fig}")

        # ── 2022 case study: naive/naive vs naive/film ──────────────────────
        nn, nf = runs["naive/naive"], runs["naive/film"]
        dates = test_target_dates(nn.dataset)
        worst_idx = worst_2022_month_index(dates, nn.targets, nn.mask)
        worst_date = dates[worst_idx]

        obs = nn.targets[worst_idx]
        pred_nn = nn.preds[worst_idx]
        pred_nf = nf.preds[worst_idx]

        _, _, nf_test_sub = nf.dataset.split_by_years(val_from=VAL_FROM_YEAR, test_from=TEST_FROM_YEAR)
        sample_idx = nf_test_sub.indices[worst_idx]
        t_last_input = sample_idx + HISTORY_LENGTH - 1
        scalars_at_t = nf.dataset.global_scalars[t_last_input]
        driver_values = {label: scalars_at_t[i].item() for i, label in DRIVER_LABELS.items()}

        case_fig = fig_dir / f"{loss_tag}_2022_case_study.svg"
        plot_case_study(
            loss_tag, worst_date, obs, pred_nn, pred_nf, nn.mask,
            nn.dataset.rlat, nn.dataset.rlon, driver_values, case_fig,
        )
        print(f"    2022 case study: {str(worst_date)[:7]}  -> {case_fig}")
        case_study_rows.append({
            "loss": loss_tag,
            "date": str(worst_date)[:10],
            "domain_mean_observed_spei": float(obs[nn.mask].mean().item()),
            **{f"driver_{k}": v for k, v in driver_values.items()},
        })

        # ── Genuineness counterfactual: naive/film, seasonal/film ───────────
        cf_rows_this_loss = []
        for label in ("naive/film", "seasonal/film"):
            run = runs[label]
            spec = AblationSpec("global", "global")
            print(f"    [{label}] zeroed-scalar counterfactual eval  [{len(run.loader)} batches]")
            preds_zeroed, _ = run_eval(
                run.model, run.loader, run.dataset, run.static_encoder, run.global_encoder, spec=spec, progress=True
            )
            rmse_true = drought_rmse_pooled(run.targets, run.preds, run.mask, DROUGHT_THRESHOLD).item()
            rmse_zeroed = drought_rmse_pooled(run.targets, preds_zeroed, run.mask, DROUGHT_THRESHOLD).item()
            row = {
                "loss": loss_tag,
                "condition": label,
                "extreme_rmse_true_scalars": rmse_true,
                "extreme_rmse_zeroed_scalars": rmse_zeroed,
                "delta_extreme_rmse": rmse_zeroed - rmse_true,
            }
            counterfactual_rows.append(row)
            cf_rows_this_loss.append(row)
            print(f"    [{label}] extreme RMSE (SPEI<=-1.5): true={rmse_true:.4f}  "
                  f"zeroed={rmse_zeroed:.4f}  delta={row['delta_extreme_rmse']:+.4f}")

        cf_fig = fig_dir / f"{loss_tag}_counterfactual.svg"
        plot_counterfactual(loss_tag, cf_rows_this_loss, cf_fig)
        print(f"    counterfactual figure -> {cf_fig}")

    _write_csv(csv_dir / "film_stratified.csv", stratified_rows)
    _write_csv(csv_dir / "film_2022_case_studies.csv", case_study_rows)
    _write_csv(csv_dir / "film_counterfactual.csv", counterfactual_rows)
    print(f"\nCSVs -> {csv_dir}/")
    print(f"Figures -> {fig_dir}/")
    return {"stratified": stratified_rows, "case_studies": case_study_rows, "counterfactual": counterfactual_rows}


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["feature-importance", "film-extremes"])
    args = parser.parse_args()

    if args.command == "feature-importance":
        run_feature_importance()
    elif args.command == "film-extremes":
        run_film_extremes()
