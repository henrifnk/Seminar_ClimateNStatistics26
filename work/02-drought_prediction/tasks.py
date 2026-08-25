import csv
import glob
import itertools
import os
import shlex
import sys
import tempfile
import textwrap
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

from invoke import Context, task  # noqa: E402

# Anchor every path to this file's directory, not the launching process's cwd,
# so the same commands work whether invoke is run from the GitBook repo root
# or from this chapter folder directly (invoke does not cd into the directory
# it found tasks.py in -- it only searches upward for the file).
ROOT = Path(__file__).parent.resolve()
SAVED_MODELS_DIR = str(ROOT / "saved_models")
FIGURES_DIR = str(ROOT / "figures" / "models")

# The `pty` module (used for proper terminal passthrough) doesn't exist on Windows.
_PTY = os.name != "nt"


def _run(c: Context, cmd: str, **kwargs):
    """c.run(), but always executed with cwd = ROOT regardless of where invoke
    itself was launched from."""
    kwargs.setdefault("pty", _PTY)
    return c.run(f"cd {shlex.quote(str(ROOT))} && {cmd}", **kwargs)

# ---------------------------------------------------------------------------
# Data pipeline
# ---------------------------------------------------------------------------


@task
def preprocess(c: Context):
    """Build data/processed/ from raw NetCDF files."""
    _run(c, "uv run python code/preprocessing.py")


@task
def visualize(c: Context, start: int = 0, panels: int = 12):
    """Save raw + processed EDA map figures to figures/. Use --start/--panels for the time window."""
    _run(
        c,
        f'uv run python -c "'
        f"import sys; sys.path.insert(0, 'code'); "
        f"import visualization as v; "
        f"v.visualize_raw_nao_pc1(); "
        f"v.visualize_raw_nao_eof(); "
        f"v.visualize_raw_mediterranean_slt(); "
        f"v.visualize_raw_topography(); "
        f"v.visualize_raw_al_variables(start_idx={start}, n_panels={panels}); "
        f"v.visualize_raw_mediterranean_slt_slide(); "
        f"v.visualize_processed_dynamic(start_idx={start}, n_panels={panels}); "
        f"v.visualize_processed_static(); "
        f"v.visualize_processed_global_scalars(); "
        f"v.visualize_drought_frequency_severe()"
        f'"',
    )


@task
def explore(c: Context, val_from_year: int = 2005, test_from_year: int = 2015):
    """Exploratory drought analysis: SPEI time series + per-cell event count maps.

    Figures are written to figures/processed/exploratory/.
    """
    _run(
        c,
        f'uv run python -c "'
        f"import sys; sys.path.insert(0, 'code'); "
        f"import visualization as v; "
        f"v.visualize_drought_exploratory("
        f"val_from_year={val_from_year}, "
        f"test_from_year={test_from_year}"
        f')"',
    )


# ---------------------------------------------------------------------------
# Training
# ---------------------------------------------------------------------------


@task
def train(c: Context, overrides: str = ""):
    """Train the ConvLSTM model. Pass Hydra overrides via --overrides 'key=val ...'"""
    _run(c, f"uv run python code/train.py {overrides}")


@task
def cfg(c: Context, overrides: str = ""):
    """Print the resolved Hydra config without running training."""
    _run(c, f"uv run python code/train.py --cfg job --resolve {overrides}")


@task(pre=[preprocess])
def pipeline(c: Context, overrides: str = ""):
    """Run the full pipeline: preprocess -> train."""
    _run(c, f"uv run python code/train.py {overrides}")


@task
def baselines(c: Context, processed_dir: str = ""):
    """Evaluate persistence, climatology, and linear-trend baselines on the test set.

    Computes the same metrics and figures as any model run (RMSE, corr, drought
    acc/F1/TPR spatial maps + test_metrics.json) and saves them once to
    figures/baselines/{persistence,climatology,trend}/. Baselines are data-derived
    (no checkpoint involved).
    """
    cmd = "uv run python code/eval_baselines.py"
    if processed_dir:
        cmd += f" --processed-dir {processed_dir}"
    _run(c, cmd)


@task
def feature_importance(c: Context):
    """Phase 1 interpretability: occlusion feature importance across the six
    Pinball architecture conditions (saved_models/best_model_pinball_q0.20_*.ckpt).

    No retraining -- test period only. Prints per-checkpoint/per-feature
    progress as it runs. Writes reports/interpretability/feature_importance_pinball.csv
    and one two-panel (dRMSE, Ddrought-F1) SVG per condition to
    figures/interpretability/feature_importance/.
    """
    _run(c, "uv run python code/interpretability.py feature-importance")


@task
def feature_importance_replot(c: Context):
    """Regenerate Phase 1 figures from the existing CSV, no eval re-run.

    Use after a plotting-only fix once `feature_importance` has already
    written reports/interpretability/feature_importance_pinball.csv -- skips
    the six checkpoints x per-feature occlusion passes entirely.
    """
    _run(c, "uv run python code/interpretability.py feature-importance-replot")


@task
def film_extremes(c: Context):
    """Phase 2 interpretability: does FiLM benefit the extremes?

    For each of the 4 losses (mse, pinball_q0.20, wmse_w1_hinge, wmse_w5_hinge),
    compares naive/naive, naive/film, and seasonal/film checkpoints. No
    retraining -- test period only. Prints per-loss/per-condition progress as
    it runs. Writes 3 CSVs (stratified error, 2022 case studies, genuineness
    counterfactual) to reports/interpretability/ and 3 figures per loss (12
    total) to figures/interpretability/film_extremes/.
    """
    _run(c, "uv run python code/interpretability.py film-extremes")


@task
def smoke(c: Context, epochs: int = 2, keep: bool = False):
    """End-to-end smoke test on a tiny synthetic fixture -- no real dataset needed.

    Generates a small synthetic processed_dir (8x8 grid, real 1971-2024 time
    span, via code/utils/make_fixture.py) and runs dataset -> model -> training ->
    test on CPU, so the full pipeline can be verified without the ~327MB real
    dataset. Cleans up the fixture and its checkpoint/logs afterward unless
    --keep is passed.
    """
    import shutil
    import tempfile

    fixture_dir = Path(tempfile.mkdtemp(prefix="drought_fixture_"))
    print(f"Generating synthetic fixture -> {fixture_dir}")
    _run(c, f"uv run python code/utils/make_fixture.py {shlex.quote(str(fixture_dir))}")

    run_name = "smoke_test"
    overrides = (
        f"data.processed_dir={shlex.quote(str(fixture_dir))} data.num_workers=0 "
        f"trainer.max_epochs={epochs} trainer.accelerator=cpu logger=csv "
        f"run_name={run_name}"
    )
    print("\nRunning smoke train -> test ...")
    _run(c, f"uv run python code/train.py {overrides}")

    if not keep:
        shutil.rmtree(fixture_dir, ignore_errors=True)
        for p in Path(SAVED_MODELS_DIR).glob(f"best_model_{run_name}.*"):
            p.unlink()
        print(f"\nCleaned up fixture ({fixture_dir}) and smoke-test checkpoint.")

    print("\nSmoke test passed.")


# ---------------------------------------------------------------------------
# WandB comparison table
# ---------------------------------------------------------------------------

_COMPARISON_METRICS = [
    # -- Val metrics (checkpoint-aligned) --------------------------------------
    "val/loss",              # within-run selection criterion (monitored by ModelCheckpoint)
    "val/rmse_median",
    "val/drought_f1_pooled",
    "val/clim/rmse_median",
    "val/drought_rocauc_median",
    "val/drought_rmse_pooled",
    # -- Headline: pooled over all valid Alpine cell-months --------------------
    "test/rmse_pooled",
    "test/mae_pooled",
    "test/corr_pooled",
    "test/persistence/rmse_pooled",
    "test/persistence/drought_tpr_pooled",
    "test/persistence/drought_f1_pooled",
    "test/clim/rmse_pooled",
    "test/clim/drought_tpr_pooled",
    "test/clim/drought_f1_pooled",
    "test/trend/rmse_pooled",
    "test/trend/drought_tpr_pooled",
    "test/trend/drought_f1_pooled",
    # rmse_vs_trend = trend_rmse_pooled - model_rmse_pooled
    # positive = model beats trend (lower error), negative = model worse than trend
    "test/rmse_vs_trend",
    "test/drought_f1_pooled",
    "test/drought_tpr_pooled",
    "test/drought_rocauc_median",   # per-cell median AUROC (no pooled equivalent)
    # -- Appendix: per-cell median (model only; baselines -> figures/baselines/) -
    "test/rmse_median",
    "test/corr_median",
    "test/persistence/rmse_median",
    "test/clim/rmse_median",
    # Not logged by the current model.py (kept only so older CSV rows that have
    # them aren't silently dropped on the next merge-write; always empty for
    # runs produced by the current code).
    "test/trend/rmse_median",
    "test/trend/corr_median",
    "test/residual_floor",          # test-period detrended std -- data property, not a metric
    "test/drought_f1_median",
    "test/drought_acc_median",
    "test/drought_tpr_median",
]

_COMPARISON_HPARAMS = [
    ("model", "loss_fn"),
    ("model", "lr"),
    ("model", "dropout"),
    ("model", "weight_decay"),
    ("model", "static_encoder"),
    ("model", "global_encoder"),
    ("data", "med_sst_agg"),
    ("model", "pinball_quantile"),
    ("model", "drought_weight"),
]


def _export_comparison_table(
    run_names: list[str],
    project: str = "drought-prediction",
    entity: str | None = None,
    out_path: str = f"{SAVED_MODELS_DIR}/gridsearch_comparison.csv",
) -> None:
    """Fetch metrics for each run from WandB and write a sorted comparison CSV.

    Val metrics are read from run history at the best-val/loss epoch so they
    are aligned with the saved checkpoint, not the final training epoch.
    Test metrics are read from run.summary (logged once after testing).
    Merges with any existing CSV so other runs are not clobbered.
    """
    try:
        import wandb
    except ImportError:
        print("wandb not installed -- skipping comparison table export.")
        return

    api = wandb.Api()
    resolved_entity = entity or api.default_entity

    print(f"\nFetching {len(run_names)} runs from WandB ({resolved_entity}/{project}) ...")
    all_runs = {r.display_name: r for r in api.runs(f"{resolved_entity}/{project}")}

    val_metrics = [m for m in _COMPARISON_METRICS if m.startswith("val/")]
    test_metrics = [m for m in _COMPARISON_METRICS if not m.startswith("val/")]

    rows = []
    for name in run_names:
        run = all_runs.get(name)
        if run is None:
            print(f"  Warning: '{name}' not found in WandB -- skipping")
            continue
        row: dict = {"run_name": name}

        for group, key in _COMPARISON_HPARAMS:
            row[key] = run.config.get(group, {}).get(key, "")
        row["seed"] = run.config.get("seed", "")

        # Test metrics: logged once at test time -- summary is authoritative.
        for metric in test_metrics:
            row[metric] = run.summary.get(metric, "")
        eval_run = all_runs.get(f"{name}_eval")
        if eval_run is not None:
            for metric in test_metrics:
                v = eval_run.summary.get(metric, "")
                if v != "" and v == v:  # prefer _eval value if present and not NaN
                    row[metric] = v

        # Val metrics: logged every epoch -- read from history at best val/loss epoch.
        try:
            hist_keys = list(dict.fromkeys(["val/loss"] + val_metrics))
            hist = run.history(keys=hist_keys)
            if not hist.empty and "val/loss" in hist.columns:
                best_idx = hist["val/loss"].idxmin()
                for metric in val_metrics:
                    row[metric] = hist.loc[best_idx, metric] if metric in hist.columns else ""
            else:
                for metric in val_metrics:
                    row[metric] = run.summary.get(metric, "")
        except Exception as exc:
            print(f"  Warning: history fetch failed for '{name}' ({exc}) -- falling back to summary")
            for metric in val_metrics:
                row[metric] = run.summary.get(metric, "")

        rows.append(row)

    if not rows:
        print("No matching WandB runs found.")
        return

    out = Path(out_path)
    out.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = ["run_name", "seed"] + [k for _, k in _COMPARISON_HPARAMS] + _COMPARISON_METRICS

    # Merge with existing CSV so previous conditions are preserved
    existing: dict[str, dict] = {}
    if out.exists():
        with open(out, newline="") as f:
            for old_row in csv.DictReader(f):
                existing[old_row["run_name"]] = old_row

    new_by_name = {r["run_name"]: r for r in rows}
    existing.update(new_by_name)  # new rows take precedence on collision

    merged = sorted(
        existing.values(),
        key=lambda r: float(r.get("test/drought_f1_pooled") or 0),
        reverse=True,
    )

    with open(out, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(merged)

    print(f"Comparison table ({len(merged)} runs, {len(new_by_name)} new/updated) -> {out}")


@task
def compare(c: Context, project: str = "drought-prediction", entity: str = ""):
    """Export a WandB comparison CSV for all runs currently in saved_models/.

    Reads run names from saved_models/best_model_*.ckpt filenames, queries WandB
    for their metrics, and writes saved_models/gridsearch_comparison.csv sorted by
    test/drought_f1_pooled descending.
    """
    ckpts = glob.glob(f"{SAVED_MODELS_DIR}/best_model_*.ckpt")
    if not ckpts:
        print(f"No saved models found in {SAVED_MODELS_DIR}/")
        return
    names = [Path(p).stem.replace("best_model_", "") for p in ckpts]
    _export_comparison_table(names, project=project, entity=entity or None)


def _evaluate_all(c: Context, ckpts: list[str], force: bool = False) -> None:
    """Run eval-only inference for each checkpoint and copy figures.

    Skips any checkpoint whose figures/models/<tag>_eval/ output already
    exists (both the spatial PNGs and test_metrics.json -- a partial/interrupted
    prior run leaving only PNGs does not count as done), unless force=True.

    Forces logger=csv regardless of the checkpoint's own training config: this
    rerun only exists to regenerate local spatial figures from an already-logged
    checkpoint, so it must never create a new WandB run -- the original training
    run is already the authoritative WandB record for these metrics.
    """
    if not ckpts:
        print("No checkpoints to evaluate.")
        return
    for ckpt in ckpts:
        tag = Path(ckpt).stem.replace("best_model_", "")
        fig_dir = Path(FIGURES_DIR) / f"{tag}_eval"
        done = fig_dir.exists() and any(fig_dir.glob("*.png")) and (fig_dir / "test_metrics.json").exists()
        if not force and done:
            print(f"  [{tag}] already evaluated at {fig_dir} -- skipping (force=True to re-run)")
            continue
        overrides_path = Path(ckpt).with_suffix(".overrides")
        if not overrides_path.exists():
            print(f"  [{tag}] No .overrides file -- skipping (re-run gridsearch to generate it)")
            continue
        overrides = overrides_path.read_text().strip().replace("\n", " ")
        cmd = (
            f"uv run python code/train.py {overrides}"
            f" eval_only=true eval_checkpoint={shlex.quote(ckpt)}"
            f" run_name={tag}_eval logger=csv"
        )
        print(f"\nEvaluating {tag} ...")
        _run(c, cmd)


@task
def evaluate(c: Context, checkpoint: str = "", force: bool = False):
    """Evaluate saved model(s) on the test set and save spatial figures.

    With no arguments, evaluates every best_model_*.ckpt in saved_models/.
    Pass --checkpoint path/to/model.ckpt to evaluate a single model.

    Checkpoints whose figures already exist are skipped by default (test
    metrics are already logged from training -- evaluate only regenerates
    figures). Pass --force to re-evaluate everything anyway.

    Each model's figures are written to figures/models/<tag>/. Requires a
    <tag>.overrides file alongside the checkpoint (written automatically by
    train.py / gridsearch).
    """
    ckpts = [checkpoint] if checkpoint else sorted(glob.glob(f"{SAVED_MODELS_DIR}/best_model_*.ckpt"))
    if not ckpts:
        print(f"No checkpoints found in {SAVED_MODELS_DIR}/")
        return
    _evaluate_all(c, ckpts, force=force)


@task
def report(c: Context):
    """Generate reports/output/results_report.html from saved_models/gridsearch_comparison.csv."""
    _run(c, "uv run python reports/generate/gen_report.py")


# ---------------------------------------------------------------------------
# Grid search -- the final 24-cell grid, each cell HP-swept
#
# RQ1 (loss, 4 cells): mse | weighted_mse hinge w=1 | weighted_mse hinge w=5 | pinball q=0.20
# RQ2 (dynamic global features, 2): naive | film
# RQ3 (static spatial features, 3): naive | single | seasonal
# 4 x 2 x 3 = 24 cells. Within each cell, lr x dropout x weight_decay (2x2x2=8
# combos) are swept and only the single best checkpoint (lowest val/loss) is
# kept -- this is what actually produced the results in reports/notes.md and
# reports/output/results_report.html (run names there carry the winning HP + seed,
# e.g. `mse_naive_film_lr3e-03_do0.0_wd1e-04_s42`), not a fixed-HP run per cell.
# ---------------------------------------------------------------------------

_SEED = 42

_COMMON_HP_AXES: dict[str, list] = {
    "model.lr": [1e-3, 3e-3],
    "model.dropout": [0.0, 0.1],
    "model.weight_decay": [0.0, 1e-4],
}

# loss_tag -> fixed (non-HP-swept) overrides for that loss cell. drought_weight
# is part of the *identity* of the two weighted_mse cells (RQ1 treats w=1 and
# w=5 as two separate final losses to report, not two values to pick between),
# so it is fixed per tag rather than swept.
_GRID_LOSSES: dict[str, dict] = {
    "mse": {"model.loss_fn": "mse"},
    "weighted_mse_w1_hinge": {
        "model.loss_fn": "weighted_mse",
        "model.weighted_loss_mode": "hinge",
        "model.drought_weight": 1.0,
    },
    "weighted_mse_w5_hinge": {
        "model.loss_fn": "weighted_mse",
        "model.weighted_loss_mode": "hinge",
        "model.drought_weight": 5.0,
    },
    "pinball_q0.20": {"model.loss_fn": "pinball", "model.pinball_quantile": 0.2},
}
_GRID_STATIC = ["naive", "single", "seasonal"]
_GRID_GLOBAL = ["naive", "film"]


def _hp_combos() -> list[tuple[float, float, float]]:
    return list(itertools.product(*_COMMON_HP_AXES.values()))


def _cell_run_name(loss_tag: str, static: str, global_enc: str, lr: float, do: float, wd: float, seed: int) -> str:
    return f"{loss_tag}_{static}_{global_enc}_lr{lr:.0e}_do{do}_wd{wd:.0e}_s{seed}"


def _sweep_one_cell(
    c: Context,
    loss_tag: str,
    static: str,
    global_enc: str,
    seed: int,
    dry_run: bool,
    cell_label: str,
) -> str | None:
    """Run the 8-combo HP sweep for one (loss, static, global) cell and keep only
    the best checkpoint (lowest val/loss). Returns the winning run_name, or None
    if dry_run or if nothing could be trained/found.
    """
    loss_overrides = _GRID_LOSSES[loss_tag]
    combos = _hp_combos()
    session_models: list[tuple[str, str]] = []

    for j, (lr, do, wd) in enumerate(combos, 1):
        run_name = _cell_run_name(loss_tag, static, global_enc, lr, do, wd, seed)
        overrides = {
            **loss_overrides,
            "model.static_encoder": static,
            "model.global_encoder": global_enc,
            "model.lr": lr,
            "model.dropout": do,
            "model.weight_decay": wd,
            "seed": seed,
        }
        cmd = "uv run python code/train.py " + " ".join(f"{k}={v}" for k, v in overrides.items())
        cmd += f" run_name={run_name} logger.run_name={run_name}"
        expected_path = f"{SAVED_MODELS_DIR}/best_model_{run_name}.ckpt"
        exists = Path(expected_path).exists()

        if dry_run:
            print(f"    ({j}/{len(combos)}) {'[skip-existing] ' if exists else ''}{cmd}")
            continue

        if exists:
            print(f"    ({j}/{len(combos)}) [skip] checkpoint already exists -> {run_name}")
            session_models.append((run_name, expected_path))
            continue

        print(f"    ({j}/{len(combos)}) {run_name}")
        _run(c, cmd)
        if Path(expected_path).exists():
            session_models.append((run_name, expected_path))

    if dry_run:
        return None
    if not session_models:
        print(f"  WARNING [{cell_label}]: no checkpoints produced or found -- skipping this cell")
        return None
    if len(session_models) == 1:
        winner_name, _ = session_models[0]
        print(f"  Winner [{cell_label}]: {winner_name}  (only candidate)")
        return winner_name

    # Compare val/loss across candidates via each checkpoint's own ModelCheckpoint
    # callback state (avoids depending on WandB/CSV logs being present).
    score_script = textwrap.dedent(f"""\
        import torch
        paths = {[p for _, p in session_models]!r}
        for path in paths:
            ckpt = torch.load(path, map_location="cpu", weights_only=False)
            cb = ckpt.get("callbacks", {{}})
            score = next(
                (v["best_model_score"] for k, v in cb.items() if "ModelCheckpoint" in k),
                None,
            )
            print(float(score) if score is not None else "nan", path)
    """)
    with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(score_script)
        script_path = f.name
    try:
        result = _run(c, f"uv run python {script_path}", hide=True, pty=False)
    finally:
        os.unlink(script_path)

    path_to_run = {path: run_name for run_name, path in session_models}
    scores: list[tuple[float, str, str]] = []
    for line in result.stdout.strip().splitlines():
        val_loss_str, path = line.split(maxsplit=1)
        scores.append((float(val_loss_str), path_to_run.get(path, path), path))

    scores.sort()
    best_val_loss, best_run, best_path = scores[0]
    print(f"  Winner [{cell_label}]: {best_run}  val/loss={best_val_loss:.4f}")
    for _, loser_run, loser_path in scores[1:]:
        os.remove(loser_path)
        overrides_file = Path(loser_path).with_suffix(".overrides")
        if overrides_file.exists():
            overrides_file.unlink()
        print(f"    dropped: {loser_run}")

    return best_run


@task
def gridsearch(
    c: Context,
    loss: str = "all",
    static_encoder: str = "all",
    global_encoder: str = "all",
    seed: int = _SEED,
    dry_run: bool = False,
):
    """Sweep the 24-cell grid (4 losses x 2 global encoders x 3 static encoders).

    Each cell sweeps lr x dropout x weight_decay (8 combos) and keeps only the
    best checkpoint by val/loss -- this reproduces the actual final procedure
    (not a single fixed-HP run per cell). --loss / --static-encoder /
    --global-encoder accept 'all' (default) or a single value, so a subset of
    the grid can be run at a time. Checkpoints that already exist (by exact
    run name, including HP+seed) are skipped and still count as a candidate.
    After all requested cells complete, metrics for the winning runs are
    fetched from WandB into saved_models/gridsearch_comparison.csv (merged,
    not clobbered) and each winner is evaluated (figures written once).
    """
    losses = list(_GRID_LOSSES) if loss == "all" else [loss]
    statics = _GRID_STATIC if static_encoder == "all" else [static_encoder]
    globals_ = _GRID_GLOBAL if global_encoder == "all" else [global_encoder]
    for name, valid in (("--loss", (list(_GRID_LOSSES), losses)), ("--static-encoder", (_GRID_STATIC, statics)),
                        ("--global-encoder", (_GRID_GLOBAL, globals_))):
        allowed, chosen = valid
        unknown = [v for v in chosen if v not in allowed]
        if unknown:
            raise ValueError(f"Unknown {name} value(s) {unknown}. Choose from {allowed} or 'all'.")

    cells = list(itertools.product(statics, globals_, losses))
    n_hp = len(_hp_combos())
    print(f"Planned cells: {len(cells)}  (x{n_hp} HP combos each = {len(cells) * n_hp} candidate runs)")

    winner_names: list[str] = []
    for i, (static, global_enc, loss_tag) in enumerate(cells, 1):
        cell_label = f"{loss_tag}/{static}/{global_enc}"
        print(f"\n[{i}/{len(cells)}] {cell_label}")
        winner = _sweep_one_cell(c, loss_tag, static, global_enc, seed, dry_run, cell_label)
        if winner:
            winner_names.append(winner)

    if dry_run:
        return

    if winner_names:
        _export_comparison_table(winner_names)
        ckpt_paths = (f"{SAVED_MODELS_DIR}/best_model_{name}.ckpt" for name in winner_names)
        ckpts = [p for p in ckpt_paths if Path(p).exists()]
        if ckpts:
            print(f"\n{'=' * 60}\nEvaluating {len(ckpts)} model(s) ...\n{'=' * 60}")
            _evaluate_all(c, ckpts)
