"""Invoke task runner for the Alpine drought-prediction pipeline.

Every task changes into this file's directory before running anything, so the
same commands work whether `invoke` is launched from the GitBook repo root or
from work/02-drought_prediction/ directly.

    uv run invoke preprocess
    uv run invoke cfg
    uv run invoke cfg --overrides "model.loss_fn=weighted_mse model.drought_weight=5"
    uv run invoke train
    uv run invoke train --overrides "model.global_encoder=film model.static_encoder=seasonal"
    uv run invoke baselines
    uv run invoke evaluate
    uv run invoke evaluate --checkpoint saved_models/best_model_mse.ckpt
    uv run invoke report
    uv run invoke gridsearch --dry-run
"""

from pathlib import Path

from invoke import task

ROOT = Path(__file__).parent.resolve()


def _run(c, script: str, args: str = "", overrides: str = "") -> None:
    cmd = " ".join(part for part in (f"python {script}", args, overrides) if part)
    print(f"$ {cmd}")
    with c.cd(str(ROOT)):
        c.run(cmd)


@task
def preprocess(c):
    """Build data/processed/ from data/raw/ (code/preprocessing.py)."""
    _run(c, "code/preprocessing.py")


@task
def cfg(c, overrides=""):
    """Print the fully resolved Hydra config and exit (no training)."""
    _run(c, "code/train.py", args="--cfg job", overrides=overrides)


@task
def train(c, overrides=""):
    """Run a single training job. Pass Hydra overrides as one quoted string."""
    _run(c, "code/train.py", overrides=overrides)


@task
def baselines(c, processed_dir=""):
    """Evaluate persistence / climatology / linear-trend baselines once."""
    args = f"--processed-dir {processed_dir}" if processed_dir else ""
    _run(c, "code/eval_baselines.py", args=args)


@task
def evaluate(c, checkpoint=""):
    """Evaluate saved_models/best_model_*.ckpt (or a single --checkpoint)."""
    if checkpoint:
        ckpts = [Path(checkpoint)]
    else:
        ckpts = sorted((ROOT / "saved_models").glob("best_model_*.ckpt"))

    if not ckpts:
        print("No checkpoints found in saved_models/. Train a model first (invoke train / gridsearch).")
        return

    for ckpt in ckpts:
        overrides_file = ckpt.with_suffix(".overrides")
        run_overrides = overrides_file.read_text().split() if overrides_file.exists() else []
        print(f"\n=== Evaluating {ckpt.name} ===")
        _run(
            c,
            "code/train.py",
            overrides=" ".join([*run_overrides, "eval_only=true", f"eval_checkpoint={ckpt}"]),
        )


@task
def report(c):
    """Generate reports/results_report.html from saved_models/gridsearch_comparison.csv."""
    _run(c, "reports/gen_report.py")


# ── Grid search ──────────────────────────────────────────────────────────────
#
# The final experimental grid is 4 losses x 2 global-feature encoders x 3
# static-feature encoders = 24 configs (see README.md and CODING_AGENT_PROMPT.md
# for the research-question mapping). The naming/skip/CSV-merge behaviour for
# this task is being finished in a later phase of the current cleanup; for now
# `--dry-run` reports the planned 24 configs so the grid definition itself can
# be reviewed independently of the execution/merge logic.

_GRID_LOSSES = [
    ("mse", ["model.loss_fn=mse"]),
    ("weighted_mse_w1", ["model.loss_fn=weighted_mse", "model.weighted_loss_mode=hinge", "model.drought_weight=1"]),
    ("weighted_mse_w5", ["model.loss_fn=weighted_mse", "model.weighted_loss_mode=hinge", "model.drought_weight=5"]),
    ("pinball", ["model.loss_fn=pinball", "model.pinball_quantile=0.2"]),
]
_GRID_GLOBAL = ["naive", "film"]
_GRID_STATIC = ["naive", "single", "seasonal"]


def _grid_configs(loss: str, static_encoder: str, global_encoder: str):
    losses = _GRID_LOSSES if loss == "all" else [p for p in _GRID_LOSSES if p[0] == loss]
    statics = _GRID_STATIC if static_encoder == "all" else [static_encoder]
    globals_ = _GRID_GLOBAL if global_encoder == "all" else [global_encoder]
    if not losses:
        raise ValueError(f"Unknown --loss {loss!r}. Choose one of {[n for n, _ in _GRID_LOSSES]} or 'all'.")
    for static in statics:
        for glob in globals_:
            for loss_name, loss_overrides in losses:
                tag = f"{loss_name}_{static}_{glob}"
                overrides = [
                    *loss_overrides,
                    f"model.static_encoder={static}",
                    f"model.global_encoder={glob}",
                    f"run_name={tag}",
                    f"logger.run_name={tag}",
                ]
                yield tag, overrides


@task
def gridsearch(c, loss="all", static_encoder="all", global_encoder="all", dry_run=False):
    """Sweep the 24-config grid (4 losses x 2 global encoders x 3 static encoders).

    --loss / --static-encoder / --global-encoder accept 'all' (default) or a
    single value, so a subset of the grid can be run at a time.
    """
    configs = list(_grid_configs(loss, static_encoder, global_encoder))
    print(f"Planned runs: {len(configs)}")
    for tag, overrides in configs:
        print(f"  {tag:35s}  {' '.join(overrides)}")

    if dry_run:
        return

    raise NotImplementedError(
        "gridsearch execution (checkpoint-skip detection + WandB metric merge into "
        "saved_models/gridsearch_comparison.csv) is finished in a later phase of the "
        "current cleanup. Use --dry-run to inspect the planned grid, or `invoke train "
        "--overrides '...'` to run a single config now."
    )
