# Long Term Drought prediction using Deep Neural Networks

Spatiotemporal drought forecasting for the Alpine domain with a Convolutional LSTM (ConvLSTM),
trained on ERA5-Land reanalysis (1971–2024). Predicts SPEI-1 at a 12-month lead on the EUR-11
rotated-pole grid (43×98 cells).

## Setup

Requires Python ≥3.12 and [uv](https://docs.astral.sh/uv/). CPU is fine, no GPU needed.

```bash
cd work/02-drought_prediction
uv sync                 # install dependencies (creates .venv)
uv run invoke --list    # list every available command
uv run invoke smoke     # quick end-to-end test on a synthetic fixture (~20s, no dataset needed)
```

Commands resolve their own paths, so they work from this folder or from the GitBook repo root.

**Data.** The processed dataset (~327 MB) is supplied externally and not committed. If
`data/processed/` is already present, nothing to do. Otherwise point at it:

```bash
export DROUGHT_DATA_DIR=/path/to/processed
export DROUGHT_RAW_DIR=/path/to/raw     # only needed for `preprocess` / `visualize`
```

Missing files fail immediately with a message naming the path and the env var to set.

## Running the code

```bash
uv run invoke preprocess     # build data/processed/ from data/raw/
uv run invoke train          # single training run, default config
uv run invoke baselines      # persistence / climatology / trend baselines
uv run invoke evaluate       # evaluate all checkpoints in saved_models/
uv run invoke report         # self-contained HTML results report
```

Override any hyperparameter via Hydra:

```bash
uv run invoke train --overrides "model.global_encoder=film model.loss_fn=pinball"
```

**Without a Weights & Biases account.** Add `logger=csv` to run fully locally (writes
`epoch_metrics.csv` / `test_metrics.json`, nothing sent to WandB). `invoke smoke` and
`invoke evaluate` already do this.

```bash
uv run invoke train --overrides "logger=csv"
```

**The full 24-configuration grid** (4 losses × 2 global × 3 static encoders):

```bash
uv run invoke gridsearch --dry-run     # preview the planned runs without running
uv run invoke gridsearch               # run all; keeps the best checkpoint per cell
uv run invoke gridsearch --loss mse    # a subset (see `invoke gridsearch --help`)
```

**Interpretability** (on existing checkpoints, test period only, no retraining):

```bash
uv run invoke feature-importance   # occlusion feature importance
uv run invoke film-extremes        # does FiLM help the drought tail
```

## Where to look for more

- **Config**: everything is Hydra-driven and overridable at the command line — see `configs/` for defaults.
- **Methods, data schema, model architecture, evaluation, results**: `reports/notes.md`.

## Project structure

```
drought_prediction/
├── code/
│   ├── preprocessing.py      # Build data/processed/ from raw files
│   ├── visualization.py      # Save figures to figures/
│   ├── dataset.py            # PyTorch Dataset, feature assembly, OOT split
│   ├── model.py              # RCNNModule (ConvLSTM), loss classes, metric logging
│   ├── train.py              # Hydra-driven training entry point
│   ├── eval_baselines.py     # Standalone baseline evaluator (run once via inv baselines)
│   ├── interpretability.py   # Eval-time ablation harness + feature-importance/film-extremes analyses
│   └── utils/
│       ├── conv_block.py     # Conv2d + BatchNorm2d building block
│       ├── metrics.py        # Per-cell RMSE / Pearson r / AUROC / F1 / TPR
│       ├── plotting.py       # Spatial heatmap and forecast plot helpers
│       ├── paths.py          # Shared data-root resolution (env vars, fail-early checks)
│       ├── make_fixture.py   # Tiny synthetic processed_dir for `invoke smoke`
│       └── timing.py         # EpochTimer callback (per-epoch wall-clock logging)
├── configs/
│   ├── config.yaml           # Root config (composes the groups below)
│   ├── data/default.yaml     # Paths, lead time, split years, feature flags
│   ├── model/default.yaml    # Architecture and loss hyperparameters
│   ├── trainer/default.yaml  # Max epochs, early stopping, LR scheduler
│   └── logger/               # WandB and CSV logger configs
├── reports/
│   ├── generate/             # HTML intermediate-report + diagnostic generators (reports, trend baseline, loss figures)
│   ├── interpretability/     # Feature-importance / FiLM-extremes CSVs
│   ├── figures/              # Loss-concept figures
│   └── notes.md              # Research notes and findings
├── figures/
│   ├── baselines/            # One subfolder per baseline (persistence/, climatology/, trend/)
│   ├── interpretability/     # Feature-importance and FiLM-extremes figures
│   └── models/               # One subfolder per evaluated model run
├── saved_models/
│   ├── gridsearch_comparison.csv   # Full metric table (192 candidate runs)
│   └── best_model_*.ckpt           # 24 winning checkpoints (one per grid cell)
└── tasks.py                  # Invoke task runner (gridsearch, compare, evaluate, baselines, …)
```
