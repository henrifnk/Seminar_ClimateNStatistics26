# Alpine Drought Prediction — ConvLSTM with Spatial and Global Conditioning

Spatiotemporal drought forecasting for the Alpine domain using a Convolutional LSTM (ConvLSTM; Shi et al. 2015) trained on ERA5 reanalysis data (1971–2024). The model predicts the **Standardised Precipitation-Evapotranspiration Index (SPEI)** directly at a 12-month lead on a rotated-pole grid (EUR-11 CORDEX, 43×98 cells).

---

## Setup and run

Requires Python ≥3.12 and [uv](https://docs.astral.sh/uv/). CPU is fine (`trainer.accelerator: auto`
also picks up MPS on Mac / CUDA on a GPU cluster automatically); nothing here requires a GPU.

```bash
cd work/02-drought_prediction   # if not already there

# Install dependencies (creates .venv automatically)
uv sync

# See every available command
uv run invoke --list

# Try something safe and fast first (no real dataset needed, ~10-20s on CPU)
uv run invoke smoke
```

All commands work the same way regardless of whether your shell's cwd is this folder or the GitBook
repo root — every `invoke` task resolves its own paths from `tasks.py`'s location, not from cwd.

**Data:** `data/processed/` and `data/raw/` are supplied externally and are never committed (see
[Data root & paths](#data-root--paths-codeutilspathspy)) — if they're already present in this
folder, no further setup is needed. Otherwise, point at them first:

```bash
export DROUGHT_DATA_DIR=/path/to/processed
export DROUGHT_RAW_DIR=/path/to/raw   # only needed for `invoke preprocess` / `invoke visualize`
```

### Core commands

```bash
uv run invoke preprocess              # build data/processed/ from data/raw/
uv run invoke cfg                     # print the fully resolved Hydra config
uv run invoke train                   # single training run with the default config

# Override hyperparameters via Hydra
uv run invoke train --overrides "model.global_encoder=film model.loss_fn=weighted_mse model.drought_weight=5.0"

uv run invoke baselines               # persistence / climatology / trend baselines (once per dataset)
```

### Grid search — the 24-cell final grid

`invoke gridsearch` sweeps 4 losses × 2 global encoders × 3 static encoders = **24 cells**. Within
each cell, `lr` × `dropout` × `weight_decay` (2×2×2 = 8 combos) are swept and only the single best
checkpoint (lowest `val/loss`) is kept — so a full run is 24 × 8 = 192 candidate training runs
producing 24 surviving checkpoints. Checkpoints that already exist (by exact run name, including
their HP + seed) are skipped and still count as a candidate.

```bash
# Preview the full grid without running anything (192 planned commands, grouped by cell)
uv run invoke gridsearch --dry-run

# Everything
uv run invoke gridsearch

# A subset: one loss, all static/global combinations
uv run invoke gridsearch --loss mse

# A single cell
uv run invoke gridsearch --loss weighted_mse_w1_hinge --static-encoder seasonal --global-encoder film
```

**Flags**

| Flag | Options | Default |
|---|---|---|
| `--loss` | `mse` / `weighted_mse_w1_hinge` / `weighted_mse_w5_hinge` / `pinball_q0.20` / `all` | `all` |
| `--static-encoder` | `naive` / `single` / `seasonal` / `all` | `all` |
| `--global-encoder` | `naive` / `film` / `all` | `all` |
| `--seed` | any int | `42` |
| `--dry-run` | — | false |

Winning run names encode the loss, condition, and winning hyperparameters (e.g.
`mse_naive_film_lr3e-03_do0.0_wd1e-04_s42`) and are used as WandB display names. After all requested
cells complete, metrics for the winners are fetched from WandB into
`saved_models/gridsearch_comparison.csv` (merged, not clobbered) and each winner is evaluated
(figures written once).

### Fetch metrics from WandB without re-training

```bash
# Re-fetch metrics for all checkpoints currently in saved_models/
uv run invoke compare
```

Reads run names from `saved_models/best_model_*.ckpt` filenames, queries WandB for their metrics at
the best-`val/loss` epoch (val metrics) and final summary (test metrics), and writes
`saved_models/gridsearch_comparison.csv` sorted by `test/drought_f1_pooled` descending. `compare` and
a non-dry-run `gridsearch` both need a WandB login first: `uv run wandb login`.

### Evaluate saved models

```bash
# Evaluate all best_model_*.ckpt files in saved_models/
uv run invoke evaluate

# Evaluate a single checkpoint
uv run invoke evaluate --checkpoint saved_models/best_model_mse_naive_film_lr3e-03_do0.0_wd1e-04_s42.ckpt
```

Requires a `<name>.overrides` file alongside each checkpoint (written automatically by `train.py` /
`gridsearch`). Figures are written to `figures/models/<tag>/`; checkpoints whose figures already
exist are skipped unless `--force` is passed.

### Report generation

```bash
uv run invoke report                                 # reports/output/results_report.html (self-contained, figures embedded as base64)
uv run python reports/generate/trend_baseline.py     # nonstationarity / trend-baseline audit
```

The report reads `saved_models/gridsearch_comparison.csv` and `figures/models/*/`. Baseline figures
are read from `figures/baselines/`.

---

## Research questions

| ID | Question | Architectural manipulation |
|---|---|---|
| **RQ1** | How does the choice of loss function affect drought detection in a regression setting? | Loss (4): MSE, Pinball (q=0.20), Weighted MSE — hinge, `drought_weight` ∈ {1, 5} |
| **RQ2** | How can dynamic global features be integrated into the ConvLSTM architecture? | Global encoder (2): `naive` (channel injection) vs `film` (FiLM-MLP) |
| **RQ3** | How can static spatial features be integrated into the ConvLSTM architecture? | Static encoder (3): `naive` (channel injection) vs `single` (shared CNN) vs `seasonal` (4×season CNNs) |

Baselines (persistence, climatology, linear trend) frame the nonstationarity/residual-variance context
these questions are asked in — see
[Baselines](reports/notes.md#baselines-data-derived-evaluated-once) and
[Nonstationarity diagnostics](#nonstationarity-diagnostics-reportsgeneratetrend_baselinepy).

---

## Project structure

```
drought_prediction/
├── code/
│   ├── preprocessing.py      # Build data/processed/ from raw files
│   ├── visualization.py      # Save cartopy map figures to figures/
│   ├── dataset.py            # PyTorch Dataset, feature assembly, OOT split
│   ├── model.py              # RCNNModule (ConvLSTM), loss classes, metric logging
│   ├── train.py              # Hydra-driven training entry point
│   ├── eval_baselines.py     # Standalone baseline evaluator (run once via inv baselines)
│   ├── make_fixture.py       # Tiny synthetic processed_dir for `invoke smoke`
│   └── utils/
│       ├── conv_block.py     # Conv2d + BatchNorm2d building block
│       ├── metrics.py        # Per-cell RMSE / Pearson r / AUROC / F1 / TPR
│       ├── plotting.py       # Spatial heatmap and forecast plot helpers
│       ├── paths.py          # Shared data-root resolution (env vars, fail-early checks)
│       └── timing.py         # EpochTimer callback (per-epoch wall-clock logging)
├── configs/
│   ├── config.yaml           # Root config (composes the three groups below)
│   ├── data/default.yaml     # Paths, lead time, split years, feature flags
│   ├── model/default.yaml    # Architecture and loss hyperparameters
│   ├── trainer/default.yaml  # Max epochs, early stopping, LR scheduler
│   └── logger/               # WandB and CSV logger configs
├── reports/
│   ├── generate/
│   │   ├── gen_report.py           # Self-contained HTML report generator
│   │   ├── gen_arch_summary.py     # Baseline + architecture-sweep HTML summary
│   │   ├── trend_baseline.py       # Per-cell OLS trend fit and nonstationarity audit
│   │   └── loss_illustrations.py   # Loss-function concept figures for the GitBook chapter
│   ├── figures/               # mse.pdf, pinball.pdf, hinge_loss.pdf, hinge_weight.pdf — used in the paper
│   ├── output/                # Generated HTML reports (gitignored, not committed)
│   └── notes.md                # Research notes and findings
├── figures/
│   ├── baselines/            # One subfolder per baseline: persistence/, climatology/, trend/
│   │   └── {name}/           # test_rmse_spatial.png, test_corr_spatial.png,
│   │                         # test_drought_{acc,f1,tpr}_spatial.png, test_metrics.json
│   └── models/               # One subfolder per evaluated model run
│       └── {run_tag}/        # Same 5 figures + test_metrics.json per run
├── saved_models/
│   ├── gridsearch_comparison.csv   # Full metric table (192 candidate runs × columns)
│   └── best_model_*.ckpt           # 24 winning checkpoints (one per grid cell)
└── tasks.py                  # Invoke task runner (gridsearch, compare, evaluate, baselines, …)
```

---

## Data

The processed dataset (~327 MB) is supplied externally and is **never committed** to this repo.

### Data root & paths (`code/utils/paths.py`)

Every entry point resolves the processed- and raw-data roots the same way, from exactly one
configurable location each, independent of the launching process's cwd:

| Root | Env var | Default (if unset) |
|---|---|---|
| Processed data (consumed by `DroughtDataset`, produced by `preprocessing.py`) | `DROUGHT_DATA_DIR` | `<this folder>/data/processed` |
| Raw NetCDF inputs (consumed by `preprocessing.py`, `visualization.py`) | `DROUGHT_RAW_DIR` | `<this folder>/data/raw` |

Outputs (`checkpoints/`, `saved_models/`, `figures/`) always resolve relative to this project
folder (`work/02-drought_prediction/`), computed from `__file__`, not from wherever the command is
launched — the same `invoke` commands work from the GitBook repo root or from this folder directly.

If the processed-data root or any of its expected files (`dynamic_grid.nc`, `static_grid.nc`,
`global_scalars.nc`, `normalization.json`) is missing, every entry point fails immediately with a
one-line message naming the missing path and the env var to set — no silent fallback. A lightweight
schema/shape check (variable names, dims, mask/grid consistency) also runs at load time.

**Smoke-testing without the real dataset:** `invoke smoke` generates a tiny synthetic fixture
(`code/make_fixture.py` — an 8×8 grid, same 1971–2024 time span so the default split years are
non-empty) and runs the full dataset → model → training → test path on CPU in well under a minute.

### Domain and grid

The **AL domain** uses the EUR-11 CORDEX rotated-pole grid (pole at 39.25°N, 198°E). The 43×98 bounding box covers the Alps; approximately 53% of cells are ocean/non-Alpine (NaN in raw files). `rlat` is zero-padded to 44 in preprocessing so both spatial dimensions are even for the convolutional operations.

### Raw inputs (ERA5 reanalysis, 1970/1971–2024, monthly)

| Variable | Domain | Shape | Description |
|---|---|---|---|
| `spei` | AL | (648, 43, 98) | Target — SPEI drought index |
| `wb` | AL | (648, 43, 98) | Water balance [kg m⁻² s⁻¹] |
| `pr` | AL | (648, 43, 98) | Precipitation [mm day⁻¹] |
| `ps` | AL | (648, 43, 98) | Surface pressure [Pa] |
| `tas` | AL | (648, 43, 98) | Near-surface air temperature [K] |
| `orogf` | EUR-11 | (280, 280) | Orography fraction — cropped to AL in preprocessing |
| `nao_index` | — | (668,) | NAO principal component (monthly) |
| Med SST | — | (670, 14) | Mediterranean SST per basin (monthly) |

### Preprocessing (`code/preprocessing.py`)

| File | Contents | Shape |
|---|---|---|
| `dynamic_grid.nc` | wb, pr, ps, spei, tas — NaN→train-mean fill, rlat padded 43→44 | (T, 44, 98) per variable |
| `static_grid.nc` | `orogf` (topography fraction) + `mask` (1=valid Alpine cell) | (44, 98) |
| `global_scalars.nc` | NAO index + 14 Med SST basins, trimmed to AL time axis | (T,) per variable |
| `normalization.json` | Per-variable training-period mean and std (z-score) | — |

**NaN handling:** non-Alpine cells are filled with the variable's training-period mean *before* normalisation to avoid extreme outlier values after z-scoring (e.g. ps=0 Pa → −13 std dev without fill). The `mask` identifies valid cells; the model restricts loss and all evaluation metrics to masked cells only.

**Mediterranean SST aggregation (`med_sst_agg=grouped`, default):** the 14 raw basin variables are collapsed to 3 regional means (Western Med / Eastern Med / Black Sea) following the Sicily Channel boundary of Millot & Taupier-Letage (2005). Combined with the NAO index and a cyclic month encoding (sin/cos), this yields **6 global scalars** fed to the FiLM generator.

---

## Model architecture (`code/model.py` — `RCNNModule`)

Direct single-horizon ConvLSTM: 36 months of history (`spei`, `wb`, `pr`, `ps`, `tas` stacked as
channels) → one SPEI map 12 months ahead. Static topography and global (NAO + Med SST) features
enter via optional conditioning modules (`static_encoder`: `naive`/`single`/`seasonal`;
`global_encoder`: `naive`/`film`) or naive channel injection. Hidden/cell state is carried across
sequential batches (truncated BPTT); the DataLoader must not shuffle. Three losses: `mse`,
`pinball` (q=0.20), `weighted_mse` (hinge, `drought_weight` ∈ {1, 5}).

See [reports/notes.md](reports/notes.md#model-architecture--technical-reference-codemodelpy--rcnnmodule)
for the full forward-pass diagram, channel layout, optional-module details, and loss formulas.

---

## Training protocol, experimental design & evaluation framework

Out-of-time split (train ≤2004 / val 2005–2014 / test ≥2015), Adam + ReduceLROnPlateau scheduling,
early stopping on `val/rmse_median`, the 24-cell grid definition (4 losses × 2 global encoders ×
3 static encoders, 8 HP combos swept per cell), baseline numbers (persistence/climatology/trend),
and the pooled evaluation metric definitions (RMSE, drought TPR/F1, `rmse_vs_trend`) are documented
in [reports/notes.md](reports/notes.md#training-protocol) — kept there to keep this README focused
on setup and commands.

---

## Nonstationarity diagnostics (`reports/generate/trend_baseline.py`)

Test-period drought frequency is 2.03× higher than training-period frequency, and a per-cell linear
trend barely beats climatology (confirming the nonstationarity isn't a simple linear drift the
model could just extrapolate). See
[reports/notes.md](reports/notes.md#nonstationarity-diagnostics--results-reportsgeneratetrend_baselinepy)
for the full numeric breakdown.

---

## Configuration

Config is managed with [Hydra](https://hydra.cc). All parameters can be overridden at the command line.

**`configs/data/default.yaml`**

| Key | Default | Description |
|---|---|---|
| `history_length` | 36 | Months of input context |
| `lead_time` | 12 | Direct forecast horizon (months) |
| `dynamic_vars` | wb, pr, ps, tas | Input variables alongside SPEI |
| `use_global_scalars` | false | Load NAO + Med SST; auto-enabled with `global_encoder: film` |
| `med_sst_agg` | grouped | `grouped` (3 regional means + NAO + sin/cos month = 6 scalars) or `none` (14 basins + NAO + sin/cos = 17 scalars) |
| `val_from_year` | 2005 | First target year of validation period |
| `test_from_year` | 2015 | First target year of test period |
| `batch_size` | 32 | Samples per batch |

**`configs/model/default.yaml`**

| Key | Default | Description |
|---|---|---|
| `embedding_size` | 16 | Channels after the embedding block |
| `hidden_state_size` | 32 | ConvLSTM hidden and cell state channels |
| `kernel_size` | 3 | Convolution kernel size throughout |
| `dropout` | 0.2 | Spatial dropout rate (Dropout2d) |
| `lr` | 3×10⁻³ | Adam learning rate |
| `weight_decay` | 0.1 | Adam weight decay |
| `loss_fn` | `mse` | `mse` / `pinball` / `weighted_mse` |
| `pinball_quantile` | 0.20 | Quantile for pinball loss |
| `drought_threshold` | −1.5 | SPEI threshold for severe drought |
| `drought_weight` | 5.0 | Upweight slope for weighted_mse (hinge — the only weighting mode implemented; the final grid uses drought_weight 1.0 and 5.0 as two separate losses) |
| `static_encoder` | `none` | `none` / `naive` / `single` / `seasonal` |
| `static_emb_size` | 16 | Output channels of the static encoder CNN(s) |
| `global_encoder` | `none` | `none` / `naive` / `film` |
| `film_hidden_size` | 64 | Hidden units in the FiLM generator MLP |

**`configs/trainer/default.yaml`**

| Key | Default |
|---|---|
| `max_epochs` | 50 |
| `early_stopping_patience` | 5 |
| `early_stopping_monitor` | `val/rmse_median` |
| `lr_scheduler_patience` | 3 |
| `deterministic` | true |

