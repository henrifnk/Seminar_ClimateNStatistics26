# Alpine Drought Prediction — ConvLSTM with Spatial and Global Conditioning

Spatiotemporal drought forecasting for the Alpine domain using a Convolutional LSTM (ConvLSTM; Shi et al. 2015) trained on ERA5 reanalysis data (1971–2024). The model predicts the **Standardised Precipitation-Evapotranspiration Index (SPEI)** directly at a 12-month lead on a rotated-pole grid (EUR-11 CORDEX, 43×98 cells).

---

## Research questions

| ID | Question | Architectural manipulation |
|---|---|---|
| **RQ0** | How strong is the nonstationarity signal and what is the exploitable residual variance? | Baselines only (persistence, climatology, linear trend) |
| **RQ1.1** | Does a learned spatial encoder for topography outperform naive channel injection? | Static encoder: `naive` (channel injection) vs `single` (shared CNN) vs `seasonal` (4×season CNNs) |
| **RQ1.2** | Does FiLM conditioning on large-scale climate indices outperform naive channel injection? | Global encoder: `naive` (channel injection) vs `film` (FiLM-MLP) |
| **RQ2** | Which loss function best balances RMSE skill against drought-event recall? | Five loss families: MSE, Pinball (q=0.20), Cellwise Pinball, Weighted MSE (threshold / hinge / absolute) |
| **RQ3** | Does the full architecture (seasonal + FiLM) outperform the naive baseline? | Ablation across all six architecture conditions |

---

## Project structure

```
drought_prediction/
├── code/
│   ├── preprocessing.py      # Build data/processed/ from raw NetCDF files
│   ├── visualization.py      # Save cartopy map figures to figures/
│   ├── dataset.py            # PyTorch Dataset, feature assembly, OOT split
│   ├── model.py              # RCNNModule (ConvLSTM), loss classes, metric logging
│   ├── train.py              # Hydra-driven training entry point
│   ├── eval_baselines.py     # Standalone baseline evaluator (run once via inv baselines)
│   └── utils/
│       ├── conv_block.py     # Conv2d + BatchNorm2d building block
│       ├── metrics.py        # Per-cell RMSE / Pearson r / AUROC / F1 / TPR
│       └── plotting.py       # Spatial heatmap and forecast plot helpers
├── configs/
│   ├── config.yaml           # Root config (composes the three groups below)
│   ├── data/default.yaml     # Paths, lead time, split years, feature flags
│   ├── model/default.yaml    # Architecture and loss hyperparameters
│   ├── trainer/default.yaml  # Max epochs, early stopping, LR scheduler
│   └── logger/               # WandB and CSV logger configs
├── reports/
│   ├── gen_report.py         # Self-contained HTML report generator
│   ├── trend_baseline.py     # Per-cell OLS trend fit and nonstationarity audit
│   └── notes.md              # Research notes and findings
├── figures/
│   ├── baselines/            # One subfolder per baseline: persistence/, climatology/, trend/
│   │   └── {name}/           # test_rmse_spatial.png, test_corr_spatial.png,
│   │                         # test_drought_{acc,f1,tpr}_spatial.png, test_metrics.json
│   └── models/               # One subfolder per evaluated model run
│       └── {run_tag}/        # Same 5 figures + test_metrics.json per run
├── saved_models/
│   ├── gridsearch_comparison.csv   # Full metric table (432 runs × 34 columns)
│   └── best_model_*.ckpt           # Best checkpoint per loss-mode × condition
└── tasks.py                  # Invoke task runner (gridsearch, compare, evaluate, baselines, …)
```

---

## Data

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

### Forecasting strategy — direct single-horizon

The model performs **direct multi-step forecasting**: given a fixed-length input window it predicts SPEI at exactly one target horizon without iterating through intermediate steps (cf. Weigend & Gershenfeld 1994 for the direct vs recursive distinction).

```
Input window:  x[t-35], x[t-34], …, x[t]     (history_length = 36 months)
                                       ↕  lead_time = 12 months
Target:        SPEI[t + 12]                    (single spatial map)
```

### Input tensor

```
x  shape: (B, C_in, H=44, W=98)
```

Channel layout — dynamic variables only; static and global scalars enter via separate modules:

```
Dynamic (always):
  spei[t-35 … t]      ← 36 channels
  wb  [t-35 … t]      ← 36 channels
  pr  [t-35 … t]      ← 36 channels         5 × 36 = 180 channels total
  ps  [t-35 … t]      ← 36 channels
  tas [t-35 … t]      ← 36 channels

In naive-injection mode (global_encoder='naive' or static_encoder='naive'):
  global scalars broadcast to (H, W) and appended as extra channels
  static features appended as 2 constant channels
```

SPEI is placed first so that `x[:, 35]` (last input timestep) directly gives the persistence baseline.

### Forward pass

```
x  (B, 180, 44, 98)
│
├─ Dropout2d(p)
│
├─ Embedding  [Conv2d+BN → ReLU → Conv2d+BN]
│   └─ x_emb  (B, emb_size=16, 44, 98)
│
├─ [FiLM conditioning — global_encoder='film']
│   ├─ FiLMGenerator MLP: scalars_t (B, 6) → γ, β  (B, emb_size, 1, 1)
│   └─ x_emb ← γ · x_emb + β           (feature-wise affine modulation)
│
├─ [Static encoder — static_encoder∈{'single','seasonal'}]
│   ├─ 'single':   one CNN on (orogf, mask) → static_feat (B, 16, 44, 98)
│   └─ 'seasonal': four CNNs (DJF/MAM/JJA/SON), selected by target month
│                  via batched gather — no Python loop over samples
│   (Eval caching: encoder runs once per epoch; result reused across batches)
│
├─ Concat: [h_{t-1} ‖ x_emb ‖ static_feat?]   (B, hid+emb[+16], 44, 98)
│
├─ ConvLSTM gates  (each: ConvBlock → activation)
│   f_t = σ(Conv([h, x]))    forget gate
│   i_t = σ(Conv([h, x]))    input gate
│   c̃_t = tanh(Conv([h, x])) candidate cell
│   o_t = σ(Conv([h, x]))    output gate
│   c_t = f_t ⊙ c_{t-1} + i_t ⊙ c̃_t
│   h_t = tanh(c_t) ⊙ o_t
│
└─ Output head
    Conv2d(hid_size=32 → 1, k=1) → ScaledTanh(×10) → (B, 1, 44, 98)
```

Hidden and cell states are carried across sequential batches and detached after each backward pass (truncated BPTT). The DataLoader must not shuffle batches.

### Optional modules

**Static spatial encoder** (`model.static_encoder`):
- `none`: disabled (no topography conditioning)
- `naive`: raw static features (orogf, mask) broadcast-appended to `x` as 2 constant channels
- `single`: one shared two-layer CNN. Encodes `(orogf, mask)` → `(B, 16, 44, 98)` and concatenated to `[h, x_emb]` before the LSTM gates. During eval/test the result is computed once and cached for all batches in the epoch (weights and static map are frozen).
- `seasonal`: four independent CNNs, one per meteorological season (DJF/MAM/JJA/SON). All four are run on the batch and the correct output is gathered by target month index, keeping the forward pass fully batched. Eval caching stores all four outputs as `(4, emb, H, W)` and indexes per sample.

**Global scalar FiLM conditioning** (`model.global_encoder`):
- `none`: disabled (no global scalar conditioning)
- `naive`: global scalars broadcast to `(H, W)` and appended as extra channels in `x`
- `film`: Feature-wise Linear Modulation (Perez et al. 2018). A two-layer MLP maps the 6 global scalars at the last input timestep to per-channel scale γ and shift β, which are applied element-wise to the embedding output `x_emb` before the LSTM gates.

### Loss functions

All losses are masked to valid Alpine cells before averaging.

| Name | `loss_fn` | Formula | Notes |
|---|---|---|---|
| MSE | `mse` | (ŷ−y)² | Unconditional regression baseline |
| Pinball | `pinball` | max(q(y−ŷ), (q−1)(y−ŷ)) | q=0.20; missing a drought (predicting too high) penalised 4× more than a false alarm — biases model toward the drought tail |
| Cellwise Pinball | `cellwise_pinball` | Pinball with per-cell quantile | q[i,j] = empirical P(SPEI[i,j] ≤ −1.5) over training; drought-prone cells receive lower q automatically |
| Weighted MSE (threshold) | `weighted_mse` + `threshold` | w · (ŷ−y)², w = drought_weight below threshold, 1 elsewhere | Binary upweight; drought_weight ∈ {1, 5} |
| Weighted MSE (absolute) | `weighted_mse` + `absolute` | \|y\| · (ŷ−y)² | Scales with SPEI severity on both tails (cf. Ravuri et al. 2021 intensity weighting); drought_weight unused |
| Weighted MSE (hinge) | `weighted_mse` + `hinge` | (1 + drought_weight · max(0, −y)) · (ŷ−y)² | Asymmetric: weight=1 for y>0, linearly increasing for y<0. At drought_weight=1, SPEI=−1.5 → weight=2.5, SPEI=−2.0 → weight=3.0 |

---

## Training protocol

### Out-of-time split

Splits are defined by the **target year** (not the input window year) to prevent look-ahead leakage from the 36-month input.

| Split | Target years | Samples (~) |
|---|---|---|
| Train | 1971–2005 | 396 |
| Validation | 2006–2014 | 108 |
| Test | 2015–2024 | 120 |

### Optimiser and scheduling

- Optimiser: Adam with weight decay (decoupled)
- Learning rate halved after 5 epochs without `val/loss` improvement (ReduceLROnPlateau)
- Early stopping at patience 20 on `val/loss`
- Maximum 100 epochs; deterministic mode enabled (cuDNN seed fixed)

### Checkpoint selection

`ModelCheckpoint` monitors **`val/loss`** (the training criterion). For `weighted_mse`, the three modes (threshold / hinge / absolute) optimise incommensurable objectives and are not comparable on a common val/loss scale. Consequently, checkpoint selection for `weighted_mse` is performed **within each mode separately**, retaining one checkpoint per mode per condition. For all other losses one checkpoint is retained per loss per condition.

### Baselines (data-derived, evaluated once)

Baselines are not model-dependent and are evaluated once via `inv baselines`, writing figures and metrics to `figures/baselines/{persistence,climatology,trend}/`.

| Baseline | Definition | Test RMSE | Test TPR | Test F1 |
|---|---|---|---|---|
| **Persistence** | Predict the last observed SPEI value (x[t], 12 months before target) for every cell | 1.3489 | 0.2811 | 0.2898 |
| **Climatology** | Predict the training-period cell mean for every test timestep | 1.0758 | 0.0 | 0.0 |
| **Linear trend** | Per-cell OLS regression on training targets; extrapolated to test period | 1.0659 | 0.0 | 0.0 |

Climatology and linear trend never predict SPEI ≤ −1.5 (drought threshold) because both produce near-zero or slowly drifting values that do not reach the drought tail — hence TPR = 0 (confirmed empirically). Persistence achieves non-trivial TPR by repeating the last observed drought state.

The **residual floor** (median per-cell test-period std after detrending the training-period trend) is ~1.06 in normalised SPEI units; `skill_margin = residual_floor − model_RMSE` is positive when a model's RMSE falls below this irreducible variance floor.

---

## Experimental design

The full grid crosses **6 architecture conditions** × **6 loss families** × 8–16 hyperparameter combos = **432 runs** total.

### Architecture conditions

| ID | `static_encoder` | `global_encoder` | Description |
|---|---|---|---|
| naive/naive | naive | naive | Fully naive-injection baseline |
| single/naive | single | naive | Non-seasonal topography CNN; naive global |
| seasonal/naive | seasonal | naive | Seasonal topography CNN; naive global — RQ1.1 |
| naive/film | naive | film | FiLM conditioning; naive static — RQ1.2 |
| single/film | single | film | Non-seasonal CNN + FiLM |
| seasonal/film | seasonal | film | Full architecture — RQ3 |

### Loss families

| Family | Modes swept |
|---|---|
| `mse` | — |
| `pinball` | q=0.20 fixed |
| `cellwise_pinball` | — |
| `weighted_mse` | threshold × {w=1, w=5}, hinge × {w=1, w=5}, absolute × {w=1, w=5} |

### Hyperparameter axes (gridsearch)

| Axis | Values |
|---|---|
| `model.lr` | 1×10⁻³, 3×10⁻³ |
| `model.dropout` | 0.0, 0.1 |
| `model.weight_decay` | 0.0, 1×10⁻⁴ |
| `model.drought_weight` *(weighted_mse only)* | 1.0, 5.0 |
| `model.weighted_loss_mode` *(weighted_mse only)* | threshold, hinge, absolute |

---

## Evaluation framework

All metrics are pooled over valid masked Alpine cell-months (a single number per run computed from a global confusion matrix or error sum, not a per-cell average of cell-level numbers). Per-cell spatial medians are also stored but are secondary; the pooled values are the headline numbers.

### Primary metrics (test period)

All regression metrics operate on **normalised SPEI** (z-scored against training-period statistics).

| Metric | WandB key | Definition |
|---|---|---|
| RMSE | `test/rmse_pooled` | Pooled RMSE over all valid cell-months |
| Pearson r | `test/corr_pooled` | Pooled linear correlation |
| Drought TPR | `test/drought_tpr_pooled` | Recall at SPEI ≤ −1.5, pooled global confusion matrix |
| Drought F1 | `test/drought_f1_pooled` | F1 at SPEI ≤ −1.5, pooled global confusion matrix |
| Drought ROC-AUC | `test/drought_rocauc_median` | Spatial median of per-cell AUROC (Wilcoxon rank-sum formula) |
| RMSE vs trend | `test/rmse_vs_trend` | trend_rmse − model_rmse; positive = model beats linear-trend baseline |
| Skill margin | `test/residual_floor` − `test/rmse_pooled` | Positive when model RMSE is below the irreducible variance floor |

Baseline pooled metrics are logged per-run under `test/{persistence,clim,trend}/{rmse,drought_tpr,drought_f1}_pooled` and stored in the comparison CSV. Spatial heatmaps for baselines live in `figures/baselines/` (generated once, not per-model).

### Checkpoint saving vs. run selection

- **Checkpoint saved by**: `val/loss` (the training criterion, monitored by `ModelCheckpoint`)
- **Best run selected by**: `val/drought_f1_pooled` (pooled F1 over the validation set, preferred over per-cell median F1 because Alpine drought events are spatially clustered and validation coverage is limited to ~108 months)

### Loss family selection

For selecting the best loss family across the full grid (`tasks.py → select_loss`):
1. Aggregate all runs per family by computing the median `val/drought_f1_pooled` and median `val/clim/rmse_median`
2. Gate on RMSE: families whose median val RMSE exceeds the climatology baseline by more than a slack threshold are excluded
3. Select the family with highest median pooled val F1 among those passing the gate

---

## Nonstationarity diagnostics (`reports/trend_baseline.py`)

Test-period (2015–2024) drought frequency is approximately **2× higher** than training-period (1971–2005) frequency (median across valid cells: ~6% training → ~12% test). The test-period mean SPEI is shifted negative relative to the training mean, inflating the climatology RMSE because the climatology predicts the training mean.

A per-cell OLS linear trend fitted on training targets and extrapolated to the test period (linear trend baseline) barely reduces RMSE compared to climatology (1.0659 vs 1.0758), confirming that the nonstationarity is not a smooth monotonic drift well-captured by linear extrapolation. Trend TPR = 0 (confirmed: the linear extrapolation never crosses the SPEI ≤ −1.5 drought threshold during the test period).

The residual floor is ~1.06 in normalised SPEI units. Fewer than 40% of the 432 grid runs achieve a positive skill margin; models that do tend to have near-zero TPR, while models with non-trivial TPR all fall below the residual floor.

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
| `val_from_year` | 2006 | First target year of validation period |
| `test_from_year` | 2015 | First target year of test period |
| `batch_size` | 4 | Samples per batch |

**`configs/model/default.yaml`**

| Key | Default | Description |
|---|---|---|
| `embedding_size` | 16 | Channels after the embedding block |
| `hidden_state_size` | 32 | ConvLSTM hidden and cell state channels |
| `kernel_size` | 3 | Convolution kernel size throughout |
| `dropout` | 0.2 | Spatial dropout rate (Dropout2d) |
| `lr` | 3×10⁻³ | Adam learning rate |
| `weight_decay` | 0.1 | Adam weight decay |
| `loss_fn` | `mse` | `mse` / `pinball` / `cellwise_pinball` / `weighted_mse` |
| `pinball_quantile` | 0.20 | Quantile for pinball loss |
| `drought_threshold` | −1.5 | SPEI threshold for severe drought |
| `drought_weight` | 5.0 | Upweight slope for weighted_mse (threshold: binary multiplier; hinge: slope of linear ramp) |
| `weighted_loss_mode` | `threshold` | `threshold` / `hinge` / `absolute` |
| `static_encoder` | `none` | `none` / `naive` / `single` / `seasonal` |
| `static_emb_size` | 16 | Output channels of the static encoder CNN(s) |
| `global_encoder` | `none` | `none` / `naive` / `film` |
| `film_hidden_size` | 64 | Hidden units in the FiLM generator MLP |

**`configs/trainer/default.yaml`**

| Key | Default |
|---|---|
| `max_epochs` | 100 |
| `early_stopping_patience` | 20 |
| `lr_scheduler_patience` | 5 |
| `deterministic` | true |

---

## Quickstart

```bash
# Install dependencies
uv sync

# Build processed data
uv run invoke preprocess

# Print the fully resolved config
uv run invoke cfg

# Single training run with default config
uv run invoke train

# Override hyperparameters via Hydra
uv run invoke train --overrides "model.global_encoder=film model.loss_fn=weighted_mse model.weighted_loss_mode=hinge"

# Evaluate persistence, climatology, and linear-trend baselines (once per dataset)
uv run invoke baselines
```

---

## Grid search

`invoke gridsearch` sweeps all hyperparameter combinations for one loss and one architecture condition, selects the best checkpoint per mode (within-mode for `weighted_mse`, single best for all other losses), and merges all run metrics into `saved_models/gridsearch_comparison.csv` without overwriting results from other conditions.

```bash
# Single condition, single loss
uv run invoke gridsearch --loss weighted_mse --static-encoder naive --global-encoder film

# All losses for one condition
uv run invoke gridsearch --loss all --static-encoder seasonal --global-encoder film

# Dry run — print commands without executing
uv run invoke gridsearch --loss all --dry-run
```

**Flags**

| Flag | Options | Default |
|---|---|---|
| `--loss` | `mse` / `pinball` / `cellwise_pinball` / `weighted_mse` / `all` | `pinball` |
| `--static-encoder` | `none` / `naive` / `single` / `seasonal` | `none` |
| `--global-encoder` | `none` / `naive` / `film` | `none` |
| `--dry-run` | — | false |

Run names encode all active hyperparameters (e.g. `weighted_mse_w1_hinge_naive_film_lr3e-03_do0.0_wd0e+00`) and are used as WandB display names, making each run directly identifiable in the comparison CSV. Existing checkpoints are detected by name and skipped without re-training; their metrics are included in the comparison pool.

---

## Fetch metrics from WandB without re-training

```bash
# Re-fetch metrics for all checkpoints currently in saved_models/
uv run invoke compare
```

Reads run names from `saved_models/best_model_*.ckpt` filenames, queries WandB for their metrics at the best-val/loss epoch (val metrics) and final summary (test metrics), and writes `saved_models/gridsearch_comparison.csv` sorted by `test/drought_rocauc_median` descending.

---

## Evaluate saved models

```bash
# Evaluate all best_model_*.ckpt files in saved_models/
uv run invoke evaluate

# Evaluate a single checkpoint
uv run invoke evaluate --checkpoint saved_models/best_model_mse_naive_naive_lr3e-03_do0.0_wd0e+00.ckpt
```

Requires a `<name>.overrides` file alongside each checkpoint (created automatically by the gridsearch). Figures are written to `figures/models/<tag>/`.

---

## Report generation

```bash
# Generate reports/results_report.html (self-contained, all figures embedded as base64)
uv run python reports/gen_report.py

# Run nonstationarity / trend-baseline audit
uv run python reports/trend_baseline.py
```

The report reads `saved_models/gridsearch_comparison.csv` and `figures/models/*/` and requires no other dependencies beyond the standard library. Baseline figures are read from `figures/baselines/`.
