# Project Notes — Technical Reference

Reference material for the drought-prediction pipeline, kept out of the README so it stays focused on
setup and commands: the model's forward-pass mechanics, loss formulas, the training protocol, the
experimental design, the evaluation framework, the nonstationarity-diagnostic numbers, and the
global-scalar feature engineering.

---

## Model architecture — technical reference (`code/model.py` — `RCNNModule`)

### Forecasting strategy — direct single-horizon

The model performs **direct multi-step forecasting**: given a fixed-length input window it predicts SPEI at exactly one target horizon without iterating through intermediate steps.

```
Input window:  x[t-35], x[t-34], …, x[t]     (history_length = 36 months)
                                       ↕  lead_time = 12 months
Target:        SPEI[t + 12]                    (single spatial map)
```

### Input tensor

```
x  shape: (B, T, n_vars, H=44, W=98)     # T = history_length = 36 monthly frames, oldest first
```

Each frame carries the dynamic variables as channels, SPEI first:

```
Per frame:  [spei_t, wb_t, pr_t, ps_t, tas_t]     (n_vars = 5)

Naive injection appends extra channels to every frame:
  static_encoder='naive'  -> +2 constant channels (orogf, mask)
  global_encoder='naive'  -> +n_global broadcast channels (6 in grouped mode)
```

SPEI is channel 0 of every frame, so the last observed SPEI (the persistence baseline) is `x[:, -1, 0]`.

### Forward pass

The ConvLSTM is unrolled over the 36 input months (the prediction is read from the final hidden state).

```
x  (B, T=36, n_vars, H, W)
│
├─ static_feat = StaticEncoder(...)           # once per forward (single/seasonal) -> (B, 16, H, W)
├─ h, c <- zeros(B, hid=32, H, W)             # fresh zero state every forward
│
└─ for t = 0 … 35:                             # unroll over the history window
     x_t   = Dropout2d(x[:, t])                # (B, n_vars, H, W)
     x_emb = Embedding(x_t)                    # ConvBlock -> ReLU -> ConvBlock -> (B, emb=16, H, W)
     if global_encoder == 'film':
       γ_t, β_t = FiLMGenerator(scalars[:, t]) # this step's own scalars -> (B, 16, 1, 1)
       x_emb    = γ_t · x_emb + β_t
     gate_in = concat([h, x_emb, static_feat?])            # (B, hid+emb[+16], H, W)
     f = σ(Conv(gate_in)); i = σ(Conv(gate_in)); g = tanh(Conv(gate_in)); o = σ(Conv(gate_in))
     c = f ⊙ c + i ⊙ g
     h = tanh(c) ⊙ o
│
└─ Output head (from final h only)
     ŷ = ScaledTanh×10( Conv2d(hid=32 -> 1, k=1, bias=False)(h) )   # (B, 1, H, W)
```

Each gate is a `ConvBlock` (Conv2d + BatchNorm) followed by its activation; the embedding's ConvBlocks include BatchNorm too.

### Optional modules

**Static spatial encoder** (`model.static_encoder`):
- `none`: disabled (no topography conditioning)
- `naive`: raw static features (orogf, mask) broadcast-appended to every input frame as 2 constant channels
- `single`: one shared two-layer CNN encodes `(orogf, mask)` -> `(B, 16, H, W)`, concatenated to the gate input `[h, x_emb]` at every unrolled step. Computed once per forward and reused across the 36 steps.
- `seasonal`: four independent CNNs, one per meteorological season (DJF/MAM/JJA/SON); the one matching the **target** month is selected per sample via a batched gather. Also computed once per forward.

**Global scalar FiLM conditioning** (`model.global_encoder`):
- `none`: disabled
- `naive`: global scalars broadcast to `(H, W)` and appended as extra channels on every frame
- `film`: Feature-wise Linear Modulation (Perez et al. 2018). A two-layer MLP maps the global scalars **at each input step** to per-channel scale γ and shift β, applied to that step's embedding `x_emb` before the gates. Conditioning is therefore per-step over the 36-month window, not from a single timestep.

### Loss functions

All losses are masked to valid Alpine cells before averaging.

| Name | `loss_fn` | Formula | Notes |
|---|---|---|---|
| MSE | `mse` | (ŷ−y)² | Unconditional regression baseline |
| Pinball | `pinball` | max(q(y−ŷ), (q−1)(y−ŷ)) | q=0.20; missing a drought (predicting too high) penalised 4× more than a false alarm — biases model toward the drought tail |
| Weighted MSE (hinge) | `weighted_mse` + `hinge` | (1 + drought_weight · max(0, −y)) · (ŷ−y)² | Asymmetric: weight=1 for y>0, linearly increasing for y<0. `drought_weight` ∈ {1, 5}; at drought_weight=1, SPEI=−1.5 → weight=2.5, SPEI=−2.0 → weight=3.0 |

---

## Training protocol

### Out-of-time split

Splits are defined by the **target year** (not the input window year) to prevent look-ahead leakage from the 36-month input.

| Split | Target years | Samples |
|---|---|---|
| Train | 1974–2004 | 361 |
| Validation | 2005–2014 | 120 |
| Test | 2015–2024 | 120 |

Train starts at target year 1974, not 1971: the earliest possible target needs 36 months of
history before it, so the first ~3 years of the 1971– raw series are consumed as input-only context.

### Optimiser and scheduling

- Optimiser: Adam with weight decay (decoupled)
- Learning rate halved after 3 epochs without `val/loss` improvement (ReduceLROnPlateau)
- Early stopping at patience 5 on `val/rmse_median` (not `val/loss` — chosen so `min_delta` is in
  RMSE units and comparable across loss functions with different scales)
- Maximum 50 epochs; deterministic mode enabled (cuDNN seed fixed)

### Checkpoint selection

`ModelCheckpoint` monitors **`val/loss`** (the training criterion). `weighted_mse` uses hinge weighting only; `drought_weight=1` and `drought_weight=5` are treated as two separate final losses (RQ1), not two values to select between — each keeps its own checkpoint. One checkpoint is retained per loss per condition.

### Baselines (data-derived, evaluated once)

Baselines are not model-dependent and are evaluated once via `uv run invoke baselines`, writing figures and metrics to `figures/baselines/{persistence,climatology,trend}/`.

| Baseline | Definition | Test RMSE | Test TPR | Test F1 |
|---|---|---|---|---|
| **Persistence** | Predict the last observed SPEI value (x[t], 12 months before target) for every cell | 1.3489 | 0.2811 | 0.2898 |
| **Climatology** | Predict the training-period cell mean for every test timestep | 1.0773 | 0.0 | 0.0 |
| **Linear trend** | Per-cell OLS regression on training targets; extrapolated to test period | 1.0651 | 0.0 | 0.0 |

---

## Experimental design

The final grid crosses **4 losses** (RQ1) × **2 global encoders** (RQ2) × **3 static encoders**
(RQ3) = **24 cells**. Within each cell, `model.lr` × `model.dropout` × `model.weight_decay`
(2×2×2 = 8 combos) are swept and only the single best checkpoint (lowest `val/loss`) is kept — see
[Grid search](../README.md#grid-search--the-24-cell-final-grid) in the README. This produces
24 × 8 = 192 candidate training runs and 24 final checkpoints.

### Architecture conditions

| ID | `static_encoder` | `global_encoder` | Description |
|---|---|---|---|
| naive/naive | naive | naive | Fully naive-injection baseline |
| single/naive | single | naive | Non-seasonal topography CNN; naive global |
| seasonal/naive | seasonal | naive | Seasonal topography CNN; naive global |
| naive/film | naive | film | FiLM conditioning; naive static |
| single/film | single | film | Non-seasonal CNN + FiLM |
| seasonal/film | seasonal | film | Full architecture (seasonal static + FiLM global) |

### Losses (RQ1)

| Loss | Values |
|---|---|
| `mse` | — |
| `pinball` | q=0.20 fixed |
| `weighted_mse` (hinge) | `drought_weight` ∈ {1, 5} — two separate final losses, not swept within a cell |

### Hyperparameter axes (swept within each cell)

| Axis | Values |
|---|---|
| `model.lr` | 1×10⁻³, 3×10⁻³ |
| `model.dropout` | 0.0, 0.1 |
| `model.weight_decay` | 0.0, 1×10⁻⁴ |

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

Baseline pooled metrics are logged per-run under `test/{persistence,clim,trend}/{rmse,drought_tpr,drought_f1}_pooled` and stored in the comparison CSV. Spatial heatmaps for baselines live in `figures/baselines/` (generated once, not per-model).

### Checkpoint saving vs. run selection

- **Checkpoint saved by**: `val/loss` (the training criterion, monitored by `ModelCheckpoint`)
- **Best run selected by**: `val/drought_f1_pooled` (pooled F1 over the validation set, preferred over per-cell median F1 because Alpine drought events are spatially clustered and validation coverage is limited to ~108 months)

---

## Global scalars

The 14 raw Mediterranean basins are aggregated (`med_sst_agg=grouped`, the default) into three regional means following the standard Sicily Channel W/E split (Millot & Taupier-Letage 2005; Lionello et al. 2006):

- **Western**: Alboran, Balearic, Ligurian, Gulf of Lion, Tyrrhenian, Sicilian
- **Eastern**: Adriatic, Ionian, Libyan, Aegean, Levantine
- **Black Sea**: kept separate (brackish, distinct circulation)

The pre-computed `Mediterranean_Sea` and `Western_Med` aggregates in the source file are excluded to avoid double-counting. Together with the NAO index and a cyclic month encoding (`month_sin`, `month_cos`), this gives the **6 global scalars** fed to the FiLM generator. These scalars are fed per input step, so FiLM conditions on each of the 36 input months rather than a single timestep. The 12-month-ahead target month is not supplied as a scalar. The `none` mode instead keeps all 14 basins + NAO + month = 17 scalars.
