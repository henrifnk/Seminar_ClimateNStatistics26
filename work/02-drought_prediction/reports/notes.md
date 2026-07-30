# Project Notes & Open Questions

Reference material moved here from the README to keep it focused on setup/commands: the model's
forward-pass mechanics, loss formulas, and the nonstationarity-diagnostic numbers. Everything below
that point is genuine open questions / limitations / TODOs, as before.

---

## Model architecture — technical reference (`code/model.py` — `RCNNModule`)

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
| Weighted MSE (hinge) | `weighted_mse` + `hinge` | (1 + drought_weight · max(0, −y)) · (ŷ−y)² | Asymmetric: weight=1 for y>0, linearly increasing for y<0. `drought_weight` ∈ {1, 5}; at drought_weight=1, SPEI=−1.5 → weight=2.5, SPEI=−2.0 → weight=3.0 |

---

## Nonstationarity diagnostics — results (`reports/generate/trend_baseline.py`)

Test-period (2015–2024) drought frequency is **2.03× higher** than training-period (1972–2004) frequency (median across valid cells: 4.9% training → 10.0% test). The test-period mean SPEI is shifted negative relative to the training mean, inflating the climatology RMSE because the climatology predicts the training mean.

A per-cell OLS linear trend fitted on training targets and extrapolated to the test period (linear trend baseline) barely reduces RMSE compared to climatology (1.0648 vs 1.0768, a 1.1% gain), confirming that the nonstationarity is not a smooth monotonic drift well-captured by linear extrapolation. Trend TPR = 0 (confirmed: the linear extrapolation never crosses the SPEI ≤ −1.5 drought threshold during the test period).

The residual floor (median per-cell test-period std of a perfect-mean predictor on detrended targets) is 1.056 in normalised SPEI units. Across the 192 candidate runs in the final grid, per-cell median model RMSE ranges 0.939–1.254 (best/median 1.001/worst); the best model beats the linear trend baseline by 0.126 RMSE and climatology by 0.138 RMSE — model skill is not primarily trend-following.

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

Baselines are not model-dependent and are evaluated once via `inv baselines`, writing figures and metrics to `figures/baselines/{persistence,climatology,trend}/`.

| Baseline | Definition | Test RMSE | Test TPR | Test F1 |
|---|---|---|---|---|
| **Persistence** | Predict the last observed SPEI value (x[t], 12 months before target) for every cell | 1.3489 | 0.2811 | 0.2898 |
| **Climatology** | Predict the training-period cell mean for every test timestep | 1.0758 | 0.0 | 0.0 |
| **Linear trend** | Per-cell OLS regression on training targets; extrapolated to test period | 1.0659 | 0.0 | 0.0 |

Climatology and linear trend never predict SPEI ≤ −1.5 (drought threshold) because both produce near-zero or slowly drifting values that do not reach the drought tail — hence TPR = 0 (confirmed empirically). Persistence achieves non-trivial TPR by repeating the last observed drought state.

The **residual floor** (median per-cell test-period standard deviation of the raw targets, `test/residual_floor`) is ~1.06 in normalised SPEI units — logged as context on the size of the test-period signal, not as a skill metric. The actual model-vs-baseline comparison is `test/rmse_vs_trend` (trend RMSE − model RMSE; positive = model beats the linear-trend baseline).

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

## Architecture

### FiLM scalar conditioning
- Currently uses only the **last input timestep** scalar (NAO, Med SST)
- For slow-moving signals like NAO and SST, a **seasonal mean or trend over the history window** might carry more signal than a single endpoint
- Easy to implement in `ScalarSubset`: replace `global_scalars[last_t]` with mean / mean+trend over `global_scalars[idx : idx + history_length]`
- Worth running as an ablation once the baseline FiLM results are in

### FiLM + BatchNorm interaction *(limitation)*
- FiLM is applied to `x_emb` after `self.embedding` (which already ends with BN), and `x_emb` then enters the gate ConvBlocks which contain another BN — see `model.py` forward pass lines ~392–402
- BN after FiLM partially undoes γ (re-normalises scale) and re-centres β, because BN computes per-channel statistics over the whole batch and rescales to unit variance
- **What survives**: BN uses one shared statistic per batch, so the *per-sample variation* in γ/β (sample A scaled differently from sample B based on NAO index) is preserved — that cross-sample variance is exactly the conditioning signal FiLM is meant to provide
- **What is lost**: the batch-common component of the scale/shift (any γ or β offset shared by all samples) is normalised out
- **Practical consequence**: FiLM's effect is muted compared to architectures using LayerNorm or no norm after FiLM (common in FiLM papers). This likely explains why naive/film does not consistently dominate naive/naive — the conditioning is present but weakened by subsequent BN in the gate convolutions
- **Fix if revisiting**: apply FiLM after the gate Conv2d but before BN (i.e. reorder ConvBlock internals), or switch gate normalisation to LayerNorm / InstanceNorm, or apply FiLM directly to gate inputs rather than to `x_emb`

### History encoding *(limitation)*
- The model stacks all `history_length` months as flat input channels → the embedding CNN has to learn temporal patterns from **channel position alone**
- The ConvLSTM recurrence runs across consecutive **prediction targets** (across batches in time), not *within* the history window
- A more principled design would unroll the ConvLSTM over the 36 input months explicitly, which would let the model naturally weight recent vs. distant past
- Current design is a common pragmatic choice; should be acknowledged in the limitations section

### Receptive field
- `kernel_size=3` with two ConvBlocks gives a 5×5 effective receptive field in the embedding — small relative to the 44×98 grid
- Large-scale pressure patterns driving Alpine moisture transport operate at much larger spatial scales
- FiLM partially compensates by injecting global NAO/SST signals globally, but spatial convolutions can't "see" far
- **TODO**: add `model.dilation` to the gridsearch (e.g. `1,2`) — dilated convolutions cheaply increase receptive field without adding parameters

### Cross-batch hidden state *(limitation)*
- The ConvLSTM hidden state (32 or 64 channels, 44×98 spatial) is carried across all sequential batches through the full training period
- It is unclear whether this long-range memory is actually used: if zeroing the state at test time doesn't hurt performance, the cross-batch recurrence isn't contributing
- Worth testing as an ablation; should be noted as a limitation if the recurrence proves ineffective

### Skip connections *(limitation)*
- No skip connections between embedding and output head
- A U-Net-style encoder-decoder with skip connections would let the output head access fine-grained spatial features directly, likely improving spatial detail in the predicted SPEI map
- Not planned for implementation but worth noting in limitations

### Baselines
- Only persistence and climatology are evaluated at test time
- A **pixel-wise linear regression** on the same input features would be a much stronger and more informative baseline
- Low priority to implement but strengthens the paper if included

---

## Evaluation

### Val vs. test metric gap
- Need to systematically compare val metrics to test metrics after each gridsearch
- A large gap (especially val ROC-AUC >> test ROC-AUC) would indicate overfitting to the 2006–2014 validation period
- The 2015–2024 test period includes exceptional drought years (2018, 2019, 2022) — possibly out-of-distribution relative to pre-2006 training data
- **TODO**: add per-year test metric breakdown (e.g. TPR per year) so we can see if model skill degrades specifically during the most extreme events

### Test period representativeness
- Training ends 2005, but central European drought frequency/severity has increased post-2010
- Consider whether results should be framed as "generalisation to a potentially more drought-prone climate" rather than pure held-out evaluation

---

## Experimental design

### Lead time
- Currently fixed at 12 months (long-term drought prediction goal); set via `data.lead_time` in config
- The **skill-horizon curve** (model skill as a function of lead time) would be scientifically interesting and is cheap to compute by sweeping `data.lead_time`
- At what lead time does the model stop beating persistence? This is a key result
- If running a full lead-time sweep is too expensive, at least compare 9 months to a shorter horizon (e.g. 3 or 6 months) to contextualise the difficulty

### Gridsearch — architectural axes
- `history_length` (data + model must match): try 12, 24, 36
- `model.hidden_state_size`: try 32, 64
- `model.embedding_size`: try 16, 32
- These are currently fixed but likely higher-leverage than lr/dropout tuning
- Implemented via `--history-lengths`, `--hidden-sizes`, `--embedding-sizes` flags (CSV) in `invoke gridsearch`

### Med SST aggregation
- Default (`med_sst_agg: grouped`): 3 regional means (Western / Eastern / Black Sea) based on the standard W/E split at the Sicily Channel (Millot & Taupier-Letage 2005; Lionello et al. 2006) + NAO + month_sin + month_cos = **6 FiLM scalars**
- Western group: Alboran, Balearic, Ligurian, Gulf of Lion, Tyrrhenian, Sicilian
- Eastern group: Adriatic, Ionian, Libyan, Aegean, Levantine
- Black Sea is kept separate (brackish, distinct circulation)
- `Mediterranean_Sea` and `Western_Med` (pre-computed aggregates in source file) are excluded in grouped mode to avoid double-counting
- `none` mode retains all 14 raw basins + NAO + month encodings = 17 scalars — useful as an ablation to check whether grouping loses information
- **TODO ablation**: compare grouped vs. none to verify grouping doesn't hurt; compare film vs. no-film to quantify the contribution of global scalars overall

### Month encoding
- `month_sin` / `month_cos` (cyclical encoding of the calendar month) added to `global_scalars.nc` in preprocessing
- Accessed at `last_t` (last input step) via `ScalarSubset`, so they encode the **current month**, not the target month
- Always included when global scalars are loaded (no separate flag needed)
