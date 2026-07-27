# Project Notes & Open Questions

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
