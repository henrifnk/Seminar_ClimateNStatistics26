# Interpretability Report

Findings from the two post-hoc interpretability analyses run on frozen `saved_models/`
checkpoints, test period only (2015–2024), no retraining. Method: occlusion-to-zero at each
feature's correct injection site (never shuffle/permute — every input is z-scored against
training statistics, so 0 is the training climatological mean, a clean in-distribution
counterfactual; permuting correlated inputs instead forces extrapolation and inflates
importance, Hooker, Mentch & Zhou 2021). Deltas describe *model reliance*, not necessity
(Breiman 2001; Fisher, Rudin & Dominici 2019) — see the correlated-feature caveat in each
section. Harness: `code/interpretability.py`. Reproduce with `invoke feature-importance` /
`invoke film-extremes`. Raw tables: `reports/interpretability/*.csv`. Figures:
`figures/interpretability/`.

---

## Phase 1 — Feature importance (Pinball q=0.20, all six architecture conditions)

Baseline (un-ablated) test performance per condition:

| Condition | RMSE | Drought F1 | Drought TPR |
|---|---|---|---|
| naive/naive | 1.154 | 0.423 | 0.412 |
| naive/film | 1.160 | 0.448 | 0.459 |
| single/naive | 1.144 | 0.438 | 0.541 |
| single/film | 1.143 | 0.425 | 0.417 |
| seasonal/naive | 1.142 | 0.427 | 0.547 |
| seasonal/film | 1.191 | 0.439 | 0.577 |

Full per-feature deltas: `reports/interpretability/feature_importance_pinball.csv`. Figures:
`figures/interpretability/feature_importance/{condition}.svg`.

### Point accuracy and drought detection are driven by different features

ΔRMSE is small everywhere (mostly ±0.01–0.05); Δdrought-F1/TPR are an order of magnitude
larger for several features. Occluding a feature can leave point accuracy almost untouched
while gutting detection skill — e.g. `naive/film`, ablating `med_sst`: ΔRMSE = +0.0014 (noise)
but ΔF1 = **−0.448** (total collapse). This is the intended reading of the brief's "governed
differently" framing, and it holds throughout: no feature's |ΔRMSE| exceeds 0.06, while five of
six conditions show at least one feature with |ΔF1| > 0.2.

### Med Sea SST is the single most relied-on feature for drought detection — and its role is architecture-conditional

`med_sst` is the #1 or #2 feature by |ΔF1| in **all six** conditions. Its effect size, though,
splits cleanly on whether the checkpoint has a dedicated spatial static encoder:

| Condition | static_encoder | ΔF1 (med_sst) | ΔTPR (med_sst) |
|---|---|---|---|
| naive/naive | naive | **−0.423 (F1 → exactly 0)** | **−0.412 (TPR → exactly 0)** |
| naive/film | naive | **−0.448 (F1 → exactly 0)** | **−0.459 (TPR → exactly 0)** |
| single/naive | single | −0.108 | −0.254 |
| single/film | single | −0.387 | −0.398 |
| seasonal/naive | seasonal | −0.201 | −0.388 |
| seasonal/film | seasonal | −0.030 | −0.197 |

Both `static_encoder=naive` conditions (no CNN-processed spatial pathway — topography is only
injected as raw constant channels) **collapse to exactly zero true positives anywhere in the
test period** when med_sst is zeroed. Every condition with a real spatial encoder (single or
seasonal CNN) retains partial detection skill without it. A follow-up diagnostic on
`naive/naive` (ablating med_sst, full test set) supports a specific reading of this collapse
— not a flat "warm SST → drought everywhere" bias, but closer to a severity gate with a real
spatial contribution:

- med_sst explains **~86% of the month-to-month variance** in domain-mean predicted severity
  (level std 0.405 → 0.154 when zeroed), and the damped level still correlates r=0.91 with the
  true level — med_sst *amplifies* an existing signal rather than being its sole source.
- The shift induced by zeroing correlates **r = −0.93** with how extreme the baseline
  prediction was (mean shift +0.87 at baseline ≤ −2 vs. −0.34 at baseline > 0) — tail
  compression toward climatology in both directions, not a uniform additive bias.
- Within-month **spatial pattern is mostly preserved** without med_sst (median r = 0.90 across
  the 120 test months, cell-rank correlation with the domain mean removed), though a bottom
  decile of months (p10 = 0.45, min = 0.03) shows med_sst genuinely reshaping *which* cells are
  flagged, not just the overall level.
- The test-period z-scored mean of med_sst (+0.20 to +0.25) is barely different from NAO's
  (+0.22) — the collapse is not an artifact of med_sst sitting unusually far from its
  training-period baseline (a "strongly trended standardized input" confound was checked and
  ruled out as the dominant explanation).

### Static topography consistently suppresses false alarms — removing it always increases recall

Ablating `static` increases drought TPR in **all six** conditions, with no exception:

| Condition | ΔTPR (static) | ΔF1 (static) |
|---|---|---|
| naive/naive | +0.231 | −0.003 |
| naive/film | +0.303 | −0.017 |
| single/naive | +0.135 | +0.015 |
| single/film | +0.125 | −0.018 |
| seasonal/naive | +0.185 | +0.007 |
| seasonal/film | +0.102 | −0.024 |

F1 barely moves (mixed sign, all within ±0.024) while TPR rises every time — a classic
precision/recall trade: the model uses topography to correctly *suppress* drought flags in
some cells, and removing it causes over-triggering elsewhere, with precision dropping to
offset the recall gain. This reads as topography's primary role being a detection-side
regularizer rather than a point-accuracy driver (its RMSE effect is small and inconsistently
signed across conditions).

### tas and month: large, condition-dependent detection reliance

`tas` (temperature) and `month` (seasonality, FiLM sin/cos) are frequently top-2/3 features by
|ΔF1|, but their effect size swings sharply with architecture — e.g. `tas`: ΔF1 = −0.062
(naive/naive) vs. −0.0002 (naive/film, essentially zero) vs. −0.328 (single/film). `month`
shows the same pattern in reverse for the *seasonal* static encoder specifically: ΔF1 = −0.181
(naive/film, no other month source) vs. −0.016 (seasonal/film) vs. +0.005 (seasonal/naive) —
the seasonal 4-CNN encoder already supplies month information architecturally, so the marginal
value of the FiLM month scalar collapses once it's present. This is a legible
redundant-vs-complementary-information result: reliance on a feature is not fixed, it depends
on what else the architecture has access to.

### Correlated-feature caveat

These are occlusion deltas, not causal necessity claims. `spei`, `wb`, and to some extent `tas`
all track moisture/energy deficit and are correlated in the input; low reliance on one doesn't
mean it's uninformative, only that the model can substitute a correlated channel when it's
removed. Read every number here as "what the *trained* model currently leans on," not "what
the task requires."

---

## Phase 2 — Does FiLM benefit the extremes? (all four losses)

Three checkpoints per loss: naive/naive (base), naive/film (isolates FiLM), seasonal/film (full
model). Bins by observed SPEI; RMSE and mean bias (**pred − obs**, confirmed in
`stratified_metrics`, `code/interpretability.py:396` — positive bias in a dry bin means the
model under-predicts drought severity, negative bias in the wet bin means it over-triggers
drought). Full tables: `reports/interpretability/film_stratified.csv`,
`film_2022_case_studies.csv`, `film_counterfactual.csv`. Figures per loss:
`figures/interpretability/film_extremes/{loss}_{stratified,2022_case_study,counterfactual}.svg`.

### Stratified error: pinball is the cleanest case for "helps extremes, aggregate flat"

| Loss | Condition | RMSE ≤−2 | bias ≤−2 | RMSE >−1 | bias >−1 | Aggregate RMSE |
|---|---|---|---|---|---|---|
| mse | naive/naive | 1.520 | +1.444 | 0.900 | −0.166 | 0.958 |
| mse | naive/film | 1.509 | +1.410 | 0.876 | −0.130 | 0.943 (−1.6%) |
| pinball | naive/naive | 0.832 | +0.740 | 1.290 | −0.933 | 1.154 |
| pinball | naive/film | **0.735** | **+0.626** | 1.300 | −0.930 | 1.160 (+0.5%) |
| wmse_w1_hinge | naive/naive | 1.253 | +1.173 | 0.991 | −0.375 | 0.969 |
| wmse_w1_hinge | naive/film | 1.181 | +1.115 | 0.973 | −0.359 | 0.948 (−2.2%) |
| wmse_w5_hinge | naive/naive | 1.008 | +0.961 | 1.129 | −0.631 | 1.035 |
| wmse_w5_hinge | naive/film | 0.946 | +0.894 | 1.161 | −0.716 | 1.055 (+1.9%) |

- **Pinball**: FiLM improves RMSE and bias in *all three* dry bins (largest gain in the most
  extreme, −0.097 RMSE / −0.113 bias at ≤−2), leaves the wet bin essentially untouched (+0.010
  RMSE, +0.003 bias), and aggregate RMSE moves by only +0.5% — the textbook "helps the
  extremes, aggregate unchanged" result from the brief.
- **MSE**: every dry bin shows a large positive bias (+0.91 to +1.44 — the model badly
  under-predicts drought severity everywhere in the dry tail, the classic regression-to-mean
  failure of a symmetric loss on a skewed target). FiLM barely dents this (≤0.03 bias
  improvement anywhere) — the *loss function*, not FiLM, is the first-order lever for MSE's
  under-prediction problem.
- **wmse_w1_hinge**: FiLM improves *every* bin including the wet one — a broad improvement, not
  a clean extremes-only isolation.
- **wmse_w5_hinge**: FiLM helps all three dry bins nicely (−0.06 RMSE at ≤−2) but at a real
  cost in the wet bin (RMSE +0.032, bias −0.085 more negative — more false alarms in normal
  months) and a net *worse* aggregate RMSE (+1.9%). Reported honestly: this is a genuine
  trade-off, not a free win.
- Adding the seasonal static encoder on top of FiLM (naive/film → seasonal/film) does **not**
  uniformly extend the isolated benefit — for pinball, seasonal/film's ≤−2 bin (0.803) sits
  *between* naive/naive and naive/film, i.e. worse than FiLM alone. Isolating FiLM cleanly
  matters; the full model doesn't automatically inherit the isolated gain multiplicatively.

### 2022 case study: all four losses independently pick the same month

Worst domain-mean-SPEI Alpine month in 2022: **July 2022**, domain-mean observed SPEI = **−1.61**
(all four losses agree). Global driver state FiLM conditioned on: NAO = −0.55 (below-average),
Western Med SST = +1.52, Eastern Med SST = +1.71, Black Sea SST = +1.74 (all ~1.5–1.7 std above
the training-period mean). This lines up with the documented 2022 European drought and the
well-recorded Mediterranean marine heatwave of that summer — a useful sanity check that the
pipeline recovers a real, independently-documented event rather than an artifact, and a
concrete illustration of what FiLM is actually conditioning on for this specific worked example.

### Genuineness counterfactual: real conditioning, strongly gated by the static pathway

Zeroing the global scalars (Phase 0's harness, reused verbatim) raises extreme-bin
(SPEI ≤ −1.5) RMSE in every one of the 8 (loss × FiLM-condition) cells — never a decrease, so
the conditioning is doing real work everywhere. But the *size* of the collapse depends almost
entirely on whether a spatial static encoder is present:

| Loss | naive/film | seasonal/film |
|---|---|---|
| mse | +62.4% | +34.4% |
| pinball | **+117.8%** | +17.6% |
| wmse_w1_hinge | +87.3% | +11.2% |
| wmse_w5_hinge | +101.4% | +19.4% |

naive/film (no competing spatial pathway) shows a 62–118% relative RMSE increase — the model
is nearly *entirely* dependent on the true scalar values for extreme-tail skill. seasonal/film
shows a much smaller (though still positive, 11–34%) increase — a real spatial static encoder
gives the model an alternative source of information it can partially fall back on. This is the
same architectural split found independently in Phase 1 (med_sst's F1 collapse is total only
for `static_encoder=naive`): **two independent analyses converge on the same result — the
FiLM/global-scalar pathway's importance is strongly conditional on whether a dedicated spatial
static encoder is also present.** Without one, the model leans on global scalars almost
exclusively for extreme detection; with one, it becomes more balanced and more robust to their
loss.

---

## Cross-cutting takeaway

The single strongest thread across both analyses: **global climate-index conditioning
(med_sst above all) and the spatial static encoder are substitutable sources of the same
"how severe is this month, and roughly where" signal.** A naively-injected static pathway
leaves the model almost entirely dependent on the global scalars for ever triggering a drought
prediction at all; a CNN-processed spatial encoder (single or seasonal) makes that dependence
partial rather than total. This did not come from a single ablation — it's convergent evidence
from Phase 1's per-feature deltas (exact-zero collapse gated by `static_encoder=naive`) and
Phase 2's counterfactual (62–118% vs. 11–34% RMSE increase, same gating variable).

## Limitations

- Single seed per checkpoint (s42) — no variance estimate across the ablation deltas.
- Occlusion reliance, not necessity (see the Phase 1 caveat above) — correlated inputs can
  substitute for each other, so a low delta does not mean a feature carries no information the
  task could use.
- The med_sst level/pattern decomposition (Phase 1) was run on one checkpoint
  (`naive/naive`, Pinball) as a targeted follow-up, not swept across all six conditions.
- Bins pool 43×98 grid cells across the whole test period; a cell-level breakdown could reveal
  regional structure this aggregate view cannot.

## Citations

- Occlusion / permutation importance & "model reliance" (no-retrain) framing: Breiman 2001
  (Random Forests); Fisher, Rudin & Dominici 2019, "All Models are Wrong, but Many are Useful"
  (JMLR).
- Correlated-feature caveat → occlude-to-mean, not permute: Hooker, Mentch & Zhou 2021,
  "Unrestricted permutation forces extrapolation" (Statistics and Computing).
- Geoscience/meteorology XAI framing: McGovern et al. 2019, "Making the Black Box More
  Transparent" (BAMS).
- Extremes-focused evaluation motivation: Watson 2022, "Machine learning applications for
  weather and climate need greater focus on extremes" (ERL). Case-study/intensity-stratified
  presentation template: Ravuri et al. 2021 (Nature, DGMR nowcasting).
- FiLM method: Perez et al. 2018 (AAAI). Base model: Marusov et al. 2024 (Env. Modelling &
  Software).
