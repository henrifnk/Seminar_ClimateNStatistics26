# Interpretability

We ran two analyses on the trained checkpoints, both on the test period only and without retraining:
a feature-importance study on the Pinball models, and a closer look at whether FiLM conditioning
helps in the drought tail. Everything below uses occlusion: we set one input to zero — which, because
every input is standardised on the training years, means replacing it with its training-period mean —
and measure how the test metrics change. This tells us what each trained model *relies on*, not which
feature is strictly necessary (Fisher & Rudin, 2019). We zero rather than permute because the inputs
are correlated, and permuting correlated inputs pushes the model off the data it was trained on
(Hooker et al., 2021).

## What the models rely on

For point accuracy, the answer is: almost nothing in particular. Across all six architecture
conditions, occluding any single feature changes the pooled RMSE by less than 0.06. This matches the
main result of the paper — at a twelve-month lead the models sit close to the trend RMSE floor, so
there is little accuracy left for any one input to move. The interesting variation is entirely in
drought detection.

Which feature drives detection depends on the architecture. In the two conditions with no static
encoder (naive/naive and naive/film), the Mediterranean SST dominates: removing it drops the drought
F1 from about 0.43 to zero — the model stops predicting any cell below the −1.5 threshold. Once a
static encoder is added (single or seasonal), detection shifts onto near-surface temperature instead:
in single/film, zeroing temperature drops F1 from 0.43 to 0.10, and in seasonal/film it drops it by
about 0.21. Temperature driving drought detection is physically sensible, since it controls
evapotranspiration and therefore SPEI. Topography behaves the opposite way — removing it *raises* the
detection rate (TPR up by 0.10 to 0.30 depending on condition), so it acts as a spatial brake that
holds back drought calls. SPEI history and the NAO index barely matter anywhere; at this lead the
model is not really running on drought persistence, and the grouped Mediterranean SST seems to carry
whatever large-scale signal is useful.

## What the Mediterranean SST actually does

The collapse of detection when the Mediterranean SST is removed looks alarming, so we checked what is
behind it on the naive/naive Pinball model. It is not a case of "warm sea, drought everywhere." The
SST works as a domain-wide severity control: it accounts for about 86% of the month-to-month variance
in the domain-mean predicted SPEI (the standard deviation of the domain-mean prediction falls from
0.405 to 0.154 when the SST is zeroed), while the damped prediction still tracks the true level
(correlation 0.91). In other words, the SST amplifies a signal that is already there rather than
being its only source.

The spatial pattern comes mostly from elsewhere. With the SST removed, the map of which cells look
driest within a given month is largely unchanged (median correlation 0.90 between the full and
ablated predictions), except in the driest ~10% of months, where the SST does reshuffle which cells
get flagged (correlation as low as 0.03). The effect on the level is a compression toward
climatology in both directions, not a flat shift: the change induced by zeroing the SST correlates at
−0.93 with how extreme the original prediction was, moving predictions +0.87 in the most extreme
drought cells and −0.34 in the wettest ones. This is why detection collapses — the most extreme
prediction moves from −2.39 up to exactly −1.50, so almost nothing crosses the threshold (22,615
flagged cell-months become one) — even though the spatial information survives. A trend artefact does
not explain it either: over the test period the Mediterranean SST sits at +0.20 to +0.25 standard
deviations, essentially the same drift as the NAO (+0.22), yet the two have very different
importance.

So the Mediterranean SST is best described as an amplitude gate: it sets how severe the whole domain
looks and therefore whether predictions reach the drought tail, while the local fields decide where
the droughts are. (The spatial correlation above is between the model's own predictions with and
without the SST; it shows the SST is not the source of the pattern, not that the pattern is correct.)

## Does FiLM help the extremes?

Yes, and the benefit is concentrated in the tail. Binning test cell-months by observed SPEI and
comparing naive/naive against naive/film, FiLM lowers both the error and the bias in the extreme
bins while leaving the normal bin essentially unchanged. Under Pinball, the RMSE in the ≤ −2 bin drops
from 0.83 to 0.73 and the bias from +0.74 to +0.63; under the w=5 hinge loss it drops from 1.01 to
0.95. (Bias here is prediction minus observation, so a positive value in a dry bin means the model
under-predicts the drought's severity — FiLM reduces that under-prediction.) The gain does not show
up in the aggregate: the overall RMSE is flat or slightly worse with FiLM (1.154 vs 1.160 under
Pinball). FiLM trades a little average accuracy for better severe-drought predictions.

To check that this is genuine conditioning and not a fixed downward bias, we re-ran the FiLM models
with the global signal zeroed. For naive/film the extreme-bin RMSE roughly doubles (from 0.54 to 1.18
under Pinball; a rise of 0.84 under the w=1 hinge loss), so the tail performance really does depend on
the actual climate state the model was given. For seasonal/film the same test barely moves the numbers
(a rise of 0.10 under Pinball), because the seasonal encoder gives that model another route to the
same information — consistent with FiLM mattering most when there is no static encoder.

The July 2022 case study is a clear example: the domain-mean observed SPEI was −1.61, the
Mediterranean SST was 1.5 to 1.7 standard deviations above normal, and the NAO was mildly negative
(−0.55). This is exactly the kind of state FiLM can use, and the maps show it deepening and extending
the predicted drought relative to the naive baseline.

## Putting it together

The two analyses point at the same mechanism. The global signal, injected through FiLM, controls the
overall severity and therefore whether predictions reach the drought tail; the local fields, mainly
temperature once a static encoder is present, decide the spatial pattern; and the loss function
controls how hard the model pushes toward the tail, with Pinball pushing hardest. This is why
detection — a threshold-crossing quantity — depends strongly on a few inputs while RMSE, dominated by
the roughly 184,000 near-normal cell-months, hardly moves at all.
