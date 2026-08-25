# Results Data Provenance (01-climate_uncertainty)

This file documents the origin and purpose of each data file currently in this folder.

## Folder scope

Current data files covered here:

- merged_regional_period_summary.csv
- model_specific_forced_responses.csv
- average_bootstrap_ci_width_by_metric.csv

## Common upstream source

All three result files are downstream products of the same core indicator table:

- ../data/period_mean_regional_jja_warming.csv

That core table stores period-mean regional JJA warming values
Y(m,r,R,P) for each model m, member r, region R, and period P.

Core construction chain for Y(m,r,R,P):

1. Monthly tas -> annual JJA gridcell temperature (day-weighted JJA mean)
2. Gridcell JJA -> regional JJA (cos(latitude) area weights)
3. Regional JJA -> anomaly vs 1995-2014 baseline
4. Annual anomalies -> 20-year period means for P1, P2, P3

## 1) merged_regional_period_summary.csv

### What this data is about

Master region-period summary table (NEU, WCE, MED x P1, P2, P3) that combines:

- mean warming
- uncertainty decomposition terms
- contribution rates
- SNR metrics
- threshold exceedance probabilities (2C, 3C)
- prediction quantiles/range width
- full vs balanced sensitivity comparison fields

### Relevant chapter(s) in 01-climate_uncertainty.Rmd

- Results:
  - Projected Warming and Divergence among Model Responses
  - Changing Structure of Projection Uncertainty
  - Consequences of Uncertainty for Projection Interpretation
  - Robustness of the Main Findings
- Appendices:
  - Appendix A.1, A.2, A.3 (all loaded from this file)
  - Appendix C.2 (full vs balanced comparison columns)

### Calculated from what data

Immediate lineage is a merge of region-period outputs equivalent to:

- forced response table
- uncertainty decomposition table
- prediction range table
- signal-to-noise table
- threshold exceedance table
- balanced sensitivity summary table

All of those are ultimately derived from:

- ../data/period_mean_regional_jja_warming.csv

### Used for what graphic(s)

This CSV is mainly used for report tables (Appendix A and C), not as a direct figure input in the Rmd.
Its columns correspond to metrics shown in these figures:

- fig02 (mean warming)
- fig04, fig05, fig06, fig07 (uncertainty metrics)
- fig08 (prediction ranges)
- fig09, fig10 (SNR)
- fig11 (threshold probabilities)
- fig18 (full vs balanced sensitivity comparison)

## 2) model_specific_forced_responses.csv

### What this data is about

Model-by-region-by-period forced warming responses, with the companion multi-model mean for each region-period.

### Relevant chapter(s) in 01-climate_uncertainty.Rmd

- Results:
  - Projected Warming and Divergence among Model Responses
- Appendices:
  - Mentioned as a complete table that is omitted to avoid repetition

### Calculated from what data

- ../data/period_mean_regional_jja_warming.csv

### Used for what graphic(s)

Primary:

- fig03_model_forced_response

Also conceptually supports interpretation in:

- fig02_regional_jja_warming_timeseries

## 3) average_bootstrap_ci_width_by_metric.csv

### What this data is about

Average width of the 95% hierarchical-bootstrap percentile confidence interval for each metric.

Each row is one metric (for example total_sd, total_snr, exceedance_probability_2c), with:

- Metric
- Mean 95% CI width

### Relevant chapter(s) in 01-climate_uncertainty.Rmd

- Results:
  - Robustness of the Main Findings
- Appendices:
  - Appendix C.1 (loaded directly from this file)

### Calculated from what data

Directly based on hierarchical-bootstrap confidence interval outputs built from:

- ../data/period_mean_regional_jja_warming.csv
- uncertainty decomposition outputs
- signal-to-noise outputs
- threshold exceedance outputs

### Used for what graphic(s)

No direct figure in the main Results section.
Used as a compact robustness table (Appendix C.1), and it summarizes uncertainty behavior that is visualized in:

- fig16_bootstrap_ci_uncertainty_metrics
- fig17_bootstrap_ci_snr_threshold_probability

## Note

Further information please check https://github.com/ShuangyingXu/climate_uncertainty .