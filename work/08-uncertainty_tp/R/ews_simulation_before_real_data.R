# ================================================================
# EWS Simulation Study Before Real-Data Application
# Climate Tipping Points / Signal Ambiguity / Bayesian Updating
#
# Author: Jihao Li
#
# Purpose:
#   This script creates a controlled benchmark before applying early-warning
#   indicators to real climate data. It compares an approaching fold
#   bifurcation with non-tipping and ambiguity scenarios generated from the
#   same stylized dynamical system.
#
# Main message for the seminar:
#   Increasing variance and lag-1 autocorrelation can be useful early-warning
#   signals, but they are not direct proof of tipping because similar signals
#   can also arise from seasonal forcing, changing noise, or sparse sampling.
# ================================================================

# -------------------------------
# 0. Packages and reproducibility
# -------------------------------

required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "zoo",
  "pROC",
  "purrr",
  "readr",
  "tibble",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    "\nExample: install.packages(c(\"",
    paste(missing_packages, collapse = "\", \""),
    "\"))"
  )
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(pROC)
library(purrr)
library(readr)
library(tibble)
library(scales)

set.seed(123)

output_dir <- "outputs/ews_before_real_data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

theme_set(theme_minimal(base_size = 12))

# -------------------------------
# 1. Helper functions
# -------------------------------

rolling_var <- function(x, width = 60) {
  zoo::rollapply(
    x,
    width = width,
    FUN = var,
    fill = NA_real_,
    align = "right"
  )
}

rolling_ac1 <- function(x, width = 60) {
  zoo::rollapply(
    x,
    width = width,
    FUN = function(z) {
      if (stats::sd(z, na.rm = TRUE) == 0) {
        return(NA_real_)
      }
      stats::acf(z, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2]
    },
    fill = NA_real_,
    align = "right"
  )
}

kendall_trend <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) < 10 || stats::sd(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::cor(seq_along(x), x, method = "kendall"))
}

safe_scale <- function(x) {
  if (all(is.na(x)) || stats::sd(x, na.rm = TRUE) == 0) {
    return(rep(0, length(x)))
  }
  as.numeric(scale(x))
}

calc_ews_features <- function(df, window = 60) {
  var_series <- rolling_var(df$x, width = window)
  ac1_series <- rolling_ac1(df$x, width = window)

  tibble(
    tau_var = kendall_trend(var_series),
    tau_ac1 = kendall_trend(ac1_series)
  )
}

prepare_rolling_example <- function(df, window = 60) {
  df %>%
    mutate(
      rolling_variance = rolling_var(x, width = window),
      rolling_ac1 = rolling_ac1(x, width = window)
    )
}

# -------------------------------
# 2. Stylized fold model
# -------------------------------

# Model:
#   dx = (a_t - x^2 + forcing_t) dt + sigma_t dW
#
# Stable equilibrium exists around sqrt(a_t) when a_t > 0.
# As a_t approaches 0, the system approaches a fold bifurcation.
#
# Scenarios:
#   tipping           : a_t decreases toward the bifurcation point.
#   stable            : a_t remains constant, no tipping.
#   seasonal          : no tipping, but periodic forcing may imitate EWS.
#   increasing_noise  : no tipping, but noise grows over time.
#   sparse_sampling   : tipping trajectory observed less frequently.

simulate_fold <- function(
    n = 600,
    a_start = 2,
    a_end = 0.05,
    sigma = 0.15,
    dt = 0.05,
    scenario = c(
      "tipping",
      "stable",
      "seasonal",
      "increasing_noise",
      "sparse_sampling"
    ),
    seasonal_amp = 0.20,
    seasonal_period = 90,
    noise_growth = 2.5,
    sample_every = 4
) {
  scenario <- match.arg(scenario)

  x <- numeric(n)

  if (scenario %in% c("tipping", "sparse_sampling")) {
    a <- seq(a_start, a_end, length.out = n)
  } else {
    a <- rep(a_start, n)
  }

  sigma_t <- rep(sigma, n)
  if (scenario == "increasing_noise") {
    sigma_t <- sigma * seq(1, noise_growth, length.out = n)
  }

  forcing_t <- rep(0, n)
  if (scenario == "seasonal") {
    forcing_t <- seasonal_amp * sin(2 * pi * seq_len(n) / seasonal_period)
  }

  x[1] <- sqrt(a[1])

  for (t in 2:n) {
    drift <- a[t] - x[t - 1]^2 + forcing_t[t]
    noise <- sigma_t[t] * sqrt(dt) * stats::rnorm(1)
    x[t] <- x[t - 1] + drift * dt + noise
  }

  out <- tibble(
    time = seq_len(n),
    x = x,
    a = a,
    sigma_t = sigma_t,
    forcing_t = forcing_t,
    scenario = scenario
  )

  if (scenario == "sparse_sampling") {
    out <- out %>%
      slice(seq(1, n(), by = sample_every)) %>%
      mutate(time = row_number())
  }

  out
}

# -------------------------------
# 3. Simulation design
# -------------------------------

n_sim <- 500
n_time <- 600
window <- 60

scenario_design <- tibble(
  scenario = c(
    "tipping",
    "stable",
    "seasonal",
    "increasing_noise",
    "sparse_sampling"
  ),
  label = c(1, 0, 0, 0, 1),
  interpretation = c(
    "Approaching fold bifurcation",
    "Non-tipping control with constant stability",
    "Non-tipping periodic forcing; possible false warning signal",
    "Non-tipping growing stochastic variability; possible false warning signal",
    "Approaching fold bifurcation with reduced sampling frequency"
  )
)

simulation_results <- purrr::map_dfr(seq_len(n_sim), function(i) {
  purrr::map_dfr(scenario_design$scenario, function(scn) {
    ts_df <- simulate_fold(
      n = n_time,
      scenario = scn,
      sigma = 0.15,
      seasonal_amp = 0.20,
      noise_growth = 2.5,
      sample_every = 4
    )

    features <- calc_ews_features(ts_df, window = window)

    tibble(
      sim_id = i,
      scenario = scn
    ) %>%
      bind_cols(features)
  })
}) %>%
  left_join(scenario_design, by = "scenario")

# Remove rare failed feature rows if a rolling statistic is undefined.
simulation_results <- simulation_results %>%
  filter(!is.na(tau_var), !is.na(tau_ac1))

# Combined EWS score:
# Positive values indicate stronger joint upward trends in variance and AC1.
simulation_results <- simulation_results %>%
  mutate(
    z_tau_var = safe_scale(tau_var),
    z_tau_ac1 = safe_scale(tau_ac1),
    score_combined = z_tau_var + z_tau_ac1
  )

# Signal threshold:
# Use the stable non-tipping control to set a 10% reference false-positive rate.
threshold <- simulation_results %>%
  filter(scenario == "stable") %>%
  summarise(threshold = stats::quantile(score_combined, 0.90, na.rm = TRUE)) %>%
  pull(threshold)

simulation_results <- simulation_results %>%
  mutate(signal = score_combined > threshold)

# -------------------------------
# 4. ROC / AUC analysis
# -------------------------------

# Main binary comparison:
#   tipping-like scenarios: tipping + sparse_sampling
#   non-tipping scenarios : stable + seasonal + increasing_noise
#
# This is deliberately stricter than only comparing tipping vs stable, because
# it asks whether EWS can distinguish tipping from ambiguity.

roc_var <- pROC::roc(
  response = simulation_results$label,
  predictor = simulation_results$tau_var,
  quiet = TRUE,
  direction = "<"
)

roc_ac1 <- pROC::roc(
  response = simulation_results$label,
  predictor = simulation_results$tau_ac1,
  quiet = TRUE,
  direction = "<"
)

roc_combined <- pROC::roc(
  response = simulation_results$label,
  predictor = simulation_results$score_combined,
  quiet = TRUE,
  direction = "<"
)

auc_table <- tibble(
  indicator = c("Rolling variance trend", "Rolling AC1 trend", "Combined EWS score"),
  auc = c(
    as.numeric(pROC::auc(roc_var)),
    as.numeric(pROC::auc(roc_ac1)),
    as.numeric(pROC::auc(roc_combined))
  )
) %>%
  mutate(auc = round(auc, 3))

# -------------------------------
# 5. Scenario summary and Bayes update
# -------------------------------

summary_table <- simulation_results %>%
  group_by(scenario, label, interpretation) %>%
  summarise(
    n = n(),
    mean_tau_var = mean(tau_var, na.rm = TRUE),
    mean_tau_ac1 = mean(tau_ac1, na.rm = TRUE),
    mean_score = mean(score_combined, na.rm = TRUE),
    signal_rate = mean(signal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(mean_tau_var, mean_tau_ac1, mean_score, signal_rate),
      ~ round(.x, 3)
    )
  )

sensitivity <- simulation_results %>%
  filter(label == 1) %>%
  summarise(value = mean(signal, na.rm = TRUE)) %>%
  pull(value)

false_positive_rate <- simulation_results %>%
  filter(label == 0) %>%
  summarise(value = mean(signal, na.rm = TRUE)) %>%
  pull(value)

priors <- c(0.01, 0.05, 0.10, 0.25, 0.50)

posterior_table <- tibble(
  prior_tipping = priors,
  sensitivity = sensitivity,
  false_positive_rate = false_positive_rate,
  posterior_tipping_given_signal =
    (sensitivity * prior_tipping) /
    (sensitivity * prior_tipping +
       false_positive_rate * (1 - prior_tipping))
) %>%
  mutate(
    across(everything(), ~ round(.x, 3))
  )

# -------------------------------
# 6. Example time series and rolling indicators
# -------------------------------

example_series <- purrr::map_dfr(scenario_design$scenario, function(scn) {
  simulate_fold(
    n = n_time,
    scenario = scn,
    sigma = 0.15,
    seasonal_amp = 0.20,
    noise_growth = 2.5,
    sample_every = 4
  )
})

example_rolling <- example_series %>%
  group_by(scenario) %>%
  group_modify(~ prepare_rolling_example(.x, window = window)) %>%
  ungroup()

# -------------------------------
# 7. Plots
# -------------------------------

p_time_series <- example_series %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_design$scenario,
      labels = c(
        "Tipping",
        "Stable control",
        "Seasonal non-tipping",
        "Increasing-noise non-tipping",
        "Sparse-sampling tipping"
      )
    )
  ) %>%
  ggplot(aes(x = time, y = x)) +
  geom_line(linewidth = 0.45, color = "#2C3E50") +
  facet_wrap(~ scenario, scales = "free_y", ncol = 1) +
  labs(
    title = "Example simulated time series",
    subtitle = "The benchmark includes both true approaching bifurcation and ambiguous non-tipping cases.",
    x = "Time",
    y = "State variable x"
  )

ggsave(
  filename = file.path(output_dir, "01_example_time_series.png"),
  plot = p_time_series,
  width = 8,
  height = 9,
  dpi = 300
)
ggsave(
  filename = file.path(output_dir, "01_example_time_series.pdf"),
  plot = p_time_series,
  width = 8,
  height = 9
)
p_rolling <- example_rolling %>%
  select(time, scenario, rolling_variance, rolling_ac1) %>%
  pivot_longer(
    cols = c(rolling_variance, rolling_ac1),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      rolling_variance = "Rolling variance",
      rolling_ac1 = "Rolling lag-1 autocorrelation"
    ),
    scenario = factor(
      scenario,
      levels = scenario_design$scenario,
      labels = c(
        "Tipping",
        "Stable control",
        "Seasonal non-tipping",
        "Increasing-noise non-tipping",
        "Sparse-sampling tipping"
      )
    )
  ) %>%
  ggplot(aes(x = time, y = value)) +
  geom_line(linewidth = 0.45, color = "#34495E", na.rm = TRUE) +
  facet_grid(indicator ~ scenario, scales = "free_y") +
  labs(
    title = "Rolling early-warning indicators",
    subtitle = "Variance and AC1 may increase before tipping, but ambiguity scenarios can also create warning-like patterns.",
    x = "Time",
    y = "Indicator value"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(output_dir, "02_rolling_indicators.png"),
  plot = p_rolling,
  width = 12,
  height = 6,
  dpi = 300
)
ggsave(
  filename = file.path(output_dir, "02_rolling_indicators.pdf"),
  plot = p_rolling,
  width = 12,
  height = 6
)
roc_df <- bind_rows(
  tibble(
    indicator = paste0("Variance trend, AUC = ", auc_table$auc[1]),
    specificity = roc_var$specificities,
    sensitivity = roc_var$sensitivities
  ),
  tibble(
    indicator = paste0("AC1 trend, AUC = ", auc_table$auc[2]),
    specificity = roc_ac1$specificities,
    sensitivity = roc_ac1$sensitivities
  ),
  tibble(
    indicator = paste0("Combined score, AUC = ", auc_table$auc[3]),
    specificity = roc_combined$specificities,
    sensitivity = roc_combined$sensitivities
  )
)

p_roc <- roc_df %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = indicator)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  coord_equal() +
  labs(
    title = "ROC curves for early-warning indicators",
    subtitle = "Binary task: approaching bifurcation vs non-tipping/ambiguity cases.",
    x = "False positive rate",
    y = "True positive rate",
    color = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(output_dir, "03_roc_auc.png"),
  plot = p_roc,
  width = 8,
  height = 6,
  dpi = 300
)
ggsave(
  filename = file.path(output_dir, "03_roc_auc.pdf"),
  plot = p_roc,
  width = 8,
  height = 6
)

p_signal_rate <- summary_table %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_design$scenario,
      labels = c(
        "Tipping",
        "Stable control",
        "Seasonal non-tipping",
        "Increasing-noise non-tipping",
        "Sparse-sampling tipping"
      )
    )
  ) %>%
  ggplot(aes(x = scenario, y = signal_rate, fill = factor(label))) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("0" = "#E67E22", "1" = "#2980B9"),
    labels = c("Non-tipping", "Tipping")
  ) +
  labs(
    title = "Signal rates by scenario",
    subtitle = "Non-tipping ambiguity scenarios quantify false-positive warning signals.",
    x = NULL,
    y = "Share classified as EWS signal",
    fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  filename = file.path(output_dir, "04_signal_rates_by_scenario.png"),
  plot = p_signal_rate,
  width = 9,
  height = 5.5,
  dpi = 300
)
ggsave(
  filename = file.path(output_dir, "04_signal_rates_by_scenario.pdf"),
  plot = p_signal_rate,
  width = 9,
  height = 5.5
)
p_posterior <- posterior_table %>%
  ggplot(aes(x = prior_tipping, y = posterior_tipping_given_signal)) +
  geom_line(linewidth = 1, color = "#8E44AD") +
  geom_point(size = 2.5, color = "#8E44AD") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Bayesian interpretation of an EWS signal",
    subtitle = paste0(
      "Posterior depends on prior risk and false-positive rate; ",
      "sensitivity = ", round(sensitivity, 3),
      ", FPR = ", round(false_positive_rate, 3), "."
    ),
    x = "Prior probability of tipping",
    y = "Posterior probability after observing EWS signal"
  )

ggsave(
  filename = file.path(output_dir, "05_bayesian_posterior.png"),
  plot = p_posterior,
  width = 8,
  height = 5.5,
  dpi = 300
)
ggsave(
  filename = file.path(output_dir, "05_bayesian_posterior.pdf"),
  plot = p_posterior,
  width = 8,
  height = 5.5
)

p_score_density <- simulation_results %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_design$scenario,
      labels = c(
        "Tipping",
        "Stable control",
        "Seasonal non-tipping",
        "Increasing-noise non-tipping",
        "Sparse-sampling tipping"
      )
    )
  ) %>%
  ggplot(aes(x = score_combined, fill = scenario)) +
  geom_density(alpha = 0.35) +
  geom_vline(
    xintercept = threshold,
    linetype = "dashed",
    linewidth = 0.8,
    color = "black"
  ) +
  labs(
    title = "Distribution of the combined EWS score",
    subtitle = "Dashed line: 90th percentile of stable-control score used as signal threshold.",
    x = "Combined EWS score",
    y = "Density",
    fill = "Scenario"
  ) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(output_dir, "06_combined_score_density.png"),
  plot = p_score_density,
  width = 9,
  height = 5.5,
  dpi = 300
)
ggsave(
  filename = file.path(output_dir, "06_combined_score_density.pdf"),
  plot = p_score_density,
  width = 9,
  height = 5.5
)
# -------------------------------
# 8. Save tables
# -------------------------------

readr::write_csv(
  simulation_results,
  file.path(output_dir, "simulation_results.csv")
)

readr::write_csv(
  auc_table,
  file.path(output_dir, "auc_table.csv")
)

readr::write_csv(
  summary_table,
  file.path(output_dir, "summary_table.csv")
)

readr::write_csv(
  posterior_table,
  file.path(output_dir, "posterior_table.csv")
)

# -------------------------------
# 9. Console summary
# -------------------------------

message("\n=== AUC table ===")
print(auc_table)

message("\n=== Scenario summary ===")
print(summary_table)

message("\n=== Bayesian posterior table ===")
print(posterior_table)

message("\nSignal threshold based on stable control 90th percentile: ", round(threshold, 3))
message("Outputs saved to: ", normalizePath(output_dir))

# -------------------------------
# 10. Suggested wording for slides
# -------------------------------

message("\nSlide wording:")
message(
  "This simulation benchmark does not claim to predict a real climate system. ",
  "It tests whether early-warning indicators can distinguish an approaching ",
  "fold bifurcation from non-tipping but statistically ambiguous alternatives. ",
  "The real-data application should therefore be interpreted as illustrative, ",
  "not as direct evidence of an imminent tipping point."
)
