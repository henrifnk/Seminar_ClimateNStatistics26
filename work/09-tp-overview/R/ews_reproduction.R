# Reproduction of the statistical EWS analysis in Boers & Rypdal (2021), PNAS.
# Data source: github.com/niklasboers/GrIS-EWS (public repository by the authors)
#
# Four analysis steps (see also 09-tp-overview.Rmd, Data and Methods):
#   1. Log-transform the melt series
#   2. Gaussian-detrend with bandwidth sigma = 30 years
#   3. Trailing-window variance and lag-1 AC1 (w = 70 years; indexed by end year)
#   4. OLS estimation of the linear trend in each indicator

library(ggplot2)
library(tidyr)
library(dplyr)

## ---- load data ---------------------------------------------------------
# columns: year, JJA summer temp, CWG stack melt, NU melt
raw <- read.table("work/09-tp-overview/data/CWG_NU_melt_jja_temp.txt", header = FALSE)
colnames(raw) <- c("year", "jja_temp", "cwg_melt", "nu_melt")
raw <- raw[order(raw$year), ]  # sort chronologically

## ---- restrict to analysis period ----------------------------------------
# 1855-2013: matches the range used in the original code (avoids the
# earlier, noisier part of the record)
df <- subset(raw, year >= 1855 & year <= 2013)

## ---- 1. log-transform melt series ---------------------------------------
# Step 1: x_t = log(m_t - min(m) + 1).
# - Shift by abs(min)+1 so every argument of log is > 0.
# - Log scale reduces heteroscedasticity before detrending.
df$melt_log <- log(df$cwg_melt + abs(min(df$cwg_melt, na.rm = TRUE)) + 1)

## ---- 2. Gaussian detrending ---------------------------------------------
# Step 2: estimate a slow trend tilde(x)_t by a Gaussian-weighted moving
# average with bandwidth sigma (years), then form residuals
#   r_t = x_t - tilde(x)_t
# used for EWS. Larger sigma => smoother trend, more low-frequency removed.
# Kernel: w_k ∝ exp(-k^2 / (2 sigma^2)), normalised to sum to 1.
# Edges: reflect the series so the smoother is defined at the ends.
gaussian_smooth <- function(x, sigma) {
  n <- length(x)
  radius <- ceiling(3 * sigma)           # truncate kernel at about ±3 sigma
  idx <- -radius:radius
  kernel <- exp(-(idx^2) / (2 * sigma^2))
  kernel <- kernel / sum(kernel)         # weights sum to 1
  x_pad <- c(rev(x[1:radius]), x, rev(x[(n - radius + 1):n]))  # reflect edges
  smoothed <- stats::filter(x_pad, kernel, sides = 2)
  as.numeric(smoothed[(radius + 1):(radius + n)])
}

# sigma = 30 years: representative value from Boers & Rypdal (2021)
df$trend     <- gaussian_smooth(df$melt_log, sigma = 30)
df$detrended <- df$melt_log - df$trend   # residual series r_t

## ---- 3. trailing-window variance and AC1 --------------------------------
# Step 3: trailing (one-sided) window of width w, as in Boers & Rypdal
# (2021) Fig. 1 D/E. For each end year t, use residuals in
# {t-w+1, ..., t} to compute sample variance and lag-1 autocorrelation.
# First valid end year: 1855+w-1 (= 1924 for w = 70); last: 2013.
run_var <- function(x, w) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in w:n) {
    out[i] <- var(x[(i - w + 1):i])
  }
  out
}

run_ac1 <- function(x, w) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in w:n) {
    seg <- x[(i - w + 1):i]
    # lag-1 autocorrelation within the window: corr(r_s, r_{s+1})
    out[i] <- cor(seg[-1], seg[-length(seg)])
  }
  out
}

df$variance <- run_var(df$detrended, w = 70)
df$ac1      <- run_ac1(df$detrended, w = 70)

## ---- 4. OLS trend estimation --------------------------------------------
# Step 4: for each indicator Y_t, fit Y_t = beta0 + beta1 * year + eps
# by OLS. The slope beta1 measures the linear trend. A regression t-test
# of H0: beta1 = 0 is also computed; p-values are indicative because
# successive window estimates are serially dependent.
valid <- !is.na(df$variance)

fit_var <- lm(variance ~ year, data = df[valid, ])
fit_ac1 <- lm(ac1 ~ year, data = df[valid, ])

cat("variance trend: slope =", round(coef(fit_var)[2], 5),
    " p =", format.pval(summary(fit_var)$coefficients[2, 4]), "\n")
cat("AC1 trend:      slope =", round(coef(fit_ac1)[2], 5),
    " p =", format.pval(summary(fit_ac1)$coefficients[2, 4]), "\n")

## ---- plot ------------------------------------------------------------------
plot_df <- df[valid, c("year", "variance", "ac1")] %>%
  pivot_longer(cols = c(variance, ac1), names_to = "indicator", values_to = "value")

p <- ggplot(plot_df, aes(x = year, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick",
              linetype = "dashed", linewidth = 0.6) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 1,
             labeller = as_labeller(c(variance = "Variance (w = 70 yr)",
                                      ac1 = "AC1 (w = 70 yr)"))) +
  labs(
    title = "EWS Indicators for the CWG Ice Sheet",
    x = "Year", y = NULL,
    caption = "Data: Boers & Rypdal (2021), github.com/niklasboers/GrIS-EWS"
  ) +
  theme_minimal(base_size = 12)

ggsave("work/09-tp-overview/figures/ews_reproduction_plot.png", p, width = 7, height = 7, dpi = 300)
