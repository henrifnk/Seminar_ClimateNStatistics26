# ======================================================================
# Plot ALE-based lag importance curves for selected dynamic features
#
# Reads the CSV produced by compute_lag_importance.py and creates
# one individual plot per selected feature (filled area + line, x-axis
# reversed so lag=0/today is on the right), saved as SVG.
# ======================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)
source("R/settings.R")

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
DIR_RESULTS <- "data/results"
RESULTS_CSV_NAME <- "lag_importance.csv"

DIR_FIGURES <- "figures"

# Only these features get individual plots
FEATURES_TO_PLOT <- c("Ta_C", "rad_whm2")

# Same color for every feature (consistent with the ALE plots, which
# also don't color-code by feature) -- a calm blue that doesn't clash
# with the season colors used elsewhere.
curve_color <- "#2a78d6"
fill_alpha <- 0.15


# ----------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------

# Load the lag importance results CSV.
load_lag_importance_data <- function(dir_results, filename) {
  read_csv(file.path(dir_results, filename), show_col_types = FALSE)
}

# Build a lag-importance plot (filled area under the curve) for a
# single feature. The x-axis is reversed so lag=0 (today) is on the
# right and time runs "leftward" into the past.
plot_lag_importance <- function(df_feature, feature_title, curve_color, fill_alpha) {
  ggplot(df_feature, aes(x = lag, y = importance)) +
    geom_area(fill = curve_color, alpha = fill_alpha) +
    geom_line(color = curve_color, linewidth = 1) +
    geom_point(color = curve_color, size = 1.2) +
    scale_x_reverse() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      x = "Lag [Days Before Target Date]",
      y = "ALE Importance [\u00B0C]",
      title = feature_title
    )
}

# Save a plot for a feature as SVG.
save_lag_importance_plot <- function(plot, dir_figures, feature, filetype = "svg") {
  dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
  extra <- if (!is.null(feature)) paste0("_", feature) else ""
  ggsave(
    filename = file.path(dir_figures, paste0("int_lag_importance", extra, ".", filetype)),
    plot = plot,
    width = 10.5,
    height = 3.5,
    units = "in"
  )
}


# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------
df_all <- load_lag_importance_data(DIR_RESULTS, RESULTS_CSV_NAME)

lag_importance_plots <- list()

for (feat in FEATURES_TO_PLOT) {
  df_feature <- df_all %>% filter(feature == feat)
  feature_title <- unique(df_feature$feature_title)
  
  p <- plot_lag_importance(df_feature, feature_title, curve_color, fill_alpha)
  
  lag_importance_plots[[feat]] <- p
  
  # save_lag_importance_plot(p, DIR_FIGURES, feat, "svg")
  # save_lag_importance_plot(p, DIR_FIGURES, feat, "pdf")
}

p_lag_imp_combined <- wrap_plots(lag_importance_plots, ncol = 2) +
  plot_layout(axis_titles = "collect")

save_lag_importance_plot(p_lag_imp_combined, DIR_FIGURES, NULL, "svg")
save_lag_importance_plot(p_lag_imp_combined, DIR_FIGURES, NULL, "pdf")
