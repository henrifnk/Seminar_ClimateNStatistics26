# ======================================================================
# Plot partial dependence (PDP) + ICE curves for selected dynamic
# features.
#
# Reads the tidy CSV produced by compute_partial_dependence.py and
# creates one individual plot per selected feature (ICE lines + PDP
# mean curve), saved as SVG
#
# ======================================================================

library(ggplot2)
library(dplyr)
library(readr)
source("R/settings.R")

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
DIR_RESULTS <- "data/results"
RESULTS_CSV_NAME <- "partial_dependence.csv"

DIR_FIGURES <- "figures"

# Only these features get individual plots
FEATURES_TO_PLOT <- c("Ta_C", "rad_whm2")

# Dedicated colors for PDP / ICE curves (distinct from the
# dynamic/static importance colors)
pdp_color <- "red"
ice_color <- "#8C8C8C"

N_ICE_LINES <- 200  # max number of individual ICE lines to draw per plot
set.seed(149)          # reproducible subsampling of ICE lines


# ----------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------

# Load the tidy partial dependence results CSV.
load_pdp_data <- function(dir_results, filename) {
  read_csv(file.path(dir_results, filename), show_col_types = FALSE)
}

# Subsample sample_ids for ICE line plotting (avoids overplotting).
sample_ice_ids <- function(df, n_ice_lines) {
  all_ids <- unique(df$sample_id)
  if (length(all_ids) <= n_ice_lines) {
    return(all_ids)
  }
  sample(all_ids, size = n_ice_lines, replace = FALSE)
}

# Build an ICE + PDP plot for a single feature.
plot_partial_dependence <- function(df_feature, feature_title, unit_label,
                                    pdp_color, ice_color, n_ice_lines) {
  ice_ids <- sample_ice_ids(df_feature, n_ice_lines)
  df_ice <- df_feature %>% filter(sample_id %in% ice_ids)
  
  df_pdp <- df_feature %>%
    group_by(x_val) %>%
    summarise(mean_diff = mean(diff, na.rm = TRUE), n_valid = sum(!is.na(diff)), .groups = "drop")
  
  n_valid_total <- length(unique(df_feature$sample_id[!is.na(df_feature$diff)]))
  
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
    geom_line(
      data = df_ice,
      aes(x = x_val, y = diff, group = sample_id),
      color = ice_color, alpha = 0.10, linewidth = 0.3
    ) +
    geom_line(
      data = df_pdp,
      aes(x = x_val, y = mean_diff, color = "PDP"),
      linewidth = 1.0
    ) +
    geom_point(
      data = df_pdp,
      aes(x = x_val, y = mean_diff, color = "PDP"),
      size = 1.8
    ) +
    scale_color_manual(
      name = NULL,
      values = c("PDP" = pdp_color),
      labels = c("PDP" = paste0("PDP (n=", n_valid_total, ")"))
    ) +
    labs(
      x = unit_label,
      y = "Change in Water Temperature [\u00B0C]",
      title = feature_title
    ) +
    theme(legend.position = c(0.12, 0.95))
}

# Save a plot for a feature.
save_pdp_plot <- function(plot, dir_figures, feature, filetype = "svg") {
  dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(dir_figures, paste0("int_pdp_", feature, ".", filetype)),
    plot = plot,
    width = 6,
    height = 4,
    units = "in"
  )
}


# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------
df_all <- load_pdp_data(DIR_RESULTS, RESULTS_CSV_NAME)

for (feat in FEATURES_TO_PLOT) {
  df_feature <- df_all %>% filter(feature == feat)
  
  feature_title <- unique(df_feature$feature_title)
  unit_label <- unique(df_feature$unit_label)
  
  p <- plot_partial_dependence(df_feature, feature_title, unit_label,
                               pdp_color, ice_color, N_ICE_LINES)
  
  save_pdp_plot(p, DIR_FIGURES, feat)
}
