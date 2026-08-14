# ======================================================================
# Plot Accumulated Local Effects (ALE) curves - overall + seasonal, with
# a seasonal rug plot, for selected dynamic features
#
# Reads the tidy CSVs produced by compute_ale.py and creates one
# individual plot per selected feature, saved as SVG
# ======================================================================

library(ggplot2)
library(dplyr)
library(readr)
source("R/settings.R")

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
DIR_RESULTS <- "data/results"
CURVES_CSV_NAME <- "ale_curves.csv"
RUG_CSV_NAME <- "ale_rug.csv"

DIR_FIGURES <- "figures"

FEATURES_TO_PLOT <- c("Ta_C", "rad_whm2")

overall_color <- "#1a1a1a"

season_colors <- c(
  "Winter (DJF)" = "#1baf7a",
  "Spring (MAM)" = "#eda100",
  "Summer (JJA)" = "#e34948",
  "Fall (SON)"   = "#4a3aa7"
)
season_order <- names(season_colors)


# ----------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------

#' Load the ALE curves + rug CSVs.
load_ale_data <- function(dir_results, curves_filename, rug_filename) {
  list(
    curves = read_csv(file.path(dir_results, curves_filename), show_col_types = FALSE),
    rug = read_csv(file.path(dir_results, rug_filename), show_col_types = FALSE)
  )
}

#' For each season, keep only the edges within its occupied bin range
#' (i.e. drop edges where bin_count == 0), so season lines don't extend
#' beyond the x-range where that season actually has data.
trim_to_occupied_range <- function(df_season) {
  df_season %>%
    filter(edge_idx >= min(edge_idx[bin_count > 0]) &
             edge_idx <= max(edge_idx[bin_count > 0]) + 1)
}

#' Build an ALE plot (overall + seasonal curves + seasonal rug) for a
#' single feature.
plot_ale_feature <- function(df_curves_feature, df_rug_feature, feature_title,
                             unit_label, overall_color, season_colors, season_order) {
  df_overall <- df_curves_feature %>% filter(season == "overall")
  
  df_seasonal <- df_curves_feature %>%
    filter(season != "overall") %>%
    group_by(season) %>%
    group_modify(~ trim_to_occupied_range(.x)) %>%
    ungroup() %>%
    mutate(season = factor(season, levels = season_order))
  
  # Stack seasonal rug ticks below the plot, offset per season (matches
  # the vertically-staggered "|" rug in the original matplotlib version)
  y_range <- range(c(df_overall$ale_value, df_seasonal$ale_value), na.rm = TRUE)
  rug_step <- diff(y_range) * 0.03
  rug_base <- y_range[1] - rug_step * 5
  
  df_rug_feature <- df_rug_feature %>%
    mutate(season = factor(season, levels = season_order)) %>%
    mutate(y_pos = rug_base + (as.integer(season) - 1) * rug_step)
  
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
    geom_line(
      data = df_seasonal,
      aes(x = x_val, y = ale_value, color = season),
      linetype = "dashed", linewidth = 0.6
    ) +
    geom_point(
      data = df_seasonal,
      aes(x = x_val, y = ale_value, color = season),
      size = 1.3
    ) +
    geom_line(
      data = df_overall,
      aes(x = x_val, y = ale_value, color = "Overall"),
      linewidth = 1.1
    ) +
    geom_point(
      data = df_overall,
      aes(x = x_val, y = ale_value, color = "Overall"),
      size = 1.8
    ) +
    geom_point(
      data = df_rug_feature,
      aes(x = value, y = y_pos, color = season),
      shape = "|", size = 3, alpha = 0.5
    ) +
    scale_color_manual(
      name = NULL,
      values = c("Overall" = overall_color, season_colors),
      breaks = c("Overall", season_order)
    ) +
    labs(x = unit_label, y = "ALE [\u00B0C]", title = paste("Accumulated Local Effects:", feature_title)) +
    theme(legend.position = "bottom")
}

#' Save a plot for a feature.
save_ale_plot <- function(plot, dir_figures, feature, filetype = "svg") {
  dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(dir_figures, paste0("int_ale_", feature, ".", filetype)),
    plot = plot,
    width = 8,
    height = 5.5,
    units = "in"
  )
}


# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------
data <- load_ale_data(DIR_RESULTS, CURVES_CSV_NAME, RUG_CSV_NAME)

for (feat in FEATURES_TO_PLOT) {
  df_curves_feature <- data$curves %>% filter(feature == feat)
  df_rug_feature <- data$rug %>% filter(feature == feat)
  
  feature_title <- unique(df_curves_feature$feature_title)
  unit_label <- if (feat == "Ta_C") "Air Temperature [\u00B0C]" else feature_title
  
  p <- plot_ale_feature(df_curves_feature, df_rug_feature, feature_title, unit_label,
                        overall_color, season_colors, season_order)
  
  save_ale_plot(p, DIR_FIGURES, feat)
}
