# ======================================================================
# Plot a 2D Partial Dependence (PDP) surface as a heatmap
#
# Reads the CSV produced by compute_partial_dependence_2d.py and
# creates one heatmap for the joint effect of FEATURE_J and FEATURE_K on
# the target, saved as SVG.
# ======================================================================

library(ggplot2)
library(dplyr)
library(readr)

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
DIR_RESULTS <- "data/results"
RESULTS_CSV_NAME <- "partial_dependence_2d.csv"

DIR_FIGURES <- "figures"

# Diverging color scale, centered at 0 (no change in prediction).
# Cooling effects (negative) in blue, warming effects (positive) in red.
FILL_LOW <- "#2a78d6"
FILL_MID <- "#FFFFFF"
FILL_HIGH <- "#B3202A"


# ----------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------

#' Load the 2D PDP results CSV.
load_pdp_2d_data <- function(dir_results, filename) {
  read_csv(file.path(dir_results, filename), show_col_types = FALSE)
}

#' Build a heatmap of the 2D PDP surface (mean_diff over x_val_j / x_val_k).
#'
#' x_val_j / x_val_k are treated as DISCRETE axes (not continuous). The
#' underlying offset/factor grids are unevenly spaced (e.g. -10, -8, -5,
#' -3, -2, -1, 0, 1, 2, 3, 5, 8, 10), and geom_tile() on a continuous
#' axis sizes every tile using the *smallest* gap between values, which
#' leaves visible gaps wherever two neighboring grid values are farther
#' apart. Using factors gives every tile equal width/height regardless
#' of the actual numeric spacing.
plot_pdp_2d_heatmap <- function(df, feature_j_title, feature_k_title,
                                fill_low, fill_mid, fill_high) {
  max_abs <- max(abs(df$mean_diff), na.rm = TRUE)
  
  df <- df %>%
    mutate(
      x_val_j = factor(x_val_j, levels = sort(unique(x_val_j))),
      x_val_k = factor(x_val_k, levels = sort(unique(x_val_k)))
    )
  
  ggplot(df, aes(x = x_val_j, y = x_val_k, fill = mean_diff)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", mean_diff)), size = 2.8, color = "black") +
    scale_fill_gradient2(
      name = "\u0394 Water Temp. (\u00B0C)",
      low = fill_low, mid = fill_mid, high = fill_high,
      midpoint = 0, limits = c(-max_abs, max_abs)
    ) +
    labs(
      x = paste0(feature_j_title, " (offset)"),
      y = paste0(feature_k_title, " (factor)"),
      title = paste("2D Partial Dependence:", feature_j_title, "\u00D7", feature_k_title)
    ) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

#' Save the heatmap as SVG.
save_pdp_2d_plot <- function(plot, dir_figures, feature_j, feature_k) {
  dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(dir_figures, paste0("int_pdp2d_", feature_j, "_", feature_k, ".svg")),
    plot = plot,
    width = 8,
    height = 6,
    units = "in"
  )
}


# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------
df <- load_pdp_2d_data(DIR_RESULTS, RESULTS_CSV_NAME)

feature_j <- unique(df$feature_j)
feature_k <- unique(df$feature_k)
feature_j_title <- unique(df$feature_j_title)
feature_k_title <- unique(df$feature_k_title)

p <- plot_pdp_2d_heatmap(df, "Air Temperature", "Solar Radiation", FILL_LOW, FILL_MID, FILL_HIGH)

save_pdp_2d_plot(p, DIR_FIGURES, feature_j, feature_k)
