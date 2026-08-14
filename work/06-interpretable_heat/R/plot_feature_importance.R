# ======================================================================
# Plot permutation feature importance (dynamic + static)
#
# Reads the CSV produced by compute_permutation_importance.py and creates
# two horizontal bar plots (one per feature type) and saves them as SVG files.
# ======================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(forcats)

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
DIR_RESULTS <- "data/results"
RESULTS_CSV_NAME <- "permutation_importance.csv"

DIR_FIGURES <- "figures"

colors <- list(
  dynamic = "#2a78d6",
  static = "#FFA500"
)

## Variable labels
dynamic_feature_titles <- c(
  Ta_C = "Air Temperature",
  P_mm = "Precipitation",
  wind_ms = "Wind Speed",
  rad_whm2 = "Solar Radiation",
  relhum = "Relative Humidity",
  Q = "Discharge"
)

static_feature_titles <- c(
  DEM = "Elevation",
  Slope = "Slope",
  Fraction_Forest = "Forest Cover",
  Imperv_500m = "Impervious Surface (500m)",
  Imperv_1000m = "Impervious Surface (1000m)",
  Imperv_2000m = "Impervious Surface (2000m)",
  Imperv_3000m = "Impervious Surface (3000m)",
  Flusskilometer = "Distance from River Mouth",
  upstream_km = "Distance from River Source",
  Gesamtlaenge_Fluss = "Total River Length"
)

set_theme(theme_minimal() +
            theme(
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              strip.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 11)
            ))


# ----------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------

# Load the permutation importance results CSV.
load_importance_data <- function(dir_results, filename) {
  read_csv(file.path(dir_results, filename), show_col_types = FALSE)
}

# Build a horizontal bar plot of feature importance (sorted descending,
# most important feature at the top), including SD error bars and a
# zero reference line.
barplot_importance <- function(df, title_map, color, plot_title) {
  df <- df %>%
    mutate(feature_label = recode(feature, !!!title_map, .default = feature)) %>%
    mutate(feature_label = fct_reorder(feature_label, mean_importance))
  
  ggplot(df, aes(x = mean_importance, y = feature_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
    geom_col(fill = color, color = "black", linewidth = 0.3, width = 0.7) +
    labs(x = "Importance (NSE decrease)", y = NULL, title = plot_title)
}

# Save a plot as SVG, sizing the height based on number of features.
save_importance_plot <- function(plot, dir_figures, filename, n_features) {
  dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(dir_figures, filename),
    plot = plot,
    width = 7,
    height = 0.45 * n_features + 1,
    units = "in"
  )
}


# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------
df_all <- load_importance_data(DIR_RESULTS, RESULTS_CSV_NAME)

df_dynamic <- df_all %>% filter(feature_type == "dynamic")
df_static <- df_all %>% filter(feature_type == "static")

plot_dynamic <- barplot_importance(
  df_dynamic, dynamic_feature_titles, colors$dynamic,
  "Permutation Importance: Dynamic Features"
)
plot_static <- barplot_importance(
  df_static, static_feature_titles, colors$static,
  "Permutation Importance: Static Features"
)

save_importance_plot(plot_dynamic, DIR_FIGURES, "int_perm_importance_dynamic.svg", nrow(df_dynamic))
save_importance_plot(plot_static, DIR_FIGURES, "int_perm_importance_static.svg", nrow(df_static))