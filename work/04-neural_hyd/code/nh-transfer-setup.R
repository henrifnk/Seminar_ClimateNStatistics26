library(ggplot2)
library(knitr)

# Standalone mode control:
# - default: auto-detect from missing result CSVs
# - force no-data mode: set env NH_NO_DATA_MODE=true before render
nh_force_no_data <- tolower(Sys.getenv("NH_NO_DATA_MODE", "false")) %in% c("1", "true", "yes")

camels_topo_candidates <- c(
  "work/04-neural_hyd/results/camels_topo.txt"
)
camels_topo_path <- camels_topo_candidates[file.exists(camels_topo_candidates)][1]
if (is.na(camels_topo_path) || length(camels_topo_path) == 0) {
  camels_topo_path <- NA_character_
}

theme_nh <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

lmu_blue <- "#00558C"
lmu_light <- "#6AB0DE"
grey50 <- "#888888"
green_ok <- "#388E3C"
amber <- "#F57F17"

nh_safe_read_csv <- function(path, fallback) {
  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE)
  } else {
    fallback
  }
}

nh_groups <- sprintf("%02d", 1:18)
nh_empty_group_summary <- data.frame(
  group = nh_groups,
  n_basins = rep(NA_integer_, length(nh_groups)),
  local_nse_mean = rep(NA_real_, length(nh_groups)),
  global_nse_mean = rep(NA_real_, length(nh_groups)),
  ft_nse_mean = rep(NA_real_, length(nh_groups)),
  local_kge_mean = rep(NA_real_, length(nh_groups)),
  global_kge_mean = rep(NA_real_, length(nh_groups)),
  ft_kge_mean = rep(NA_real_, length(nh_groups)),
  delta_ft_vs_local_nse = rep(NA_real_, length(nh_groups)),
  delta_ft_vs_global_nse = rep(NA_real_, length(nh_groups)),
  stringsAsFactors = FALSE
)

nh_empty_all_basins <- data.frame(
  group = character(0),
  basin = character(0),
  local_nse = numeric(0),
  ft_nse = numeric(0),
  stringsAsFactors = FALSE
)

nh_empty_local_ft <- data.frame(
  basin = character(0),
  local_nse = numeric(0),
  ft_nse = numeric(0),
  stringsAsFactors = FALSE
)

nh_empty_global_eval <- data.frame(
  basin = character(0),
  NSE = numeric(0),
  KGE = numeric(0),
  stringsAsFactors = FALSE
)

nh_results_csv <- "work/04-neural_hyd/results/results_folder_groups_summary_with_global_true_ea.csv"
nh_has_results_data <- (!nh_force_no_data) && file.exists(nh_results_csv)

transfer_benchmark <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_groups_summary_with_global_true_ea.csv",
  nh_empty_group_summary
)
transfer_all_basins <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_groups_all_basins_ea.csv",
  nh_empty_all_basins
)

transfer_group_cuda <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_groups_summary_with_global_true.csv",
  nh_empty_group_summary
)
transfer_all_cuda <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_groups_all_basins.csv",
  nh_empty_all_basins
)

g05_local_ft <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_05_local_vs_finetune_ea.csv",
  nh_empty_local_ft
)
g05_global <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_05_global_eval_true_ea.csv",
  nh_empty_global_eval
)

g01_local_ft_cuda <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_01_local_vs_finetune.csv",
  nh_empty_local_ft
)
g01_global_cuda <- nh_safe_read_csv(
  "work/04-neural_hyd/results/results_folder_01_global_eval_true.csv",
  nh_empty_global_eval
)

transfer_benchmark$group <- sprintf("%02d", as.integer(transfer_benchmark$group))
transfer_all_basins$group <- sprintf("%02d", as.integer(transfer_all_basins$group))
transfer_group_cuda$group <- sprintf("%02d", as.integer(transfer_group_cuda$group))
transfer_all_cuda$group <- sprintf("%02d", as.integer(transfer_all_cuda$group))

g05_local_ft$basin <- sprintf("%08d", as.integer(g05_local_ft$basin))
g05_global$basin <- sprintf("%08d", as.integer(g05_global$basin))
g05_merged <- merge(g05_local_ft, g05_global[, c("basin", "NSE", "KGE")], by = "basin", all.x = TRUE)

g01_local_ft_cuda$basin <- sprintf("%08d", as.integer(g01_local_ft_cuda$basin))
g01_global_cuda$basin <- sprintf("%08d", as.integer(g01_global_cuda$basin))
g01_merged_cuda <- merge(g01_local_ft_cuda, g01_global_cuda[, c("basin", "NSE", "KGE")], by = "basin", all.x = TRUE)

wmean <- function(x, w) {
  if (length(x) == 0 || length(w) == 0) {
    return(NA_real_)
  }
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok) || sum(w[ok]) == 0) {
    return(NA_real_)
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

overall_local_nse <- wmean(transfer_benchmark$local_nse_mean, transfer_benchmark$n_basins)
overall_global_nse <- wmean(transfer_benchmark$global_nse_mean, transfer_benchmark$n_basins)
overall_ft_nse <- wmean(transfer_benchmark$ft_nse_mean, transfer_benchmark$n_basins)

overall_local_kge <- wmean(transfer_benchmark$local_kge_mean, transfer_benchmark$n_basins)
overall_global_kge <- wmean(transfer_benchmark$global_kge_mean, transfer_benchmark$n_basins)
overall_ft_kge <- wmean(transfer_benchmark$ft_kge_mean, transfer_benchmark$n_basins)

overall_local_nse_cuda <- wmean(transfer_group_cuda$local_nse_mean, transfer_group_cuda$n_basins)
overall_global_nse_cuda <- wmean(transfer_group_cuda$global_nse_mean, transfer_group_cuda$n_basins)
overall_ft_nse_cuda <- wmean(transfer_group_cuda$ft_nse_mean, transfer_group_cuda$n_basins)

overall_local_kge_cuda <- wmean(transfer_group_cuda$local_kge_mean, transfer_group_cuda$n_basins)
overall_global_kge_cuda <- wmean(transfer_group_cuda$global_kge_mean, transfer_group_cuda$n_basins)
overall_ft_kge_cuda <- wmean(transfer_group_cuda$ft_kge_mean, transfer_group_cuda$n_basins)

transfer_benchmark$delta_ft_local_nse <- transfer_benchmark$ft_nse_mean - transfer_benchmark$local_nse_mean
transfer_benchmark$delta_ft_global_nse <- transfer_benchmark$ft_nse_mean - transfer_benchmark$global_nse_mean
transfer_benchmark$delta_ft_local_kge <- transfer_benchmark$ft_kge_mean - transfer_benchmark$local_kge_mean
transfer_benchmark$delta_ft_global_kge <- transfer_benchmark$ft_kge_mean - transfer_benchmark$global_kge_mean

g05_summary <- transfer_benchmark[transfer_benchmark$group == "05", ]
nh_value_or_na <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(x[1])
}

g05_local_mean <- nh_value_or_na(g05_summary$local_nse_mean)
g05_global_mean <- nh_value_or_na(g05_summary$global_nse_mean)
g05_ft_mean <- nh_value_or_na(g05_summary$ft_nse_mean)
g05_ft_local_delta <- nh_value_or_na(g05_summary$delta_ft_vs_local_nse)
g05_ft_global_delta <- nh_value_or_na(g05_summary$delta_ft_vs_global_nse)

# Reporting scope for this chapter (aligned with text): 531 basins.
reported_basin_count <- 531L
