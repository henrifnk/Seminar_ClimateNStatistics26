library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(scales)
library(purrr)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(patchwork)
source("R/settings.R")

relabel <- function(x) {
  ifelse(x %in% names(all_labels), all_labels[x], x)
}

relabel_station <- function(x) {
  ifelse(x %in% names(station_labels), station_labels[x], x)
}

# ------------------------------------------------------------------
# Part I: Create Joined/ Preprocessed Data Frame
# ------------------------------------------------------------------
path_raw <- "data/raw"
path_intermediate <- "data/intermediate"

data_files <- list.files(path_raw)

data_dynamic <- do.call(rbind, lapply(data_files[data_files != "static_features.csv"],
                                      function(file) {
                                        read.csv(file.path(path_raw, file)) %>%
                                          mutate(Station = str_extract(file, pattern = ".*(?=\\.csv)"))
                                      }))

static_features_df <- read_delim(file.path(path_raw, "static_features.csv"),
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(-Kommentar)

data_dynamic <- data_dynamic %>%
  filter(!is.na(year)) %>%
  mutate(date_time = make_datetime(year, month, day, hour, tz = "UTC")) %>%
  group_by(Station) %>%
  complete(date_time = seq(
    from = floor_date(min(date_time, na.rm = TRUE), unit = "day"),
    to = ceiling_date(max(date_time, na.rm = TRUE), unit = "day") - hours(3),
    by = "3 hours"
  )) %>%
  ungroup() %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(Station, date) %>%
  summarise(
    wt = mean(wt),
    Ta_C = mean(Ta_C),
    P_mm = sum(P_mm),
    wind_ms = mean(wind_ms),
    rad_whm2 = sum(rad_whm2),
    relhum = mean(relhum),
    Q = mean(Q),
    .groups = "drop"
  )

dat <- data_dynamic %>%
  left_join(static_features_df, by = "Station")

write.csv(dat, file = file.path(path_intermediate, "data_processed.csv"), row.names = FALSE)


# ------------------------------------------------------------------
# Part II: Descriptive plots
# ------------------------------------------------------------------
val_start <- as.Date("2016/12/01")
test_start <- as.Date("2018/12/01")
dir_descr_plots <- "work/06-interpretable_heat/figures"
target <- "wt"
dynamic_features <- c("Ta_C", "P_mm", "wind_ms", "rad_whm2", "relhum", "Q")
static_features <- c("DEM", "Slope", "Fraction_Forest",
                     "Imperv_500m", "Imperv_1000m", "Imperv_2000m", "Imperv_3000m",
                     "Flusskilometer", "Gesamtlänge Fluss", "upstream_km")
vars_all <- c(target, dynamic_features, static_features)


## Part 1: Station Periods -----------------------------------------
station_periods <- dat %>%
  group_by(Station) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    n_years = as.numeric((max(date) - min(date)) / 365.25)
  ) %>%
  mutate(Station_label = relabel_station(Station))

plot_station_periods <- station_periods %>%
  ggplot(aes(y = reorder(Station_label, start_date))) +
  geom_linerange(aes(xmin = start_date, xmax = end_date),
                 color = "orange", linewidth = 4, alpha = 0.8) +
  geom_point(aes(x = start_date), color = "darkorange", size = 3) +
  geom_point(aes(x = end_date), color = "darkorange", size = 3) +
  geom_vline(
    xintercept = c(val_start, test_start),
    linetype = "dashed",
    color = "darkblue",
    linewidth = 0.8
  ) +
  labs(x = NULL, y = "Station")

# ggsave(filename = file.path(dir_descr_plots, "stations_periods.pdf"), plot_station_periods)
ggsave(filename = file.path(dir_descr_plots, "stations_periods.svg"), plot_station_periods, 
       width = 8, height = 4)


## Part 2: Missing Values -------------------------------------------
df_na <- dat %>%
  select(Station, date, all_of(c(target, dynamic_features))) %>%
  pivot_longer(cols = all_of(c(target, dynamic_features)),
               names_to = "variable",
               values_to = "value") %>%
  mutate(status = ifelse(is.na(value), "missing", "available"),
         Station_label = relabel_station(Station),
         variable_label = relabel(variable))

### Overview Heatmap: % missing per station/ variable
summary_na <- df_na %>%
  group_by(Station_label, variable_label) %>%
  summarise(pct_missing = mean(is.na(value)), .groups = "drop")

p_summary <- ggplot(summary_na, aes(x = variable_label, y = Station_label, fill = pct_missing)) +
  geom_tile(color = "white") +
  geom_text(aes(label = percent(pct_missing, accuracy = 1)), size = 2.8, color = "black") +
  scale_fill_viridis_c(option = "magma", direction = -1, labels = percent,
                       name = "% missing") +
  labs(x = NULL, y = NULL) +
  # theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave(file.path(dir_descr_plots, "summary_missing.png"), p_summary, width = 8, height = 7, dpi = 600)


### Check, what variables have identical temporal NA pattern
na_matrix <- df_na %>%
  select(Station, date, variable, status) %>%
  pivot_wider(names_from = variable, values_from = status)

vars <- dynamic_features %>% union(target)
identical_groups <- list()
checked <- c()

for (v1 in vars) {
  if (v1 %in% checked) next
  group <- v1
  for (v2 in setdiff(vars, c(checked, v1))) {
    if (all(na_matrix[[v1]] == na_matrix[[v2]], na.rm = TRUE)) {
      group <- c(group, v2)
    }
  }
  identical_groups[[v1]] <- group
  checked <- c(checked, group)
}

print(identical_groups)

var_to_group_df <- imap_dfr(identical_groups, ~ tibble(variable = .x, group = paste(.x, collapse = ", ")))
var_to_group <- deframe(var_to_group_df)

print(var_to_group)

dat_long_reduced <- df_na %>%
  mutate(variable_group = var_to_group[variable]) %>%
  distinct(Station, date, variable_group, status) %>%
  rename(variable = variable_group) %>%
  mutate(Station_label = relabel_station(Station))

### Detailed plot: one separate file per variable-group panel
panel_groups <- unique(dat_long_reduced$variable)

for (grp in panel_groups) {
  dat_panel <- dat_long_reduced %>% filter(variable == grp)
  n_stations <- n_distinct(dat_panel$Station_label)

  # relabel each individual variable inside the group string,
  # e.g. "Ta_C, wt" -> "Air Temperature, Water Temperature"
  grp_vars <- str_split(grp, ",\\s*")[[1]]
  grp_label <- paste(relabel(grp_vars), collapse = ", ")

  p_detail <- ggplot(dat_panel, aes(x = date, y = Station_label, fill = status)) +
    geom_tile() +
    scale_fill_manual(values = c("missing" = "firebrick", "available" = "grey85")) +
    labs(x = "Date", y = NULL, fill = NULL, title = grp_label) +
    # theme_minimal(base_size = 9) +
    theme(
      panel.spacing = unit(0.6, "lines"),
      legend.position = "bottom",
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 15),
      title = element_text(size = 15)
    )
  
  safe_name <- str_replace_all(grp, "[^A-Za-z0-9]+", "_")
  fig_height = (n_stations + 3) * 0.4
  ggsave(file.path(dir_descr_plots, paste0("stations_missings_", safe_name, ".svg")), p_detail,
         width = 2 * fig_height, height = fig_height)
}



## Part 3: Correlation -----------------------------------------------
cor_mat <- dat %>%
  select(all_of(vars_all)) %>%
  rename_with(relabel) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

cor_all <- ggcorrplot(cor_mat,
                      hc.order = TRUE,
                      type = "lower",
                      lab = TRUE, lab_size = 2.5,
                      colors = c("#2166AC", "white", "#B2182B"))

ggsave(file.path(dir_descr_plots, "corr_vars_heatmap.svg"), cor_all, width = 8, height = 7)
