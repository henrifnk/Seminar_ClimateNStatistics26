library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)


# load static features

static_features <- read.csv("work/05-riverine_heat/data/data_raw/static_features.csv")
static_features
# load processed data

# salz <- readRDS("work/05-riverine_heat/data/data_processed/fraenkische_saale_salz.rds")
# wolfsmuenster <- readRDS("work/05-riverine_heat/data/data_processed/fraenkische_saale_wolfsmuenster.rds")
# schenkenau <- readRDS("work/05-riverine_heat/data/data_processed/Itz_Schenkenau.rds")
# frankfurt_osthafen <- readRDS("work/05-riverine_heat/data/data_processed/main_frankfurt_osthafen.rds")
kemmern <- readRDS("work/05-riverine_heat/data/data_processed/main_kemmern.rds")
kleinheubach <- readRDS("work/05-riverine_heat/data/data_processed/main_kleinheubach.rds")
# krotzenburg <- readRDS("work/05-riverine_heat/data/data_processed/main_krotzenburg.rds")
# mainleus <- readRDS("work/05-riverine_heat/data/data_processed/main_mainleus.rds")
schweinfurt <- readRDS("work/05-riverine_heat/data/data_processed/main_schweinfurt.rds")
schwuerbitz <- readRDS("work/05-riverine_heat/data/data_processed/main_schwuerbitz.rds")
# steinbach <- readRDS("work/05-riverine_heat/data/data_processed/main_steinbach.rds")
wuerzburg <- readRDS("work/05-riverine_heat/data/data_processed/main_wuerzburg.rds")
pettstadt <- readRDS("work/05-riverine_heat/data/data_processed/regnitz_pettstadt.rds")
# sachsenheim <- readRDS("work/05-riverine_heat/data/data_processed/wern_sachsenheim.rds")

# print(salz, n = 1000)


# create list of dataframes

stations <- list(
  # salz = salz,
  # wolfsmuenster = wolfsmuenster,
  # schenkenau = schenkenau,
  # frankfurt_osthafen = frankfurt_osthafen,
  kemmern = kemmern ,
  kleinheubach = kleinheubach,
  # krotzenburg = krotzenburg,
  # mainleus = mainleus,
  schweinfurt = schweinfurt,
  schwuerbitz = schwuerbitz,
  # steinbach = steinbach,
  wuerzburg = wuerzburg,
  pettstadt = pettstadt
  # sachsenheim = sachsenheim
)

head(stations)


all_events <- map_dfr(names(stations), function(station_name) {
  
  df_station <- stations[[station_name]]
  
  df_station %>%
    filter(heatwave) %>%
    group_by(heatwave_id) %>%
    summarise(
      start_date = min(date),
      end_date = max(date),
      year = year(start_date),
      duration = n(),
      mean_intensity = mean(wt - threshold, na.rm = TRUE),
      max_intensity = max(wt - threshold, na.rm = TRUE),
      severity = sum(wt - threshold, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      station = station_name,
      .before = 1
    )
})

annual_summary <- all_events %>%
  group_by(station, year) %>%
  summarise(
    heatwave_events = n(),
    heatwave_days = sum(duration),
    mean_duration = mean(duration),
    max_duration = max(duration),
    mean_intensity = mean(mean_intensity),
    max_intensity = max(max_intensity),
    mean_severity = mean(severity),
    total_severity = sum(severity),
    .groups = "drop"
  )


all_years <- expand.grid(
  station = names(stations),
  year = 2005:2019
)

annual_summary <- all_years %>%
  left_join(annual_summary, by = c("station", "year")) %>%
  mutate(
    heatwave_events = replace_na(heatwave_events, 0),
    heatwave_days = replace_na(heatwave_days, 0),
    total_severity = replace_na(total_severity, 0)
  )

head(annual_summary)

frequency_summary <- annual_summary %>%
group_by(station) %>%
  summarise(
    mean_events = mean(heatwave_events),
    sd_events = sd(heatwave_events),
    mean_heatwave_days = mean(heatwave_days),
    .groups = "drop"
  )



all_events %>%
  group_by(station) %>%
  summarise(n = n_distinct(heatwave_id))
annual_summary
annual_summary %>%
  group_by(year) %>%
  summarise(n = sum(heatwave_events))

  
station_levels <- c(
  "schwuerbitz",
  "pettstadt",
  "kemmern",
  "schweinfurt",
  "wuerzburg",
  "kleinheubach"
)

annual_summary$station <- factor(
  as.character(annual_summary$station),
  levels = station_levels
)
annual_summary

str(annual_summary$station)
str(station_levels)

unique(as.character(annual_summary$station))
station_levels

setdiff(unique(as.character(annual_summary$station)), station_levels)

annual_summary$station <- factor(
  as.character(annual_summary$station),
  levels = station_levels
)

annual_summary$station
# print(annual_summary, max = 10)


# station labels fr plot

station_labels <- c(
  schwuerbitz = "Main: Schwürbitz",
  pettstadt = "Regnitz: Pettstadt",
  kemmern = "Main: Kemmern",
  schweinfurt = "Main: Schweinfurt",
  wuerzburg = "Main: Würzburg",
  kleinheubach = "Main: Kleinheubach"
)

station_levels <- c("schwuerbitz",
                    "pettstadt",
                    "kemmern",
                    "schweinfurt",
                    "wuerzburg",
                    "kleinheubach")


annual_summary$station <- factor(
  annual_summary$station,
  levels = station_levels
)
annual_summary

# 1. FREQUENCY

# overview

annual_summary %>%
  group_by(station) %>%
  summarise(
    mean_heatwave_events= mean(heatwave_events),
    sd_heatwave_events  = sd(heatwave_events),
    cv_heatwave_events   = sd(heatwave_events) / mean(heatwave_events)
  )


# annualy heatwave frequency by station jitter

heatwave_events_overview <- ggplot(
  annual_summary,
  aes(x = station, y = heatwave_events)
) +
  geom_jitter(
    width = 0.12,
    height = 0.08,
    size = 2.4,
    alpha = 0.65
  ) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    aes(
      ymin = after_stat(y),
      ymax = after_stat(y)
    ),
    width = 0.55,
    linewidth = 0.9,
    colour = "#D55E00"
  ) +
  scale_x_discrete(labels = station_labels) +
  scale_y_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_x_discrete(
    limits = station_levels,
    labels = station_labels
  )+
  labs(
    title = "Annual heatwave frequency by station",
    # subtitle = "Points represent individual years; horizontal markers represent station means",
    x = NULL,
    y = "Heatwave events per year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    #plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

heatwave_events_overview


# 2. DURATION

# overview

all_events %>%
  group_by(station) %>%
  summarise(
    mean_duration= mean(duration),
    sd_duration  = sd(duration),
    cv_duration  = sd(duration) / mean(duration)
  )


# plot

mean_duration_overview <- ggplot(
  all_events,
  aes(x = station, y = duration)
) +
  geom_boxplot(
    width = 0.42,
    outlier.shape = NA,
    fill = "grey90",
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    height = 0,
    alpha = 0.7,
    size = 2.3
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3.5,
    colour = "#D55E00"
  ) +
  scale_x_discrete(labels = station_labels) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_x_discrete(
    limits = station_levels,
    labels = station_labels
  )+
  labs(
    title = "Heatwave duration by station",
    # subtitle = "Points represent individual heatwave events; diamonds show station means",
    x = NULL,
    y = "Heatwave duration (days)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    #plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

mean_duration_overview

# 3. INTENSITY

all_events %>%
  group_by(station) %>%
  summarise(
    mean_mean_intensity = mean(mean_intensity),
    sd_intensity  = sd(mean_intensity),
    cv_intensity   = sd(mean_intensity) / mean(mean_intensity)
  )

# plot

mean_intensity_overview <- ggplot(all_events,
       aes(station, mean_intensity)) +
  geom_boxplot(
    width = 0.4,
    outlier.shape = NA,
    fill = "grey90",
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.7,
    size = 2.2
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3.5,
    colour = "#D55E00"
  ) +
  scale_x_discrete(labels = station_labels) +
  labs(
    title = "Heatwave intensity by station",
    # subtitle = "Points represent individual heatwave events; diamonds indicate station means",
    x = NULL,
    y = expression("Heatwave intensity (" * degree * "C)")
  ) +
  scale_x_discrete(
    limits = station_levels,
    labels = station_labels
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(
    colour = "black",
    fill = NA,
    linewidth = 0.8
  ),
    #plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

mean_intensity_overview 

# 2. DOWNSTREAM DIFFERENCES

# downstream differences, only main data

station_summary <- annual_summary %>%
  group_by(station) %>%
  summarise(
    mean_events = mean(heatwave_events),
    mean_duration = mean(mean_duration, na.rm = TRUE),
    mean_intensity = mean(mean_intensity, na.rm = TRUE),
    mean_severity = mean(mean_severity, na.rm = TRUE),
    total_severity = mean(total_severity, na.rm = TRUE),
    .groups = "drop"
  )

kilometers_main <- c("frankfurt_osthafen" = 37.59,
                     "kemmern" = 390.93,
                     "kleinheubach" = 121.74,
                     "krotzenburg" = 63.23,
                     "mainleus" = 461.14,
                     "schweinfurt" = 330.78,
                     "schwuerbitz" = 438.29,
                     "steinbach" = 200.52,
                     "wuerzburg" = 251.97,
                     "krotzenburg" = 63.23)

station_summary <- station_summary %>%
  mutate(river_km = kilometers_main[station])
station_summary

station_summary <- station_summary %>%
  arrange(river_km)

# plots

# sort annual summary

annual_summary <- annual_summary %>%
  mutate(river_km = kilometers_main[station])


annual_summary <- annual_summary %>%
  arrange(river_km) %>%
  mutate(station = factor(station, levels = unique(station)))

station_summary


# radar chart

#1

station_labels <- c(
  kemmern = "Kemmern",
  kleinheubach = "Kleinheubach",
  schweinfurt = "Schweinfurt",
  schwuerbitz = "Schwürbitz",
  wuerzburg = "Würzburg",
  pettstadt = "Pettstadt"
)

#2

station_summary
metric_means <- station_summary %>%
  summarise(
    frequency_mean = mean(mean_events, na.rm = TRUE),
    duration_mean = mean(mean_duration, na.rm = TRUE),
    intensity_mean = mean(mean_intensity, na.rm = TRUE)
  )

metric_means

#3

radar_data <- station_summary %>%
  transmute(
    station,
    
    frequency = mean_events / metric_means$frequency_mean,
    duration = mean_duration / metric_means$duration_mean,
    intensity = mean_intensity / metric_means$intensity_mean
  )

radar_data

#4

radar_long <- radar_data %>%
  pivot_longer(
    cols = c(frequency, duration, intensity),
    names_to = "metric",
    values_to = "relative_value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "frequency",
        "duration",
        "intensity"
      )
    )
  )

#5

axis_information <- tibble(
  metric = factor(
    c("duration", "intensity", "frequency"),
    levels = c("frequency", "duration", "intensity")
  ),
  
  angle = c(
    pi / 2,
    -pi / 6,
    7 * pi / 6       
  ),
  
  metric_label = c(
    "Duration",
    "Intensity",
    "Frequency"
  )
)

radar_long <- radar_long %>%
  left_join(
    axis_information,
    by = "metric"
  ) %>%
  mutate(
    x = relative_value * cos(angle),
    y = relative_value * sin(angle)
  )

#6

radar_polygon <- radar_long %>%
  mutate(
    polygon_order = case_when(
      metric == "duration" ~ 1,
      metric == "intensity" ~ 2,
      metric == "frequency" ~ 3
    )
  ) %>%
  arrange(
    station,
    polygon_order
  )

#7

reference_polygon <- crossing(
  station = unique(radar_data$station),
  axis_information
) %>%
  mutate(
    relative_value = 1,
    
    polygon_order = case_when(
      metric == "duration" ~ 1,
      metric == "intensity" ~ 2,
      metric == "frequency" ~ 3
    ),
    
    x = cos(angle),
    y = sin(angle)
  ) %>%
  arrange(
    station,
    polygon_order
  )

#8

grid_levels <- c(0.5, 1.0, 1.5)

grid_polygons <- crossing(
  station = unique(radar_data$station),
  grid_level = grid_levels,
  axis_information
) %>%
  mutate(
    polygon_order = case_when(
      metric == "duration" ~ 1,
      metric == "intensity" ~ 2,
      metric == "frequency" ~ 3
    ),
    
    x = grid_level * cos(angle),
    y = grid_level * sin(angle)
  ) %>%
  arrange(
    station,
    grid_level,
    polygon_order
  )

#9

maximum_value <- max(
  1.5,
  radar_data$frequency,
  radar_data$duration,
  radar_data$intensity,
  na.rm = TRUE
)

maximum_value <- max(
  1.5,
  radar_data$frequency,
  radar_data$duration,
  radar_data$intensity,
  na.rm = TRUE
)


plot_limit <- maximum_value * 1.18

axis_lines <- crossing(
  station = unique(radar_data$station),
  axis_information
) %>%
  mutate(
    x_start = 0,
    y_start = 0,
    x_end = maximum_value * cos(angle),
    y_end = maximum_value * sin(angle)
  )

axis_labels <- crossing(
  station = unique(radar_data$station),
  axis_information
) %>%
  mutate(
    x = plot_limit * cos(angle),
    y = plot_limit * sin(angle),
    
    hjust = case_when(
      metric == "frequency" ~ 1,
      metric == "intensity" ~ 0,
      TRUE ~ 0.5
    ),
    
    vjust = case_when(
      metric == "duration" ~ 0,
      TRUE ~ 0.5
    )
  )

#10 plot

polygon_spatial <- ggplot() +
  geom_polygon(
    data = grid_polygons,
    aes(
      x = x,
      y = y,
      group = interaction(station, grid_level)
    ),
    fill = NA,
    colour = "grey85",
    linewidth = 0.4
  ) +
  geom_segment(
    data = axis_lines,
    aes(
      x = x_start,
      y = y_start,
      xend = x_end,
      yend = y_end
    ),
    colour = "grey80",
    linewidth = 0.4
  ) +
  geom_polygon(
    data = reference_polygon,
    aes(
      x = x,
      y = y,
      group = station
    ),
    fill = "grey70",
    colour = "grey45",
    alpha = 0.25,
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_polygon(
    data = radar_polygon,
    aes(
      x = x,
      y = y,
      group = station
    ),
    fill = "orange",
    colour = "orange",
    alpha = 0.25,
    linewidth = 1
  ) +
  geom_point(
    data = radar_polygon,
    aes(
      x = x,
      y = y
    ),
    colour = "orange",
    size = 2.3
  ) +
  geom_text(
    data = axis_labels,
    aes(
      x = x,
      y = y,
      label = metric_label,
      hjust = hjust,
      vjust = vjust
    ),
    size = 3.7,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ station,
    labeller = as_labeller(station_labels),
    ncol = 3
  ) +
  
  coord_equal(
    xlim = c(-plot_limit, plot_limit),
    ylim = c(-plot_limit, plot_limit),
    clip = "off"
  ) +
  
  labs(
    title = "Heatwave characteristics by station",
    subtitle = paste(
      "Values are expressed relative to the mean across all stations;",
      "the dashed grey triangle represents the overall mean"
    ),
    caption = paste(
      "1.0 = mean across all stations;",
      "values above 1 indicate above-average conditions"
    )
  ) +
  
  theme_void(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 11,
      margin = margin(
        b = 15
      )
    ),
    
    plot.caption = element_text(
      size = 9,
      colour = "grey35",
      hjust = 0
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 12,
      margin = margin(
        b = 8
      )
    ),
    
    panel.spacing = unit(
      1.5,
      "lines"
    ),
    
    plot.margin = margin(
      15,
      20,
      15,
      20
    )
  )

ggsave(
  "work/05-riverine_heat/figures/heatwave_events_overview.pdf",
  plot = heatwave_events_overview,
  width = 5,
  height = 4
)
heatwave_events_overview

ggsave(
  "work/05-riverine_heat/figures/polygon_spatial.pdf",
  plot = polygon_spatial,
  width = 5,
  height = 4
)
polygon_spatial

ggsave(
  "work/05-riverine_heat/figures/mean_duration_overview.pdf",
  plot = mean_duration_overview,
  width = 5,
  height = 4
)
mean_duration_overview
ggsave(
  "work/05-riverine_heat/figures/mean_intensity_overview.pdf",
  plot = mean_intensity_overview,
  width = 5,
  height = 4
)
mean_intensity_overview

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. Prepare yearly radar data
# ============================================================

radar_data_year <- annual_summary %>%
  group_by(year) %>%
  summarise(
    frequency = mean(heatwave_events, na.rm = TRUE),
    duration  = mean(mean_duration, na.rm = TRUE),
    intensity = mean(mean_intensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

# Relative to overall mean (= reference triangle)

radar_data_year <- radar_data_year %>%
  mutate(
    frequency = frequency / mean(frequency),
    duration  = duration  / mean(duration),
    intensity = intensity / mean(intensity)
  )

# Chronological order

radar_data_year$year <- factor(
  radar_data_year$year,
  levels = sort(unique(radar_data_year$year))
)

# ============================================================
# 2. Determine common scale
# ============================================================

maximum_value <- max(
  radar_data_year$frequency,
  radar_data_year$duration,
  radar_data_year$intensity
)

maximum_value <- ceiling(maximum_value * 10) / 10

grid_levels <- c(0.5, 1.0, 1.5)

if(maximum_value > 1.5){
  grid_levels <- c(grid_levels, maximum_value)
}

# ============================================================
# 3. Convert to long format
# ============================================================

plot_data <- radar_data_year %>%
  pivot_longer(
    cols = c(frequency, duration, intensity),
    names_to = "metric",
    values_to = "value"
  )

plot_data$metric <- factor(
  plot_data$metric,
  levels = c("duration", "intensity", "frequency")
)

angles <- c(
  duration = 90,
  intensity = 330,
  frequency = 210
)

plot_data <- plot_data %>%
  mutate(
    angle = angles[metric],
    angle_rad = angle * pi / 180,
    x = value * cos(angle_rad),
    y = value * sin(angle_rad)
  )

plot_data <- plot_data %>%
  group_by(year) %>%
  arrange(metric) %>%
  bind_rows(slice(., 1)) %>%
  ungroup()

# ============================================================
# 4. Reference triangle (overall mean = 1)
# ============================================================

reference <- data.frame(
  metric = factor(
    c("duration", "intensity", "frequency", "duration"),
    levels = c("duration", "intensity", "frequency")
  ),
  value = 1
)

reference <- reference %>%
  mutate(
    angle = angles[metric],
    angle_rad = angle * pi / 180,
    x = value * cos(angle_rad),
    y = value * sin(angle_rad)
  )

# ============================================================
# 5. Background grid
# ============================================================

grid <- expand.grid(
  metric = factor(
    c("duration", "intensity", "frequency"),
    levels = c("duration", "intensity", "frequency")
  ),
  level = grid_levels
)

grid <- grid %>%
  mutate(
    angle = angles[metric],
    angle_rad = angle * pi / 180,
    x = level * cos(angle_rad),
    y = level * sin(angle_rad)
  )

grid <- grid %>%
  group_by(level) %>%
  arrange(metric) %>%
  bind_rows(slice(., 1)) %>%
  ungroup()

# ============================================================
# 6. Axes
# ============================================================

axes <- data.frame(
  metric = c("duration", "intensity", "frequency"),
  x = maximum_value * cos(angles * pi / 180),
  y = maximum_value * sin(angles * pi / 180)
)

labels <- data.frame(
  metric = c("Duration", "Intensity", "Frequency"),
  x = (maximum_value + 0.20) * cos(angles * pi / 180),
  y = (maximum_value + 0.20) * sin(angles * pi / 180)
)

# ============================================================
# 7. Plot
# ============================================================

ggplot() +
  
  geom_polygon(
    data = grid,
    aes(x, y, group = level),
    fill = NA,
    colour = "grey90",
    linewidth = 0.4
  ) +
  
  geom_segment(
    data = axes,
    aes(
      x = 0,
      y = 0,
      xend = x,
      yend = y
    ),
    colour = "grey85",
    linewidth = 0.4
  ) +
  
  geom_polygon(
    data = reference,
    aes(x, y),
    fill = "grey70",
    alpha = 0.10,
    colour = "grey45",
    linewidth = 0.9,
    linetype = 2
  ) +
  
  geom_polygon(
    data = plot_data,
    aes(
      x,
      y,
      group = year
    ),
    fill = "#0072B2",
    alpha = 0.20,
    colour = "#0072B2",
    linewidth = 1.2
  ) +
  
  geom_point(
    data = plot_data,
    aes(x, y),
    colour = "#0072B2",
    size = 2
  ) +
  
  geom_text(
    data = labels,
    aes(x, y, label = metric),
    fontface = "bold",
    size = 4.5
  ) +
  
  coord_equal() +
  
  facet_wrap(
    ~year,
    ncol = 5
  ) +
  
  theme_void() +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    panel.spacing = unit(1.4, "lines")
  ) +
  
  labs(
    title = "Heatwave characteristics by year",
    subtitle = "Values are expressed relative to the mean across all years; the dashed grey triangle represents the overall mean"
  )

# TEMPORAL TRENDS

# 1. FREQUENCY


# faceted plot of all stations, including common quasi poisson model trend


# Create one prediction for every observed station-year row

glm_frequency <- glm(
  heatwave_events ~ year + station,
  family = quasipoisson(link = "log"),
  data = annual_summary
)


plot_data <- annual_summary %>%
  mutate(
    predicted = predict(
      glm_frequency,
      newdata = annual_summary,
      type = "response"
    )
  )

heatwave_events_plot <- ggplot(
  plot_data,
  aes(x = year, y = heatwave_events)
) +
  geom_point(size = 2) +
  geom_line(
    aes(
      y = predicted,
      group = station
    ),
    colour = "blue",
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ station,
    labeller = as_labeller(station_labels)
  ) +
  labs(
    title = "Heatwaves per year and station",
    subtitle = "Fitted quasi-Poisson model with a common temporal trend",
    x = "Year",
    y = "Heatwave days"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )
heatwave_events_plot

# 2. DURATION

# 2.1 mean duration

lm_duration <- lm(
  mean_duration ~ year + station,
  data = annual_summary
)


plot_data_dur <- annual_summary %>%
  mutate(
    predicted_dur = predict(
      lm_duration,
      newdata = annual_summary,
      type = "response"
    )
  )

duration_plot <- ggplot(
  plot_data_dur,
  aes(x = year, y = mean_duration)
) +
  geom_point(size = 2) +
  geom_line(
    aes(
      y = predicted_dur,
      group = station
    ),
    colour = "blue",
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ station,
    labeller = as_labeller(station_labels)
  ) +
  labs(
    title = "Mean duration per year and station",
    subtitle = "Fitted linear modell with a common temporal trend",
    x = "Year",
    y = "Mean Duration (days)"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

duration_plot

# 3. INTENSITY

# 3. 1 mean intensity

lm_intensity <- lm(
  mean_intensity ~ year + station,
  data = annual_summary
)

plot_data_int <- annual_summary %>%
  mutate(
    predicted_int = predict(
      lm_intensity,
      newdata = annual_summary,
      type = "response"
    )
  )


intensity_plot <- ggplot(
  plot_data_int,
  aes(x = year, y = mean_intensity)
) +
  geom_point(size = 2) +
  geom_line(
    aes(
      y = predicted_int,
      group = station
    ),
    colour = "blue",
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ station,
    labeller = as_labeller(station_labels)
  ) +
  labs(
    title = "Mean Intensity per year and station",
    subtitle = "Fitted linear model with a common temporal trend",
    x = "Year",
    y = "Mean intensity (°C)"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

intensity_plot

# 4. SEVERITY

# 4.1 mean severity over time

ggplot(annual_summary,
       aes(x = year, y = mean_severity)) +
  geom_line(
    colour = "grey60",
    linewidth = 0.5
  ) +
  geom_point(
    colour = "black",
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    alpha = 0.25
  ) +
  facet_wrap(
    ~station,
    labeller = as_labeller(station_labels)
  ) +
  labs(
    title = "Yearly mean heatwave severity per station",
    x = "Year",
    y = "Mean severity per heatwave (°C days)"
  ) +
  theme_minimal()

# 4.2 total annual severity

ggplot(annual_summary,
       aes(x = year, y = total_severity)) +
  geom_line(
    colour = "grey60",
    linewidth = 0.5
  ) +
  geom_point(
    colour = "black",
    size = 2
  )+
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    alpha = 0.25
  ) +
  facet_wrap(
    ~station,
    labeller = as_labeller(station_labels)
  ) +
  labs(
    title = "Yearly total heatwave severity per station",
    x = "Year",
    y = "Total annual severity (°C days)"
  ) +
  theme_minimal()

# SAVE PLOTS

ggsave(
  "work/05-riverine_heat/figures/heatwave_events_plot.pdf",
  plot = heatwave_events_plot,
  width = 5,
  height = 4
)
heatwave_events_plot
ggsave(
  "work/05-riverine_heat/figures/duration_plot.pdf",
  plot = duration_plot,
  width = 5,
  height = 4
)
duration_plot
ggsave(
  "work/05-riverine_heat/figures/intensity_plot.pdf",
  plot = intensity_plot,
  width = 5,
  height = 4
)
intensity_plot

