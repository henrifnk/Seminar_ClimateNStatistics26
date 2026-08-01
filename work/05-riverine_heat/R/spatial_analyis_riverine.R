library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(Matrix)
library(lme4)
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


library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. Stationsnamen für die Facet-Beschriftung
# ------------------------------------------------------------

station_labels <- c(
  kemmern = "Kemmern",
  kleinheubach = "Kleinheubach",
  schweinfurt = "Schweinfurt",
  schwuerbitz = "Schwürbitz",
  wuerzburg = "Würzburg",
  pettstadt = "Pettstadt"
)

# Falls deine Stationsnamen noch "main_" oder "regnitz_" enthalten,
# passe den Vektor entsprechend an, zum Beispiel:
#
# station_labels <- c(
#   main_kemmern = "Kemmern",
#   main_kleinheubach = "Kleinheubach",
#   main_schweinfurt = "Schweinfurt",
#   main_schwuerbitz = "Schwürbitz",
#   main_wuerzburg = "Würzburg",
#   regnitz_pettstadt = "Pettstadt"
# )


# ------------------------------------------------------------
# 2. Mittelwerte über alle Stationen berechnen
# ------------------------------------------------------------
station_summary
metric_means <- station_summary %>%
  summarise(
    frequency_mean = mean(mean_events, na.rm = TRUE),
    duration_mean = mean(mean_duration, na.rm = TRUE),
    intensity_mean = mean(mean_intensity, na.rm = TRUE)
  )

metric_means


# ------------------------------------------------------------
# 3. Werte relativ zum Mittelwert standardisieren
#
# Wert = 1: genau durchschnittlich
# Wert > 1: überdurchschnittlich
# Wert < 1: unterdurchschnittlich
# ------------------------------------------------------------

radar_data <- station_summary %>%
  transmute(
    station,
    
    frequency = mean_events / metric_means$frequency_mean,
    duration = mean_duration / metric_means$duration_mean,
    intensity = mean_intensity / metric_means$intensity_mean
  )

radar_data

# ------------------------------------------------------------
# 4. Daten ins Long-Format bringen
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 5. Winkel und Achsenbeschriftungen definieren
# ------------------------------------------------------------

axis_information <- tibble(
  metric = factor(
    c("duration", "intensity", "frequency"),
    levels = c("frequency", "duration", "intensity")
  ),
  
  angle = c(
    pi / 2,          # Duration: oben
    -pi / 6,         # Intensity: rechts unten
    7 * pi / 6       # Frequency: links unten
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

# ------------------------------------------------------------
# 6. Reihenfolge der Ecken festlegen
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 7. Referenzdreieck für jede Station
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 8. Rasterdreiecke erzeugen
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 9. Achsenlinien
# ------------------------------------------------------------

maximum_value <- max(
  1.5,
  radar_data$frequency,
  radar_data$duration,
  radar_data$intensity,
  na.rm = TRUE
)

# Etwas Platz für die Beschriftungen hinzufügen

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

# ------------------------------------------------------------
# 10. Facettierter Radar Chart
# ------------------------------------------------------------

ggplot() +
  
  # Rasterdreiecke
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
  
  # Drei Radarachsen
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
  
  # Graues Referenzdreieck: Mittelwert aller Stationen
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
  
  # Stationsdreieck
  geom_polygon(
    data = radar_polygon,
    aes(
      x = x,
      y = y,
      group = station
    ),
    fill = "#0072B2",
    colour = "#0072B2",
    alpha = 0.25,
    linewidth = 1
  ) +
  
  # Punkte an den drei Ecken
  geom_point(
    data = radar_polygon,
    aes(
      x = x,
      y = y
    ),
    colour = "#0072B2",
    size = 2.3
  ) +
  
  # Achsenbeschriftungen
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
  
  # Eine Grafik pro Station
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



# fmsb

install.packages("fmsb")

library(fmsb)
library(dplyr)

# Relative Werte, wie zuvor
metric_means <- station_summary %>%
  summarise(
    across(
      c(frequency, duration, intensity),
      ~ mean(.x, na.rm = TRUE)
    )
  )

radar_data <- station_summary %>%
  mutate(
    frequency = mean_events / metric_means$frequency,
    duration  = mean_duration  / metric_means$duration,
    intensity = mean_intensity / metric_means$intensity
  )

max_value <- max(
  1.5,
  radar_data$frequency,
  radar_data$duration,
  radar_data$intensity,
  na.rm = TRUE
)

# 2 × 3 Anordnung
par(
  mfrow = c(2, 3),
  mar = c(1.5, 1.5, 3, 1.5)
)

for (i in seq_len(nrow(radar_data))) {
  
  station_values <- radar_data[i, ] %>%
    select(frequency, duration, intensity)
  
  plot_values <- rbind(
    max = c(max_value, max_value, max_value),
    min = c(0, 0, 0),
    reference = c(1, 1, 1),
    station = station_values
  )
  
  radarchart(
    plot_values,
    
    # Keine oder nur sehr wenige Rasterlinien
    seg = 1,
    cglcol = "grey90",
    cglty = 1,
    cglwd = 0.6,
    
    # Referenzdreieck und Stationsdreieck
    pcol = c("grey45", "#0072B2"),
    plty = c(2, 1),
    plwd = c(1.5, 2.5),
    
    pfcol = c(
      adjustcolor("grey60", alpha.f = 0.08),
      adjustcolor("#0072B2", alpha.f = 0.20)
    ),
    
    pty = c(NA, 16),
    
    vlabels = c(
      "Frequency",
      "Duration",
      "Intensity"
    ),
    
    axistype = 0,
    title = station_labels[radar_data$station[i]]
  )
}

par(mfrow = c(1, 1))




