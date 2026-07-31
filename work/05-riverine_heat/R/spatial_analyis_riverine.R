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
# annualy heatwave frequency by station 

ggplot(
  annual_summary,
  aes(x = station, y = heatwave_events)
) +
  geom_count(
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
    linewidth = 0.8,
    colour = "#D55E00"
  ) +
  scale_x_discrete(
    labels = station_labels
  ) +
  scale_size_area(
    max_size = 6,
    name = "Number of years"
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Annual heatwave frequency by station",
    #subtitle = "Points show annual values; horizontal markers show station means",
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
    axis.text.x = element_text(
      hjust = 1
    ),
    legend.position = "right"
  )


# # annualy heatwave frequency by station jitter


annual_summary %>%
  group_by(station) %>%
  summarise(
    mean_heatwave_events= mean(heatwave_events),
    sd_heatwave_events  = sd(heatwave_events),
    cv_heatwave_events   = sd(heatwave_events) / mean(heatwave_events)
  )


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




# 1.2 DURATION

# sd

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



# 1.3 INTENSITY

all_events %>%
  group_by(station) %>%
  summarise(
    mean_mean_intensity = mean(mean_intensity),
    sd_intensity  = sd(mean_intensity),
    cv_intensity   = sd(mean_intensity) / mean(mean_intensity)
  )

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


ggplot(all_events,
       aes(station, mean_intensity)) +
  geom_boxplot() +
  labs(
    x = "Station",
    y = "Mean heatwave intensity (°C)"
  ) +
  theme_minimal()

all_events$mean_intensity


# 1.4 SEVERITY

all_events %>%
  group_by(station) %>%
  summarise(
    mean_severity= mean(severity),
    sd_severity  = sd(severity),
    cv_severity  = sd(severity) / mean(severity)
  )

ggplot(
  all_events,
  aes(x = station, y = severity)
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
  labs(
    title = "Total heatwave intensity by station",
    subtitle = "Points represent individual heatwave events; diamonds show station means",
    x = NULL,
    y = "Tota heatwave intensity (C° days)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

ggplot(all_events,
       aes(station, severity)) +
  geom_boxplot() +
  labs(
    x = "Station",
    y = "Heatwave severity (°C days)"
  ) +
  theme_minimal()




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


# plots


station_summary <- station_summary %>%
  arrange(river_km)


ggplot(
  station_summary,
  aes(x = river_km, y = mean_events)
) +
  geom_point(
    size = 3.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.9,
    alpha = 0.18
  ) +
  geom_text(
    aes(label = station_labels[station]),
    nudge_y = 0.08,
    size = 4,
    check_overlap = TRUE
  ) +
  scale_x_reverse() +
  labs(
    title = "Longitudinal pattern in heatwave frequency",
    subtitle = "Main River stations, ordered from upstream to downstream",
    x = "River kilometre",
    y = "Mean annual heatwave events"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3)
  )







lm(mean_events ~ river_km, data = station_summary)
summary(lm(mean_events ~ river_km, data = station_summary))


cor(station_summary$river_km,
    station_summary$mean_events,
    use = "complete.obs")

summary(lm(mean_events ~ river_km, data = station_summary))

# sort annual summary

annual_summary <- annual_summary %>%
  mutate(river_km = kilometers_main[station])


annual_summary <- annual_summary %>%
  arrange(river_km) %>%
  mutate(station = factor(station, levels = unique(station)))

ggplot(annual_summary,
       aes(x = station,
           y = heatwave_events)) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2,
    alpha = 0.6
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4,
    shape = 18,
    colour = "red"
  ) +
  labs(
    x = "Station",
    y = "Heatwave events per year"
  ) +
  theme_minimal()


# duration

lm(mean_duration ~ river_km, data = station_summary)


summary(lm(mean_duration ~ river_km, data = station_summary))


# intensity

lm(mean_intensity ~ river_km, data = station_summary)

summary(lm(mean_intensity ~ river_km, data = station_summary))

# SEVERITY

# mean severity

ggplot(station_summary,
       aes(station, mean_severity)) +
  geom_boxplot() +
  labs(
    x = "Station",
    y = "Mean heatwave intensity (°C)"
  ) +
  theme_minimal()



# mean(station) total(year) severity

ggplot(station_summary,
       aes(station, total_severity)) +
  geom_boxplot() +
  labs(
    x = "Station",
    y = "Mean heatwave intensity (°C)"
  ) +
  theme_minimal()





ggsave(
  "Images/heatwave_events_overview.pdf",
  plot = heatwave_events_overview,
  width = 5,
  height = 4
)
ggsave(
  "Images/mean_duration_overview.pdf",
  plot = mean_duration_overview,
  width = 5,
  height = 4
)
ggsave(
  "Images/mean_intensity_overview.pdf",
  plot = mean_intensity_overview,
  width = 5,
  height = 4
)
heatwave_events_overview
mean_duration_overview
mean_intensity_overview

