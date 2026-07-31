library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(Matrix)
library(lme4)
library(tidyr)
library(MASS)

# load processed data

# salz <- readRDS("data_processed/fraenkische_saale_salz.rds")
# wolfsmuenster <- readRDS("data_processed/fraenkische_saale_wolfsmuenster.rds")
# schenkenau <- readRDS("data_processed/Itz_Schenkenau.rds")
# frankfurt_osthafen <- readRDS("data_processed/main_frankfurt_osthafen.rds")
kemmern <- readRDS("data_processed/main_kemmern.rds")
kleinheubach <- readRDS("data_processed/main_kleinheubach.rds")
# krotzenburg <- readRDS("data_processed/main_krotzenburg.rds")
# mainleus <- readRDS("data_processed/main_mainleus.rds")
schweinfurt <- readRDS("data_processed/main_schweinfurt.rds")
schwuerbitz <- readRDS("data_processed/main_schwuerbitz.rds")
# steinbach <- readRDS("data_processed/main_steinbach.rds")
wuerzburg <- readRDS("data_processed/main_wuerzburg.rds")
pettstadt <- readRDS("data_processed/regnitz_pettstadt.rds")
# sachsenheim <- readRDS("data_processed/wern_sachsenheim.rds")

# print(salz, n = 1000)

# create list of dataframes

stations <- list(
  # salz = salz,
  # wolfsmuenster = wolfsmuenster,
  # schenkenau = schenkenau,
  # frankfurt_osthafen = frankfurt_osthafen,
  #krotzenburg = krotzenburg,
  # mainleus = mainleus,
  schwuerbitz = schwuerbitz,
  pettstadt = pettstadt,
  kemmern = kemmern,
  schweinfurt = schweinfurt,
  # steinbach = steinbach,
  wuerzburg = wuerzburg,
  kleinheubach = kleinheubach
  # sachsenheim = sachsenheim
)

# all events function, creates start, end, duration, mean_intensity, max_intensity and severity for every heatwave

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
all_events

# annualy summary df: stores for each station, the heatwave events, heatwave days, mean duration, max duration,
# mean intensity, max intensity, mean severity and total severity per year

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
    freq_times_dur = heatwave_events*mean_duration,
    .groups = "drop"
  )

annual_summary

# create framework for dataframe

all_years <- expand.grid(
  station = names(stations),
  year = 2005:2019
)

# add to annual_summary

annual_summary <- all_years %>%
  left_join(annual_summary, by = c("station", "year")) %>%
  mutate(
    heatwave_events = replace_na(heatwave_events, 0),
    heatwave_days = replace_na(heatwave_days, 0),
    total_severity = replace_na(total_severity, 0)
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

station_summary <- annual_summary %>%
  group_by(station) %>%
  summarise(
    mean_events = mean(heatwave_events),
    max_duration = max(mean_duration, na.rm = TRUE),
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

all_events
annual_summary
station_summary




# ANALYSIS


lm_events <- lm(data = annual_summary, heatwave_events ~ year + station)

summary(lm_events)


lm_events_effects <- lm(data = annual_summary, heatwave_events ~ year*station)

summary(lm_events_effects)

# insignificant

poisson_events <- glm(
  heatwave_events ~ year + station,
  family = quasipoisson(link = "log"),
  data = annual_summary
)

summary(poisson_events)


poisson_events_effects <- glm(
  heatwave_events ~ year * station,
  family = quasipoisson(link = "log"),
  data = annual_summary
)

summary(poisson_events_effects)

# insignificant

poisson_days <- glm(
  heatwave_days ~ year * station,
  family = quasipoisson(link = "log"),
  data = annual_summary
)
summary(poisson_days)

# significant

poisson_severity <- glm(
  total_severity ~ year * station,
  family = quasipoisson(link = "log"),
  data = annual_summary
)
summary(poisson_severity)
