library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(Matrix)
library(lme4)
library(tidyr)
library(MASS)

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

station_levels <- c("kleinheubach",
                    "wuerzburg",
                    "schweinfurt",
                    "kemmern",
                    "pettstadt",
                    "schwuerbitz")


annual_summary$station <- factor(
  annual_summary$station,
  levels = station_levels
)

# ANALYSIS
annual_summary

# 1. FREQUENCY

# single temporal trends

station_models_events <- annual_summary %>%
  group_by(station) %>%
  group_modify(~ {
    
    model <- glm(
      heatwave_events ~ year,
      family = quasipoisson(link = "log"),
      data = .x
    )
    
    coef <- summary(model)$coefficients
    
    tibble(
      slope = coef["year", "Estimate"],
      percent_change_per_year =
        (exp(coef["year", "Estimate"]) - 1) * 100,
      p_value = coef["year", "Pr(>|t|)"]
    )
  }) %>%
  ungroup()

station_models_events

# common temporal trend

poisson_events <- glm(
  heatwave_events ~ year + station,
  family = quasipoisson(link = "log"),
  data = annual_summary
)

summary(poisson_events)

poisson_events_int <- glm(
  heatwave_events ~ year * station,
  family = quasipoisson(link = "log"),
  data = annual_summary
)

summary(poisson_events_int)

# F test to check difference between models

anova(poisson_events, poisson_events_int, test = "F")


# 2. DURATION

# single temporal trends

station_models_duration <- annual_summary %>%
  group_by(station) %>%
  group_modify(~ {
    
    model <- glm(
      mean_duration ~ year,
      family = quasipoisson(link = "log"),
      data = .x
    )
    
    coef <- summary(model)$coefficients
    
    tibble(
      slope = coef["year", "Estimate"],
      percent_change_per_year =
        (exp(coef["year", "Estimate"]) - 1) * 100,
      p_value = coef["year", "Pr(>|t|)"]
    )
  }) %>%
  ungroup()

station_models_duration

# common temporal trend

lm_duration <- lm(
  mean_duration ~ year + station,
  data = annual_summary
)

summary(lm_duration)

lm_duration_int <- lm(
  mean_duration ~ year * station,
  data = annual_summary
)

summary(lm_duration)

anova(lm_duration, lm_duration_int, test = "F")

# 3. INTENSITY

# single temporal trends

# common temporal trend

lm_intensity <- lm(
  mean_intensity ~ year + station,
  data = annual_summary
)

summary(lm_intensity)

lm_intensity_int <- lm(
  mean_intensity ~ year * station,
  data = annual_summary
)

summary(lm_intensity_int)

anova(lm_intensity, lm_intensity_int, test = "F")

# River kilometer Trends

# data transformation

annual_summary

station_summary <- annual_summary %>%
  filter(station != "pettstadt") %>%
  group_by(station) %>%
  summarise(
    max_duration = mean(max_duration, na.rm = TRUE),
    mean_events = mean(heatwave_events),
    mean_duration = mean(mean_duration, na.rm = TRUE),
    mean_intensity = mean(mean_intensity, na.rm = TRUE),
    mean_severity = mean(mean_severity, na.rm = TRUE),
    total_severity = mean(total_severity, na.rm = TRUE),
    .groups = "drop"
  )

kilometers_main <- c("schwuerbitz" = 438.29,
                     #"pettstadt" = kemmern + 13.96
                     "kemmern" = 390.93,
                     "schweinfurt" = 330.78,
                     "wuerzburg" = 251.97,
                     "kleinheubach" = 121.74)

station_summary <- station_summary %>%
  mutate(
    river_km = unname(
      kilometers_main[as.character(station)]
    )
  )
station_summary

station_summary <- station_summary %>%
  arrange(river_km)

# models

station_summary

# frequency
lm(mean_events ~ river_km, data = station_summary)
summary(lm(mean_events ~ river_km, data = station_summary))


cor(station_summary$river_km,
    station_summary$mean_events,
    use = "complete.obs")

summary(lm(mean_events ~ river_km, data = station_summary))


# duration

lm(mean_duration ~ river_km, data = station_summary)


summary(lm(mean_duration ~ river_km, data = station_summary))

# max duration 

lm(max_duration ~ river_km, data = station_summary)

summary(lm(max_duration ~ river_km, data = station_summary))


# intensity

lm(mean_intensity ~ river_km, data = station_summary)

summary(lm(mean_intensity ~ river_km, data = station_summary))
