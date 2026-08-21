library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

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

anova(lm_duration, lm_duration_int)

# 3. INTENSITY

# single temporal trends

station_models_intensity <- annual_summary %>%
  group_by(station) %>%
  group_modify(~ {
    
    model <- glm(
      mean_intensity ~ year,
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

station_models_intensity

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

anova(lm_intensity, lm_intensity_int)


# River kilometer Trends

# models

station_summary_models

# frequency
lm(mean_events ~ river_km, data = station_summary_models)
summary(lm(mean_events ~ river_km, data = station_summary_models))


cor(station_summary_models$river_km,
    station_summary_models$mean_events,
    use = "complete.obs")

summary(lm(mean_events ~ river_km, data = station_summary_models))


# duration

lm(mean_duration ~ river_km, data = station_summary_models)


summary(lm(mean_duration ~ river_km, data = station_summary_models))

# max duration 

lm(max_duration ~ river_km, data = station_summary_models)

summary(lm(max_duration ~ river_km, data = station_summary_models))


# intensity

lm(mean_intensity ~ river_km, data = station_summary_models)

summary(lm(mean_intensity ~ river_km, data = station_summary_models))
