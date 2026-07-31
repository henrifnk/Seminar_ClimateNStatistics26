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

annual_summary


# annual_summary zuerst neu erzeugen / neu laden

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


# 1. FREQUENCY

# plot: heatwave tage pro jahr für einen ort

# annual_summary %>%
#   filter(station == "wolfsmuenster") %>%
#   ggplot(aes(x = year, y = heatwave_days)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "Salz",
#     x = "Year",
#     y = "Heatwave days"
#   ) +
#   theme_minimal()

# heatwave events for all stations combined

data <- annual_summary %>%
  group_by(year) %>%
  summarise(heatwave_days = sum(heatwave_days))
  
ggplot(data,
       aes(year, heatwave_days)) +
  geom_point() +
  #geom_line() +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    alpha = 0.25
  ) +
  labs(
    title = "Heatwave days per year",
    x = "Year",
    y = "Heatwave days"
  ) +
  theme_minimal()



fit <- glm(
  heatwave_events ~ year,
  family = poisson,
  data = annual_summary
)


deviance(fit) / df.residual(fit)


fit2 <- glm(
  heatwave_events ~ year + station,
  family = poisson,
  data = annual_summary
)

summary(fit2)


summary(fit)
summary(lm(heatwave_days ~ year, data = data))

fit_interaction <- glm(
  heatwave_events ~ year * station,
  family = poisson,
  data = annual_summary
)

anova(fit, fit_interaction, test = "Chisq")


# 1.1 heatwave days for all stations w lm incorporated

ggplot(annual_summary,
      aes(year, heatwave_days)) +
  geom_point() +
  #geom_line() +
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
    title = "Heatwave days per year and station",
    x = "Year",
    y = "Heatwave days"
  ) +
  theme_minimal()

lm_days <- lm(data = annual_summary, heatwave_days ~ year + station)
summary(lm_days)# trend table heatwave days increase


# 1.2 heatwave_events per year w linear trend incorporated

heatwave_events_time <- ggplot(annual_summary,
       aes(year, heatwave_events)) +
  geom_point() +
  #geom_line() +
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
    title = "Heatwave events per year and station",
    x = "Year",
    y = expression("Heatwave events")
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )
heatwave_events_time <- ggplot(annual_summary,
                               aes(year, heatwave_events)) +
  geom_point() +
  #geom_line() +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    alpha = 0.25
  )+
  facet_wrap(
    ~station,
    labeller = as_labeller(station_labels)
  ) +
  labs(
    title = "Heatwave events per year and station",
    x = "Year",
    y = expression("Heatwave events")
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

heatwave_events_time



# trend table event increase

station_trends_events <- annual_summary %>%
  group_by(station) %>%
  group_modify(~{
    
    model <- lm(heatwave_events ~ year, data = .x)
    
    ci <- confint(model)["year", ]
    
    tibble(
      events_per_decade = coef(model)["year"] * 10,
      lower_CI = ci[1] * 10,
      upper_CI = ci[2] * 10,
      r_squared = summary(model)$r.squared,
      p_value = summary(model)$coefficients["year", "Pr(>|t|)"]
    )
  })

station_trends_events



# 2. DURATION

# 2.1 mean duration

mean_duration <- ggplot(annual_summary,
       aes(x = year,
           y = mean_duration)) +
  geom_point() +
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
    title = "Mean heatwave duration per year and station",
    x = "Year",
    y = expression("Mean duration (days)")
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )




# trend table duration

duration_trends <- annual_summary %>%
  group_by(station) %>%
  group_modify(~{
    
    model <- lm(mean_duration ~ year, data = .x)
    
    s <- summary(model)
    
    tibble(
      slope = coef(model)["year"],
      duration_change_decade = coef(model)["year"] * 10,
      r_squared = s$r.squared,
      p_value = s$coefficients["year", "Pr(>|t|)"]
    )
})

duration_trends

# plot: längste heatwave pro jahr

# ggplot(annual_summary, aes(x = year, y = max_duration, colour = station)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     x = "Year",
#     y = "Maximum heatwave duration"
#   ) +
#   theme_minimal()

# 3. INTENSITY

# 3. 1 mean intensity

mean_intensity <- ggplot(annual_summary,
       aes(x = year,
           y = mean_intensity)) +
  geom_point() +
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
    title = "Yearly mean heatwave intensity per station",
    x = "Year",
    y = "Mean heatwave intensity (°C)"
  ) +
  theme_minimal()+
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )






# trend table intensity 

intensity_trends <- annual_summary %>%
  group_by(station) %>%
  group_modify(~{
    
    model <- lm(mean_intensity ~ year, data = .x)
    
    s <- summary(model)
    
    tibble(
      slope = coef(model)["year"],
      intensity_change_decade = coef(model)["year"] * 10,
      r_squared = s$r.squared,
      p_value = s$coefficients["year", "Pr(>|t|)"]
    )
})

intensity_trends

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

# frq times duration

ggplot(annual_summary,
       aes(x = year, y = freq_times_dur)) +
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


lm_s <- lm(data = annual_summary, total_severity ~ year + station)

summary(lm_s)# trend table heatwave days increase
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

# trend table severity

selected_stations <- c(
  "kemmern",
  "kleinheubach",
  "pettstadt",
  "schweinfurt",
  "schwuerbitz",
  "wuerzburg"
)


calculate_trends <- function(data, response) {
  
  data %>%
    filter(station %in% selected_stations) %>%
    group_by(station) %>%
    group_modify(~{
      
      formula <- as.formula(paste(response, "~ year"))
      model <- lm(formula, data = .x)
      s <- summary(model)
      
      tibble(
        slope = coef(model)["year"],
        change_per_decade = coef(model)["year"] * 10,
        r_squared = s$r.squared,
        p_value = s$coefficients["year", "Pr(>|t|)"]
      )
    })
}

calculate_trends

mean_severity_trends <- calculate_trends(
  annual_summary,
  "mean_severity"
)

mean_severity_trends

total_severity_trends <- calculate_trends(
  annual_summary,
  "total_severity"
)

total_severity_trends

ggsave(
  "Images/heatwave_events_time.pdf",
  plot = heatwave_events_time,
  width = 5,
  height = 4
)
ggsave(
  "Images/mean_duration.pdf",
  plot = mean_duration,
  width = 5,
  height = 4
)
ggsave(
  "Images/mean_intensity.pdf",
  plot = mean_intensity,
  width = 5,
  height = 4
)
heatwave_events_time
mean_duration
mean_intensity



fit_duration <- lm(
  mean_duration ~ year + station,
  data = annual_summary
)

summary(fit_duration)

# redsidual vs fitted
mean_duration_residuals <- ggplot(
  data.frame(
    fitted = fitted(fit_duration),
    residuals = resid(fit_duration)
  ),
  aes(fitted, residuals)
) +
  geom_point() +
  geom_hline(
    yintercept = 0,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted"
  ) +
  theme_minimal()

mean_duration_residuals
# -> +keine obvious krümmung, keine trischterform
# positive residuen etwas größer (lange heatwaves)

# qq plot

plot(fit_duration, which = 2)

annual_summary[c(11, 12, 72), ]


# homoskedastizität
plot(fit_duration, which = 3)   # Scale-Location

#leichter anstieg der varianz, abr noch ok

plot(fit_duration, which = 4)   # Cook's Distance

# -> 3 seht lange heatwaves mit großem einfluss

fit1 <- lm(mean_duration ~ year + station,
           data = annual_summary)

fit2 <- lm(mean_duration ~ year * station,
           data = annual_summary)

anova(fit1, fit2)


prediction_data <- expand.grid(
  year = seq(
    min(annual_summary$year, na.rm = TRUE),
    max(annual_summary$year, na.rm = TRUE),
    length.out = 100
  ),
  station = levels(factor(annual_summary$station))
)

#Add predictions and confidence intervals:
  
  pred <- predict(
    fit_duration,
    newdata = prediction_data,
    interval = "confidence"
  )

prediction_data <- cbind(prediction_data, pred)

library(ggplot2)

duration_plot <- ggplot(
  annual_summary,
  aes(x = year, y = mean_duration)
) +
  geom_point() +
  
  # Separate descriptive regression for each station
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # Confidence interval from the common-slope model
  geom_ribbon(
    data = prediction_data,
    aes(
      x = year,
      ymin = lwr,
      ymax = upr,
      group = station
    ),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  
  # Fitted line from the common-slope model
  geom_line(
    data = prediction_data,
    aes(
      x = year,
      y = fit,
      group = station
    ),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ station,
    labeller = as_labeller(station_labels)
  ) +
  
  labs(
    title = "Mean heatwave duration per year and station",
    subtitle = paste0(
      "Solid lines: common-slope model; \n",
      "dashed lines: station-specific regressions"
    ),
    x = "Year",
    y = "Mean duration (days)"
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



# EVENTS

fit_events <- glm(
  heatwave_events ~ year + station,
  family = poisson,
  data = annual_summary
)

summary(fit_events)

fit_events_lm <- lm(heatwave_events ~ year + station, data = annual_summary)
summary(fit_events_lm)

plot(
  fitted(fit_events_lm),
  resid(fit_events_lm),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs Fitted"
)

abline(h = 0, col = "red", lty = 2)

# bänder -> normal, da zähldaten (ganze zahlen)

# schlecht: streuet stark bei großen werten (unterschättz große werte)


# -> +keine obvious krümmung, keine trischterform
# positive residuen etwas größer (lange heatwaves)

# qq plot

plot(fit_events_lm, which = 2)


# dispersion poisson
dispersion <- sum(residuals(fit_events, type = "pearson")^2) /
  df.residual(fit_events)

dispersion

# test diepersion


library(DHARMa)

sim <- simulateResiduals(fit_events)

plot(sim)

testDispersion(sim)

# homoskedastizität
plot(fit_duration, which = 3)   # Scale-Location

#leichter anstieg der varianz, abr noch ok

plot(fit_duration, which = 4) 

prediction_data <- expand.grid(
  year = seq(
    min(annual_summary$year),
    max(annual_summary$year),
    length.out = 100
  ),
  station = levels(annual_summary$station)
)



pred <- predict(
  fit_events_lm,
  newdata = prediction_data,
  se.fit = TRUE
)

prediction_data$fit <- pred$fit
prediction_data$lwr <- pred$fit - 1.96 * pred$se.fit
prediction_data$upr <- pred$fit + 1.96 * pred$se.fit



heatwave_events_plot <- ggplot(
  annual_summary,
  aes(year, heatwave_events)
) +
  geom_point() +
  ## station-specific Poisson regression
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "blue",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  ## confidence band from common model
  geom_ribbon(
    data = prediction_data,
    aes(
      x = year,
      ymin = lwr,
      ymax = upr,
      group = station
    ),
    inherit.aes = FALSE,
    alpha = 0.20
  ) +
  ## common model
  geom_line(
    data = prediction_data,
    aes(
      x = year,
      y = fit,
      group = station
    ),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 1
  ) +
  facet_wrap(
    ~station,
    labeller = as_labeller(station_labels)
  ) +
  labs(
    title = "Heatwave events per year and station",
    subtitle = "Solid line: common linear model;\nDashed line: station-specific linear regressions",
    x = "Year",
    y = "Heatwave events"
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



# intensity

fit_intensity <- lm(
  mean_intensity ~ year + station,
  data = annual_summary
)

summary(fit_intensity)

# residuals vs fitte

plot(fit_intensity, which = 1)

# linearität erfüllt

# qq
plot(fit_intensity, which = 2)

# sieht sehr gut aus

# scale location
plot(fit_intensity, which = 3)

prediction_data_i <- expand.grid(
  year = seq(
    min(annual_summary$year, na.rm = TRUE),
    max(annual_summary$year, na.rm = TRUE),
    length.out = 100
  ),
  station = levels(factor(annual_summary$station))
)

#Add predictions and confidence intervals:

pred_i <- predict(
  fit_intensity,
  newdata = prediction_data_i,
  interval = "confidence"
)

prediction_data_i <- cbind(prediction_data_i, pred_i)

library(ggplot2)

intensity_plot <- ggplot(
  annual_summary,
  aes(x = year, y = mean_intensity)
) +
  geom_point() +
  
  # Separate descriptive regression for each station
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # Confidence interval from the common-slope model
  geom_ribbon(
    data = prediction_data_i,
    aes(
      x = year,
      ymin = lwr,
      ymax = upr,
      group = station
    ),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  
  # Fitted line from the common-slope model
  geom_line(
    data = prediction_data_i,
    aes(
      x = year,
      y = fit,
      group = station
    ),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ station,
    labeller = as_labeller(station_labels)
  ) +
  
  labs(
    title = "Mean heatwave intensity per year and station",
    subtitle = paste0(
      "Solid lines: common-slope model;\n",
      "Dashed lines: station-specific regressions"
    ),
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


# TOTAL SEVERITY

fit_severity <- lm(
  total_severity ~ year + station,
  data = annual_summary
)

summary(fit_severity)


prediction_data_s <- expand.grid(
  year = seq(
    min(annual_summary$year, na.rm = TRUE),
    max(annual_summary$year, na.rm = TRUE),
    length.out = 100
  ),
  station = levels(factor(annual_summary$station))
)

#Add predictions and confidence intervals:

pred_s <- predict(
  fit_severity,
  newdata = prediction_data_s,
  interval = "confidence"
)

prediction_data_s <- cbind(prediction_data_s, pred_s)

library(ggplot2)

severity_plot <- ggplot(
  annual_summary,
  aes(x = year, y = total_severity)
) +
  geom_point() +
  
  # Separate descriptive regression for each station
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # Confidence interval from the common-slope model
  geom_ribbon(
    data = prediction_data_s,
    aes(
      x = year,
      ymin = lwr,
      ymax = upr,
      group = station
    ),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  
  # Fitted line from the common-slope model
  geom_line(
    data = prediction_data_s,
    aes(
      x = year,
      y = fit,
      group = station
    ),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ station,
    labeller = as_labeller(station_labels)
  ) +
  
  labs(
    title = "Mean heatwave intensity per year and station",
    subtitle = paste0(
      "Solid lines: common-slope model; ",
      "dashed lines: station-specific regressions"
    ),
    x = "Year",
    y = "Mean duration"
  ) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

severity_plot

ggsave(
  "Images/heatwave_events_plot.pdf",
  plot = heatwave_events_plot,
  width = 5,
  height = 4
)
ggsave(
  "Images/duration_plot.pdf",
  plot = duration_plot,
  width = 5,
  height = 4
)
ggsave(
  "Images/intensity_plot.pdf",
  plot = intensity_plot,
  width = 5,
  height = 4
)
heatwave_events_plot
duration_plot
intensity_plot
mean_duration_residuals

ggsave(
  "Images/mean_duration_residuals.pdf",
  plot = mean_duration_residuals,
  width = 5,
  height = 4
)

qq_data <- data.frame(
  sample = resid(fit_duration)
)

mean_duration_qq <- ggplot(qq_data, aes(sample = sample)) +
  stat_qq(size = 2) +
  stat_qq_line(colour = "red", linewidth = 0.8) +
  labs(
    title = "Normal Q-Q",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

ggsave(
  "Images/mean_duration_qq.pdf",
  plot = mean_duration_qq,
  width = 5,
  height = 4
)


intensity_residuals <- ggplot(
  data.frame(
    fitted = fitted(fit_intensity),
    residuals = resid(fit_intensity)
  ),
  aes(x = fitted, y = residuals)
) +
  geom_point(size = 2.3) +
  geom_hline(
    yintercept = 0,
    colour = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw(base_size = 13)
mean_duration_qq
ggsave(
  "Images/intensity_residuals.pdf",
  plot = intensity_residuals,
  width = 5,
  height = 4
)

events_residuals <- ggplot(
  data.frame(
    fitted = fitted(fit_events_lm),
    residuals = resid(fit_events_lm)
  ),
  aes(x = fitted, y = residuals)
) +
  geom_point(size = 2.3) +
  geom_hline(
    yintercept = 0,
    colour = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw(base_size = 13)
intensity_residuals
events_residuals
mean_duration_residuals

ggsave(
  "Images/events_residuals.pdf",
  plot = events_residuals,
  width = 5,
  height = 4
)
#Es gibt keinen Hinweis, dass sich die Trends der Stationen unterscheiden.

#Damit ist es statistisch sinnvoll, eine gemeinsame Steigung zu schätzen.

#Was ich noch sauber machen würde

#Die Modellannahmen der linearen Modelle dokumentieren (Residualplots, Homoskedastizität, QQ-Plot).
#Für den Poisson-GLM kurz erwähnen, dass du Overdispersion geprüft hast (dein Verhältnis Deviance/df ≈ 1.23 spricht dafür, dass das Modell plausibel ist).
#In der Diskussion ehrlich erwähnen, dass wiederholte Messungen derselben Station die Unabhängigkeitsannahme einschränken und dass komplexere Modelle (z. B. gemischte Modelle oder GLS) in zukünftigen Analysen eine mögliche Erweiterung wären.

# Common slope
fit_common <- lm(
  mean_duration ~ year + station,
  data = annual_summary
)

# Station-specific slopes
fit_interaction <- lm(
  mean_duration ~ year * station,
  data = annual_summary
)
anova(fit_common, fit_interaction)
saveRDS(fit_duration, "Images/fit_duration.rds")
saveRDS(fit_intensity, "Images/fit_intensity.rds")
saveRDS(fit_events, "Images/fit_events.rds")
saveRDS(fit_events_lm, "Images/fit_events_lm.rds")

summary(fit_events)

plot(fit_events_lm, which = 1)
plot(fit_events_lm, which = 2)
plot(fit_events_lm, which = 3)

plot(fit_events_lm, which = 4)
library(ggplot2)

qq_events <- ggplot(
  data.frame(
    sample = resid(fit_events_lm)
  ),
  aes(sample = sample)
) +
  stat_qq(size = 2.3) +
  stat_qq_line(
    colour = "red",
    linewidth = 0.8
  ) +
  labs(
    title = "Normal Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_bw(base_size = 13)
ggsave(
  "Images/qq_events.pdf",
  plot = qq_events,
  width = 5,
  height = 4
)

library(lmtest)
sim <- simulateResiduals(fit_events)
plot(sim)
bptest(fit_events_lm)
