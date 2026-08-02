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
kemmern <- readRDS("work/05-riverine_heat/data/data_processed/main_kemmern.rds")
kleinheubach <- readRDS("work/05-riverine_heat/data/data_processed/main_kleinheubach.rds")
# krotzenburg <- readRDS("data_processed/main_krotzenburg.rds")
# mainleus <- readRDS("data_processed/main_mainleus.rds")
schweinfurt <- readRDS("work/05-riverine_heat/data/data_processed/main_schweinfurt.rds")
schwuerbitz <- readRDS("work/05-riverine_heat/data/data_processed/main_schwuerbitz.rds")
# steinbach <- readRDS("data_processed/main_steinbach.rds")
wuerzburg <- readRDS("work/05-riverine_heat/data/data_processed/main_wuerzburg.rds")
pettstadt <- readRDS("work/05-riverine_heat/data/data_processed/regnitz_pettstadt.rds")
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


# faceted plot of all stations, including common quasi poisson model trend


# Create one prediction for every observed station-year row
plot_data <- annual_summary %>%
  mutate(
    predicted = predict(
      model,
      newdata = annual_summary,
      type = "response"
    )
  )

heatwave_events_plot <- ggplot(
  plot_data,
  aes(x = year, y = heatwave_days)
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
    title = "Heatwave days per year and station",
    subtitle = "Fitted quasi-Poisson model with a common temporal trend",
    x = "Year",
    y = "Heatwave days"
  ) +
  theme_minimal()

# 2. DURATION

# 2.1 mean duration

duration_plot <- ggplot(annual_summary,
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



# 3. INTENSITY

# 3. 1 mean intensity

intensity_plot <- ggplot(annual_summary,
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





