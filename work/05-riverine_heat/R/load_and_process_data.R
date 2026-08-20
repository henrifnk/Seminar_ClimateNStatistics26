
library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)

# load raw data

fraenkische_saale_salz <- read.csv("work/05-riverine_heat/data/Fraenkische_Saale_Salz.csv")
fraenkische_saale_wolfsmuenster <- read.csv("work/05-riverine_heat/data/Fraenkische_Saale_Wolfsmuenster.csv")
Itz_schenkenau <- read.csv("work/05-riverine_heat/data/Itz_Schenkenau.csv")
main_frankfurt_osthafen <- read.csv("work/05-riverine_heat/data/main_frankfurt_osthafen.csv")
main_kemmern <- read.csv("work/05-riverine_heat/data/main_kemmern.csv")
main_kleinheubach <- read.csv("work/05-riverine_heat/data/main_kleinheubach.csv")
main_krotzenburg <- read.csv("work/05-riverine_heat/data/main_krotzenburg.csv")
main_mainleus <- read.csv("work/05-riverine_heat/data/main_mainleus.csv")
main_schweinfurt <- read.csv("work/05-riverine_heat/data/main_schweinfurt.csv")
main_schwuerbitz <- read.csv("work/05-riverine_heat/data/main_schwuerbitz.csv")
main_steinbach <- read.csv("work/05-riverine_heat/data/main_steinbach.csv")
main_wuerzburg <- read.csv("work/05-riverine_heat/data/main_wuerzburg.csv")
regnitz_pettstadt <- read.csv("work/05-riverine_heat/data/regnitz_pettstadt.csv")
wern_sachsenheim <- read.csv("work/05-riverine_heat/data/wern_sachsenheim.csv")
static_features <- read.csv("work/05-riverine_heat/data/static_features.csv")



# create list of data frames for loop
# here: exclude stations with insufficient data

stations <- list(
  salz = fraenkische_saale_salz,
  wolfsmuenster = fraenkische_saale_wolfsmuenster,
  schenkenau = Itz_schenkenau,
  frankfurt_osthafen = main_frankfurt_osthafen,
  krotzenburg = main_krotzenburg,
  mainleus = main_mainleus,
  schwuerbitz = main_schwuerbitz,
  pettstadt = regnitz_pettstadt,
  kemmern = main_kemmern,
  schweinfurt = main_schweinfurt,
  steinbach = main_steinbach,
  wuerzburg = main_wuerzburg,
  kleinheubach = main_kleinheubach,
  sachsenheim = wern_sachsenheim
)

#filter out uncommon years:
stations <- lapply(stations, function(df) {
   df %>%
     filter(year >= 2005, year <= 2019)
})

# function to loop:
  
detect_heatwaves <- function(df) {
  
  # data transformation
  
  # for each gauge, a´calculate a 90th- percentile threshold and add flag to the data frame whether the day exceeds it or not
  # -> seperate threshold for each gauge, as in different locations, the temperature is different
  # then we can plot and analyse the data
  # also write down for each gauge, for which years there is data
  
  # evtl combine data into one dataframe, maybe first combine, then after we can still filter by year, gauge, etc
  
  # define threshold
  
  # 1. create time column
  
  df$date <- as.POSIXct(
    paste(df$year, df$month, df$day),
    format = "%Y %m %d",
  )
  
  # 2. take only maximum temperature for every day
  
  df <- df %>%
    mutate(water = "wt") %>%
    group_by(date) %>%
    summarise(
      wt = mean(wt, na.rm = TRUE),
      # wt = if (all(is.na(wt))) NA_real_ else max(wt, na.rm = TRUE),
      at = mean(Ta_C, na.rm = TRUE),
      across(-c(wt, Ta_C, hour, P_mm, wind_ms, rad_whm2, relhum, Q), first),
      .groups = "drop"
    )
  
  
  # 3. create day of year column
  
  df$doy <- yday(df$date)
  df$year <- year(df$date)
  
  # 4. create threshold 
  
  threshold <- numeric(365)
  
  for (j in 1:365) {
    
    # 11-day moving window
    window <- ((j-5):(j+5) - 1) %% 365 + 1
    
    temps <- df$wt[
      df$doy %in% window
    ]
    
    threshold[j] <- quantile(
      temps,
      probs = 0.90,
      na.rm = TRUE
    )
  }
  
  
  # check for nas
  
  n_values <- numeric(365)
  
  for (j in 1:365) {
    
    window <- ((j - 5):(j + 5) - 1) %% 365 + 1
    
    temps <- df$wt[
      df$doy %in% window
    ]
    
    n_values[j] <- sum(!is.na(temps))
  }
  
  # 5. attach threshold to data frame
  
  df$threshold <-
    threshold[df$doy]
  
  # plot threshold
  
  # ggplot(max_fraenkische_saale_salz, aes(date)) +
  #  geom_line(aes(y = wt), colour = "black") +
  #   geom_line(aes(y = threshold), colour = "red") +
  #   facet_wrap(~ year)
  
  
  # ggplot(subset(max_fraenkische_saale_salz, year == 2019),
  #        aes(x = date)) +
  #  geom_line(aes(y = wt), colour = "black") +
  #   geom_line(aes(y = threshold), colour = "red") +
  #   labs(
  #     title = "Daily Maximum Temperature and Threshold (2012)",
  #     x = "Date",
  #     y = "Temperature (°C)"
  #   ) +
  #   theme_minimal()
  
  
  
  # 6. add column flag (where temperature is above the threshold)
  df$flag <- df$wt > df$threshold
  
  
  
  # 7. look for heatwaves (at least 5 consecutive days)
  
  
  df <- df %>%
    arrange(date) %>%
    mutate(
      flag = !is.na(wt) & !is.na(threshold) & wt > threshold
    )
  
  # Run-length encoding of raw threshold exceedance
  r <- rle(df$flag)
  
  # TRUE runs are valid only if they are at least 5 days long
  valid_true_run <- r$values == TRUE & r$lengths >= 5
  
  # Start with only valid heatwave runs
  final_values <- valid_true_run
  
  # Bridge gaps of 1–2 days only if both sides are valid heatwaves
  for (i in seq_along(r$values)) {
    if (
      r$values[i] == FALSE &&
      r$lengths[i] <= 2 &&
      i > 1 &&
      i < length(r$values) &&
      valid_true_run[i - 1] &&
      valid_true_run[i + 1]
    ) {
      final_values[i] <- TRUE
    }
  }
  
  # Create final heatwave flag
  df$heatwave <- inverse.rle(list(
    lengths = r$lengths,
    values = final_values
  ))
  
  # 8. Now calculate final heatwave duration and event ID
  r_final <- rle(df$heatwave)
  
  df$heatwave_duration <- ifelse(
    df$heatwave,
    rep(r_final$lengths, r_final$lengths),
    NA
  )
  
  event_ids_by_run <- ifelse(
    r_final$values,
    cumsum(r_final$values),
    NA
  )
  
  df$heatwave_id <- rep(event_ids_by_run, r_final$lengths)
  
  
  return(df)
}


# loop 'detect_heatwaves' function over stations

for (i in seq_along(stations)) {
  stations[[i]] <- detect_heatwaves(stations[[i]])
}

# check nas:

na_check <- map_dfr(names(stations), function(station_name) {
  
  df <- stations[[station_name]]
  
  df %>%
    mutate(
      date = as.Date(date),
      expected_obs = if_else(leap_year(year), 366, 365)
    ) %>%
    group_by(year) %>%
    summarise(
      station = station_name,
      total_days = n(),
      available_wt = sum(!is.na(wt)),
      missing_wt = sum(is.na(wt)),
      expected_days = first(expected_obs),
      completeness = available_wt / expected_days,
      usable_90_percent = completeness >= 0.90,
      .groups = "drop"
    ) %>%
    dplyr::select(station, year, dplyr::everything())
})

# exclude stations with too much missing data

stations <- stations[c("schwuerbitz", "pettstadt", "kemmern", "schweinfurt", "wuerzburg", "kleinheubach")]


# FURTHER PROCESSING

# create start, end, duration, mean_intensity, max_intensity and severity for every heatwave

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

# annual summary df: stores for each station, the heatwave events, heatwave days, mean duration, max duration,
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

all_events %>%
  group_by(station) %>%
  summarise(n = n_distinct(heatwave_id))
annual_summary
annual_summary %>%
  group_by(year) %>%
  summarise(n = sum(heatwave_events))


# station levels to order stations

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

unique(as.character(annual_summary$station))

setdiff(unique(as.character(annual_summary$station)), station_levels)

annual_summary$station <- factor(
  as.character(annual_summary$station),
  levels = station_levels
)

# River kilometer Trends

# data transformation

station_summary_models <- annual_summary %>%
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

station_summary_models <- station_summary_models %>%
  mutate(
    river_km = unname(
      kilometers_main[as.character(station)]
    )
  )

station_summary_models <- station_summary_models %>%
  arrange(river_km)

# station labels fr plot

station_labels <- c(
  kleinheubach = "Main: Kleinheubach",
  wuerzburg = "Main: Würzburg",
  schweinfurt = "Main: Schweinfurt",
  kemmern = "Main: Kemmern",
  pettstadt = "Regnitz: Pettstadt",
  schwuerbitz = "Main: Schwürbitz"
)

station_levels <- c(
  "kleinheubach",
  "wuerzburg",
  "schweinfurt",
  "kemmern",
  "pettstadt",
  "schwuerbitz"
)


annual_summary$station <- factor(
  annual_summary$station,
  levels = station_levels
)

station_summary <- annual_summary %>%
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


