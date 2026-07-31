
library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)

# load raw data

fraenkische_saale_salz <- read.csv("work/05-riverine_heat/data/data_raw/Fraenkische_Saale_Salz.csv")
fraenkische_saale_wolfsmuenster <- read.csv("work/05-riverine_heat/data/data_raw/Fraenkische_Saale_Wolfsmuenster.csv")
Itz_schenkenau <- read.csv("work/05-riverine_heat/data/data_raw/Itz_Schenkenau.csv")
main_frankfurt_osthafen <- read.csv("work/05-riverine_heat/data/data_raw/main_frankfurt_osthafen.csv")
main_kemmern <- read.csv("work/05-riverine_heat/data/data_raw/main_kemmern.csv")
main_kleinheubach <- read.csv("work/05-riverine_heat/data/data_raw/main_kleinheubach.csv")
main_krotzenburg <- read.csv("work/05-riverine_heat/data/data_raw/main_krotzenburg.csv")
main_mainleus <- read.csv("work/05-riverine_heat/data/data_raw/main_mainleus.csv")
main_schweinfurt <- read.csv("work/05-riverine_heat/data/data_raw/main_schweinfurt.csv")
main_schwuerbitz <- read.csv("work/05-riverine_heat/data/data_raw/main_schwuerbitz.csv")
main_steinbach <- read.csv("work/05-riverine_heat/data/data_raw/main_steinbach.csv")
main_wuerzburg <- read.csv("work/05-riverine_heat/data/data_raw/main_wuerzburg.csv")
regnitz_pettstadt <- read.csv("work/05-riverine_heat/data/data_raw/regnitz_pettstadt.csv")
wern_sachsenheim <- read.csv("work/05-riverine_heat/data/data_raw/wern_sachsenheim.csv")
static_features <- read.csv("work/05-riverine_heat/data/data_raw/static_features.csv")



# create list of data frames for loop
# here: exclude stations with insufficient data

stations <- list(
  fraenkische_saale_salz = fraenkische_saale_salz,
  fraenkische_saale_wolfsmuenster = fraenkische_saale_wolfsmuenster,
  Itz_schenkenau = Itz_schenkenau,
  main_frankfurt_osthafen = main_frankfurt_osthafen,
  main_kemmern = main_kemmern,
  main_kleinheubach = main_kleinheubach,
  main_krotzenburg = main_krotzenburg,
  main_mainleus = main_mainleus,
  main_schweinfurt = main_schweinfurt,
  main_schwuerbitz = main_schwuerbitz,
  main_steinbach = main_steinbach,
  main_wuerzburg = main_wuerzburg,
  regnitz_pettstadt = regnitz_pettstadt,
  wern_sachsenheim = wern_sachsenheim
  
  # ...
)
stations


# NAs (calculated in other skript):

# salz 2013 - 2019 / 15/16
# wolfsmuenster 2013-2020
# schenkenau 2013-2020
# frankfurt_osthafen 2009-2020 / 2016
# kemmern 2001 -2020
# kleinheubach 2001 - 2019
# krotzenburg 2009 - 2020
# mainleus 2018 - 2020
# schweinfurt 2001 - 2020
# schwuerbitz 2004 - 2020
# steinbach 2007 - 2020
# wuerzburg 2002 - 2020
# pettstadt 2005 - 2020
# sachsenheim 2010 - 2020




#filter out uncommon years:
stations <- lapply(stations, function(df) {
   df %>%
     filter(year >= 2005, year <= 2019)
})

stations

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
stations[[1]]
# save the processed data frames automatically

for (name in names(stations)) {
  
  saveRDS(
    stations[[name]],
    file = paste0("data_processed/", name, ".rds")
  )
  
}
