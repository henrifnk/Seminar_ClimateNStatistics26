library(dplyr)
library(lubridate)
library(purrr)


# load processed data

salz <- readRDS("work/05-riverine_heat/data/data_processed/fraenkische_saale_salz.rds")
wolfsmuenster <- readRDS("work/05-riverine_heat/data/data_processed/fraenkische_saale_wolfsmuenster.rds")
schenkenau <- readRDS("work/05-riverine_heat/data/data_processed/Itz_Schenkenau.rds")
frankfurt_osthafen <- readRDS("work/05-riverine_heat/data/data_processed/main_frankfurt_osthafen.rds")
kemmern <- readRDS("work/05-riverine_heat/data/data_processed/main_kemmern.rds")
kleinheubach <- readRDS("work/05-riverine_heat/data/data_processed/main_kleinheubach.rds")
krotzenburg <- readRDS("work/05-riverine_heat/data/data_processed/main_krotzenburg.rds")
mainleus <- readRDS("work/05-riverine_heat/data/data_processed/main_mainleus.rds")
schweinfurt <- readRDS("work/05-riverine_heat/data/data_processed/main_schweinfurt.rds")
schwuerbitz <- readRDS("work/05-riverine_heat/data/data_processed/main_schwuerbitz.rds")
steinbach <- readRDS("work/05-riverine_heat/data/data_processed/main_steinbach.rds")
wuerzburg <- readRDS("work/05-riverine_heat/data/data_processed/main_wuerzburg.rds")
pettstadt <- readRDS("work/05-riverine_heat/data/data_processed/regnitz_pettstadt.rds")
sachsenheim <- readRDS("work/05-riverine_heat/data/data_processed/wern_sachsenheim.rds")

# create list of dataframes

stations <- list(
  salz = salz,
  wolfsmuenster = wolfsmuenster,
  schenkenau = schenkenau,
  frankfurt_osthafen = frankfurt_osthafen,
  krotzenburg = krotzenburg,
  mainleus = mainleus,
  schwuerbitz = schwuerbitz,
  pettstadt = pettstadt,
  kemmern = kemmern,
  schweinfurt = schweinfurt,
  steinbach = steinbach,
  wuerzburg = wuerzburg,
  kleinheubach = kleinheubach,
  sachsenheim = sachsenheim
)

head(stations)

# check for NAs, and calculate percentage of NAs, 
# exclude years with less then 90 percent of days available
# exclude years with less than 15 full years available

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

na_check