library(dplyr)
library(lubridate)
library(purrr)


# load processed data

salz <- readRDS("data_processed/fraenkische_saale_salz.rds")
wolfsmuenster <- readRDS("data_processed/fraenkische_saale_wolfsmuenster.rds")
schenkenau <- readRDS("data_processed/Itz_Schenkenau.rds")
frankfurt_osthafen <- readRDS("data_processed/main_frankfurt_osthafen.rds")
kemmern <- readRDS("data_processed/main_kemmern.rds")
kleinheubach <- readRDS("data_processed/main_kleinheubach.rds")
krotzenburg <- readRDS("data_processed/main_krotzenburg.rds")
mainleus <- readRDS("data_processed/main_mainleus.rds")
schweinfurt <- readRDS("data_processed/main_schweinfurt.rds")
schwuerbitz <- readRDS("data_processed/main_schwuerbitz.rds")
steinbach <- readRDS("data_processed/main_steinbach.rds")
wuerzburg <- readRDS("data_processed/main_wuerzburg.rds")
pettstadt <- readRDS("data_processed/regnitz_pettstadt.rds")
sachsenheim <- readRDS("data_processed/wern_sachsenheim.rds")

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