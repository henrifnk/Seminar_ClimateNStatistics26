# ================================================================
# Add-on: North Atlantic SST Case Study
#
# Purpose:
#   Apply the same early-warning indicators to a real climate proxy:
#   North Atlantic sea-surface temperature (SST) anomaly.
#
# Important interpretation:
#   This is NOT a direct AMOC collapse detection.
#   It is an illustrative AMOC-related proxy application.
#
# Data source:
#   NOAA PSL ERSST v5 monthly SST:
#   https://downloads.psl.noaa.gov/Datasets/noaa.ersst.v5/sst.mnmean.nc
# ================================================================

# -------------------------------
# 0. Packages
# -------------------------------

required_real_packages <- c(
  "ncdf4",
  "dplyr",
  "ggplot2",
  "zoo",
  "readr",
  "tibble",
  "scales"
)

missing_real_packages <- required_real_packages[
  !vapply(required_real_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_real_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_real_packages, collapse = ", "),
    "\nExample: install.packages(c(\"",
    paste(missing_real_packages, collapse = "\", \""),
    "\"))"
  )
}

library(ncdf4)
library(dplyr)
library(ggplot2)
library(zoo)
library(readr)
library(tibble)
library(scales)

output_dir <- "outputs/north_atlantic_sst_case_study"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

theme_set(theme_minimal(base_size = 12))

# -------------------------------
# 1. Reuse EWS helper functions
# -------------------------------

rolling_var <- function(x, width = 20) {
  zoo::rollapply(
    x,
    width = width,
    FUN = var,
    fill = NA_real_,
    align = "right",
    na.rm = TRUE
  )
}

rolling_ac1 <- function(x, width = 20) {
  zoo::rollapply(
    x,
    width = width,
    FUN = function(z) {
      z <- stats::na.omit(z)
      if (length(z) < 10 || stats::sd(z) == 0) {
        return(NA_real_)
      }
      stats::acf(z, lag.max = 1, plot = FALSE)$acf[2]
    },
    fill = NA_real_,
    align = "right"
  )
}

kendall_trend <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) < 10 || stats::sd(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::cor(seq_along(x), x, method = "kendall"))
}

# -------------------------------
# 2. Download NOAA ERSST v5 if needed
# -------------------------------

sst_url <- "https://downloads.psl.noaa.gov/Datasets/noaa.ersst.v5/sst.mnmean.nc"
sst_file <- file.path(output_dir, "sst.mnmean.nc")

if (!file.exists(sst_file)) {
  message("Downloading NOAA ERSST v5 monthly SST data...")
  utils::download.file(sst_url, destfile = sst_file, mode = "wb")
}

# -------------------------------
# 3. Read NetCDF and extract North Atlantic region
# -------------------------------

nc <- ncdf4::nc_open(sst_file)

lon <- ncdf4::ncvar_get(nc, "lon")
lat <- ncdf4::ncvar_get(nc, "lat")
time <- ncdf4::ncvar_get(nc, "time")

# ERSST time is usually days since 1800-01-01.
time_units <- ncdf4::ncatt_get(nc, "time", "units")$value
origin_string <- sub("days since ", "", time_units)
dates <- as.Date(time, origin = origin_string)

# North Atlantic / subpolar gyre proxy box:
#   45-60 N, 300-350 E = 60-10 W
# This is AMOC-related but not a direct AMOC measurement.
lon_index <- which(lon >= 300 & lon <= 350)
lat_index <- which(lat >= 45 & lat <= 60)

sst_box <- ncdf4::ncvar_get(
  nc,
  "sst",
  start = c(min(lon_index), min(lat_index), 1),
  count = c(length(lon_index), length(lat_index), -1)
)

missing_value <- ncdf4::ncatt_get(nc, "sst", "missing_value")$value
fill_value <- ncdf4::ncatt_get(nc, "sst", "_FillValue")$value

ncdf4::nc_close(nc)

sst_box[sst_box == missing_value] <- NA_real_
sst_box[sst_box == fill_value] <- NA_real_

# Area-weighted mean using cos(latitude).
weights_lat <- cos(lat[lat_index] * pi / 180)
weights_array <- array(
  rep(weights_lat, each = length(lon_index)),
  dim = c(length(lon_index), length(lat_index))
)

monthly_mean <- vapply(seq_along(dates), function(i) {
  field <- sst_box[, , i]
  stats::weighted.mean(as.vector(field), as.vector(weights_array), na.rm = TRUE)
}, numeric(1))

sst_monthly <- tibble(
  date = dates,
  year = as.integer(format(date, "%Y")),
  month = as.integer(format(date, "%m")),
  sst = monthly_mean
) %>%
  filter(!is.na(sst))

# -------------------------------
# 4. Convert to annual anomaly
# -------------------------------

# Annual mean removes the seasonal cycle and makes EWS interpretation easier.
sst_annual <- sst_monthly %>%
  group_by(year) %>%
  summarise(
    sst = mean(sst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(year >= 1900)

# Baseline anomaly relative to 1900-2020 mean.
sst_annual <- sst_annual %>%
  mutate(
    sst_anomaly = sst - mean(sst, na.rm = TRUE)
  )

# Optional detrended anomaly:
# This removes the linear warming trend before calculating EWS.
# For presentation, show raw anomaly and EWS on detrended anomaly.
trend_model <- lm(sst_anomaly ~ year, data = sst_annual)
sst_annual <- sst_annual %>%
  mutate(
    sst_detrended = residuals(trend_model)
  )

# -------------------------------
# 5. Apply EWS indicators
# -------------------------------

window_years <- 20

sst_ews <- sst_annual %>%
  mutate(
    rolling_variance = rolling_var(sst_detrended, width = window_years),
    rolling_ac1 = rolling_ac1(sst_detrended, width = window_years)
  )

tau_var_real <- kendall_trend(sst_ews$rolling_variance)
tau_ac1_real <- kendall_trend(sst_ews$rolling_ac1)

real_summary <- tibble(
  dataset = "North Atlantic SST proxy",
  region = "45-60N, 60-10W",
  variable = "Annual detrended SST anomaly",
  window_years = window_years,
  tau_rolling_variance = round(tau_var_real, 3),
  tau_rolling_ac1 = round(tau_ac1_real, 3),
  interpretation = paste(
    "Illustrative AMOC-related proxy only;",
    "not direct evidence of AMOC tipping."
  )
)

# -------------------------------
# 6. Plots
# -------------------------------

p_sst <- sst_annual %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = sst_anomaly), color = "#2C3E50", linewidth = 0.6) +
  geom_smooth(
    aes(y = sst_anomaly),
    method = "loess",
    se = FALSE,
    color = "#C0392B",
    linewidth = 0.9
  ) +
  labs(
    title = "North Atlantic SST anomaly",
    subtitle = "Area-weighted annual mean over 45-60N, 60-10W; used as an AMOC-related proxy.",
    x = "Year",
    y = "SST anomaly"
  )

ggsave(
  file.path(output_dir, "01_north_atlantic_sst_anomaly.png"),
  p_sst,
  width = 8,
  height = 5,
  dpi = 300
)

p_ews <- sst_ews %>%
  select(year, rolling_variance, rolling_ac1) %>%
  tidyr::pivot_longer(
    cols = c(rolling_variance, rolling_ac1),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = dplyr::recode(
      indicator,
      rolling_variance = "Rolling variance",
      rolling_ac1 = "Rolling lag-1 autocorrelation"
    )
  ) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(color = "#34495E", linewidth = 0.6, na.rm = TRUE) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 1) +
  labs(
    title = "Early-warning indicators on North Atlantic SST proxy",
    subtitle = paste0(
      "Indicators are calculated on detrended annual SST anomalies using a ",
      window_years,
      "-year rolling window."
    ),
    x = "Year",
    y = "Indicator value"
  )

ggsave(
  file.path(output_dir, "02_north_atlantic_sst_ews.png"),
  p_ews,
  width = 8,
  height = 6,
  dpi = 300
)

readr::write_csv(
  sst_annual,
  file.path(output_dir, "north_atlantic_sst_annual.csv")
)

readr::write_csv(
  sst_ews,
  file.path(output_dir, "north_atlantic_sst_ews.csv")
)

readr::write_csv(
  real_summary,
  file.path(output_dir, "north_atlantic_sst_summary.csv")
)

message("\n=== North Atlantic SST proxy summary ===")
print(real_summary)

message("\nOutputs saved to: ", normalizePath(output_dir))

message("\nSlide wording:")
message(
  "As an illustrative real-world application, I apply the same EWS indicators ",
  "to a North Atlantic SST proxy related to AMOC variability. The analysis is ",
  "not interpreted as direct evidence of AMOC tipping, because SST proxies are ",
  "affected by external forcing, observational uncertainty, and non-tipping ",
  "climate variability."
)
