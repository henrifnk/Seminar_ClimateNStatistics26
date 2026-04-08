## Data source: https://doi.org/10.5281/zenodo.12749575
# Install and load required packages if needed
if (!requireNamespace("ncdf4", quietly = TRUE)) install.packages("ncdf4")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(ncdf4)
library(dplyr)
library(lubridate)
library(ggplot2)

# Set directory path containing the NetCDF files
nc_dir <- "C:/Users/Kathrin/Documents/Dokumente/Master/WiSe24_25/Statistik/BSSP370cmip6.0480013/BSSP370cmip6.0480013"

# Get list of all .nc files in the directory
nc_files <- list.files(path = nc_dir, pattern = "\\.nc$", full.names = TRUE)

# Function to extract temperature data from a NetCDF file
extract_temp_data <- function(file_path) {
  nc <- nc_open(file_path)
  
  temp_var <- if ("T" %in% names(nc$var)) {
    "T"
  } else if ("TREFHT" %in% names(nc$var)) {
    "TREFHT"
  } else {
    print(paste("Available variables in", basename(file_path), ":"))
    print(names(nc$var))
    nc_close(nc)
    return(NULL)
  }
  
  temp_data <- ncvar_get(nc, temp_var)
  time_data <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  nc_close(nc)
  
  if (grepl("days since", time_units)) {
    ref_date <- as.Date(sub("days since ", "", time_units))
    dates <- ref_date + time_data
  } else {
    dates <- seq_along(time_data)
  }
  
  mean_temp <- if (length(dim(temp_data)) > 1) {
    apply(temp_data, length(dim(temp_data)), mean, na.rm = TRUE)
  } else {
    temp_data
  }
  
  file_name <- basename(file_path)
  ensemble_info <- regmatches(file_name, regexpr("ens\\d+", file_name))
  start_date <- regmatches(file_name, regexpr("\\d{4}-\\d{2}-\\d{2}", file_name))
  
  data.frame(
    file = file_name,
    ensemble = if (length(ensemble_info) > 0) ensemble_info else NA,
    start_date = if (length(start_date) > 0) start_date else NA,
    date = dates,
    temperature = mean_temp
  )
}

# Process all files
all_results <- data.frame()

for (file in nc_files) {
  cat("Processing file:", basename(file), "\n")
  result <- extract_temp_data(file)
  if (!is.null(result)) {
    all_results <- rbind(all_results, result)
  }
}

# Calculate the coldest average temperatures over the entire time period
avg_results <- all_results %>%
  group_by(file, ensemble, start_date) %>%
  summarize(mean_temp = mean(temperature, na.rm = TRUE), .groups = "drop")

coldest_avg_3 <- avg_results %>%
  arrange(mean_temp) %>%
  head(3)

# Calculate the coldest minimum values over the entire time period
min_results <- all_results %>%
  group_by(file, ensemble, start_date) %>%
  summarize(min_temp = min(temperature, na.rm = TRUE), .groups = "drop")

coldest_min_3 <- min_results %>%
  arrange(min_temp) %>%
  head(3)

# Extract files of the coldest minima and average values
coldest_avg_files <- unique(coldest_avg_3$file)
coldest_min_files <- coldest_min_3$file

# Mark in the entire dataset
all_results <- all_results %>%
  mutate(is_coldest_avg = ifelse(file %in% coldest_avg_files, "Coldest Avg 3", "All others"),
         is_coldest_min = ifelse(file %in% coldest_min_files, "Coldest Min 3", "All others"))

# Define 3 colors for the coldest scenarios
color_map <- c("blue", "red", "green")

# Plot of the coldest average temperatures
ggplot(all_results, aes(x = date, y = temperature, group = file, color = is_coldest_avg)) +
  geom_line(data = all_results %>% filter(is_coldest_avg == "All others"), color = "grey", alpha = 0.3) +
  geom_line(data = all_results %>% filter(is_coldest_avg == "Coldest Avg 3"), aes(color = ensemble), size = 1) +
  scale_color_manual(values = color_map) +
  labs(title = "Temperature Time Series (3 Coldest Average Highlighted)",
       y = "Temperature (°C)",
       color = "Ensemble") +
  theme_classic()

ggsave("coldest_avg_storylines_plot.png", width = 10, height = 6)

# Plot of the coldest minimum temperatures
ggplot(all_results, aes(x = date, y = temperature, group = file, color = is_coldest_min)) +
  geom_line(data = all_results %>% filter(is_coldest_min == "All others"), color = "grey", alpha = 0.3) +
  geom_line(data = all_results %>% filter(is_coldest_min == "Coldest Min 3"), aes(color = ensemble), size = 1) +
  scale_color_manual(values = color_map) +
  labs(title = "Temperature Time Series (3 Coldest Minima Highlighted)",
       y = "Temperature (°C)",
       color = "Ensemble") +
  theme_classic()

ggsave("coldest_min_storylines_plot.png", width = 10, height = 6)

# Summary statistics for both cases
cat("\nSummary statistics for the 3 coldest average storylines:\n")
coldest_avg_3 %>%
  print()

cat("\nSummary statistics for the 3 coldest minima storylines:\n")
coldest_min_3 %>%
  print()

