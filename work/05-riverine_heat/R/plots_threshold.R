
library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

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


kemmern

stations <- list(
  # salz = salz,
  # wolfsmuenster = wolfsmuenster,
  # schenkenau = schenkenau,
  # frankfurt_osthafen = frankfurt_osthafen,
  kemmern = kemmern ,
  kleinheubach = kleinheubach,
  # krotzenburg = krotzenburg,
  # mainleus = mainleus,
  schweinfurt = schweinfurt,
  schwuerbitz = schwuerbitz,
  # steinbach = steinbach,
  wuerzburg = wuerzburg,
  pettstadt = pettstadt
  # sachsenheim = sachsenheim
)


plots <- function(station, year_cons) {
  
  plot_data <- stations[[station]] %>%
    filter(year == year_cons) %>%
    filter(
      !is.na(date),
      is.finite(wt),
      is.finite(threshold)
    )
  
  ggplot(plot_data, aes(x = date)) +
    geom_ribbon(
      data = \(x) filter(x, heatwave, wt > threshold),
      aes(
        ymin = threshold,
        ymax = wt,
        group = heatwave_id
      ),
      fill = "red",
      alpha = 0.2
    )+
    geom_line(
      aes(y = wt),
      colour = "black",
      linewidth = 0.8
    ) +
    geom_line(
      aes(y = threshold),
      colour = "#E69F00",
      linetype = "dotted",
      linewidth = 0.9
    ) +
    geom_line(
      data = \(x) filter(x, heatwave),
      aes(y = wt, group = heatwave_id),
      colour = "red",
      linewidth = 1
    ) +
    labs(
      title = paste(station_labels[[station]], "(", year_cons, ")", sep = ""),
      x = "Date",
      y = expression("Water temperature (" * degree * "C)")
    ) +
    theme_minimal(base_size = 14)+
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

plot_list <- list()

for (station in names(stations)) {
  years <- unique(stations[[station]]$year)
  
  for (yr in years) {
    plot_list[[paste(station, yr, sep = "_")]] <- plots(station, yr)
  }
}

plot_list$kemmern_2018

# plot all years

plots_all_years <- function(station, year_cons) {
  
  plot_data <- stations[[station]] %>%
    mutate(
      date = as.Date(date),
      doy = yday(date)
    ) %>%
    filter(!is.na(wt), !is.na(threshold))
  
  ggplot(plot_data, aes(x = doy)) +
    
    # Background: all years
    geom_line(
      aes(y = wt, group = year),
      colour = "grey75",
      linewidth = 0.35,
      alpha = 0.7
    ) +
    
    # Severity (selected year)
    geom_ribbon(
      data = \(x) filter(x, year == year_cons, heatwave, wt > threshold),
      aes(
        ymin = threshold,
        ymax = wt,
        group = heatwave_id
      ),
      fill = "red",
      alpha = 0.20
    ) +
    
    # Selected year temperature
    geom_line(
      data = \(x) filter(x, year == year_cons),
      aes(y = wt),
      colour = "black",
      linewidth = 0.8
    ) +
    
    # Threshold
    geom_line(
      data = \(x) filter(x, year == year_cons),
      aes(y = threshold),
      colour = "#E69F00",
      linetype = "dashed",
      linewidth = 0.9
    ) +
    
    # Heatwave line
    geom_line(
      data = \(x) filter(x, year == year_cons, heatwave),
      aes(y = wt, group = heatwave_id),
      colour = "red",
      linewidth = 1
    ) +
    
    scale_x_continuous(
      breaks = c(121, 152, 182, 213, 244, 274),
      labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct")
    ) +
    
    labs(
      title = paste(station_labels[[station]], "(", year_cons, ")", sep = ""),
      x = "Month",
      y = expression("Water temperature (" * degree * "C)")
    ) +
    
    theme_minimal(base_size = 14)+
    theme(
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8
      ),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
    )
}




threshold_kemmern <- plots_all_years("kemmern", 2018)

ggsave("work/05-riverine_heat/figures/05-threshold_kemmern.png", plot = threshold_kemmern, width = 10, height = 8, dpi = 300)



