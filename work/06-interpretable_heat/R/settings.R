library(ggplot2)

labels_station <- c(
  "Main_Kemmern"                       = "Main (Kemmern)",
  "Main_Kleinheubach"                  = "Main (Kleinheubach)",
  "Main_Schweinfurt"                   = "Main (Schweinfurt)",
  "Main_Wuerzburg"                     = "Main (Würzburg)",
  "Main_Schwuerbitz"                   = "Main (Schwürbitz)",
  "Regnitz_Pettstadt"                  = "Regnitz (Pettstadt)",
  "Main_Steinbach"                     = "Main (Steinbach)",
  "Main_Frankfurt_Osthafen"            = "Main (Frankfurt Osthafen)",
  "Fraenkische_Saale_Wolfsmuenster"    = "Fränkische Saale (Wolfsmünster)",
  "Wern_Sachsenheim"                   = "Wern (Sachsenheim)",
  "Fraenkische_Saale_Salz"             = "Fränkische Saale (Salz)",
  "Itz_Schenkenau"                     = "Itz (Schenkenau)",
  "Main_Mainleus"                      = "Main (Mainleus)",
  "Main_Krotzenburg"                   = "Main (Krotzenburg)"
)

# Dynamic + target variable labels
labels_dynamic <- c(
  "wt"       = "Water Temperature",
  "Ta_C"     = "Air Temperature",
  "P_mm"     = "Precipitation",
  "wind_ms"  = "Wind Speed",
  "rad_whm2" = "Solar Radiation",
  "relhum"   = "Relative Humidity",
  "Q"        = "Discharge"
)

# Static feature labels
labels_static <- c(
  "DEM"              = "Elevation",
  "Slope"            = "Slope",
  "Fraction_Forest"  = "Forest Cover",
  "Imperv_500m"      = "Impervious Surface (500m)",
  "Imperv_1000m"     = "Impervious Surface (1000m)",
  "Imperv_2000m"     = "Impervious Surface (2000m)",
  "Imperv_3000m"     = "Impervious Surface (3000m)",
  "Flusskilometer"   = "Distance from River Mouth",
  "upstream_km"      = "Distance from River Source",
  "Gesamtlaenge_Fluss" = "Total River Length"
)

colors <- list(
  dynamic = "#2a78d6",
  static = "#FFA500",
  static_accent = "#FF8C00"
)

labels_all_vars <- c(labels_dynamic, labels_static)

set_theme(theme_minimal() +
            theme(
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              strip.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 11)
            ))

