library(terra)
library(dplyr)
library(forcats)
library(ggplot2)
library(sf)

## Biome 6000 #####

# Loading the Biome6000 dataset
data <- read.csv("https://researchdata.reading.ac.uk/99/1/BIOME%206000%20DB%20classified%20plotfile_v1.csv")
table(data$Biome.6000.Consolidated.Name)

# Ensure proper column names 
colnames(data) <- c("Target.age..ka.", "Site.Name", "Latitude", "Longitude", "Biome.6000.Consolidated.Name", "Biome.6000.biome.code", "Biome.4.2.Biome.6000.common.biomes", "MegaBiomes..Scheme.2.", "BIOME.4.2.names.from.model.code", "Biome.4.2.code", "BIOME4.2.BIOME.6000.equivalencies")

# Fix potential UTF-8 issues in the data
data <- data %>% 
  mutate(
    Site.Name = iconv(Site.Name, from = "UTF-8", to = "UTF-8", sub = ""),
    Biome.6000.Consolidated.Name = iconv(Biome.6000.Consolidated.Name, from = "UTF-8", to = "UTF-8", sub = "")
  )%>% filter(Target.age..ka. == 0)

# Filter the data to include only the relevant columns
data <- data%>% select(Site.Name, Latitude, Longitude, Biome.6000.Consolidated.Name)

#Fatorize the Biome.6000.Consolidated.Name column and fix names
data$Biome.6000.Consolidated.Name<- as.factor(data$Biome.6000.Consolidated.Name)
levels(data$Biome.6000.Consolidated.Name)<-gsub("[- ]", ".", levels(data$Biome.6000.Consolidated.Name))

#Reclassifying
data_c <- data %>%
  filter(!Biome.6000.Consolidated.Name %in% c("wet.sclerophyll.forest", "cool.grassland", "temperate.xerophytic.woods/scrub")) %>%
  mutate(Biome.6000.Consolidated.Name = droplevels(Biome.6000.Consolidated.Name)) %>%
  mutate(Biome.6000.Consolidated.Name= fct_collapse(Biome.6000.Consolidated.Name, 
                                                    "temperate.deciduous.broadleaf.forest" = c("temperate.deciduous.broadleaf.forest", "cold.mixed.forest", "temperate.evergreen.needleleaf.forest"),
                                                    "xerophytic.woods/scrub"=c("xerophytic.woods/scrub","temperate.grassland.and.xerophytic.shrubland", "temperate.or.tropical.grassland.and.xerophytic.shrubland"),
                                                    "warm.temperate.evergreen.broadleaf.and.mixed.forest"= c("warm.temperate.evergreen.broadleaf.and.mixed.forest","warm.temperate.evergreen.broadleaf.forest", "temperate.evergreen.needleleaf.forest"),
                                                    "erect.dwarf.shrub.tundra"=c("erect.dwarf.shrub.tundra", "prostrate.dwarf.shrub.tundra"),
                                                    "cool.temperate.rainforest"=c("cool.temperate.rainforest", "warm.temperate.rainforest")
                                                    
  ))

# only keeping one observation per site, two when tey have same frequency
data_cn <- data_c %>%
  group_by(Site.Name, Biome.6000.Consolidated.Name) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Site.Name) %>%
  filter(Count == max(Count)) %>%
  select(Site.Name, Biome.6000.Consolidated.Name) %>%
  left_join(
    data_c %>%                                   
      select(Site.Name, Latitude, Longitude) %>%
      distinct(),
    by = "Site.Name"
  )
#DEM covariates####


dem <- rast("seminar24/data/dem/merged_dem_land.tif")
slope<-rast("seminar24/data/dem/slope_layer.tif")
curvature<-rast("seminar24/data/dem/curvature.tif")
tpi<-rast("seminar24/data/dem/tpi_15.tif")


cov_stack_dem<-c(dem, slope, curvature, tpi)
 

####CHELSA covariates####

#loading all files and stacking togheter
bio_files <- paste0("seminar24/data/CHELSEA/bio/CHELSA_bio10_", 
                    sprintf("%02d", 1:18), ".tif")
bio_list <- lapply(bio_files, rast)
bio_stack <- rast(bio_list)

tmean_files <- paste0("seminar24/data/CHELSEA/tmean/CHELSA_temp10_", 
                      sprintf("%02d", 1:12), "_1979-2013_V1.2_land.tif")
tmean_list <- lapply(tmean_files, rast)
tmean_stack <- rast(tmean_list)


tmin_files <- paste0("seminar24/data/CHELSEA/tmin/CHELSA_tmin10_", 
                     sprintf("%02d", 1:12), "_1979-2013_V1.2_land.tif")
tmin_list <- lapply(tmin_files, rast)
tmin_stack <- rast(tmin_list)


tmax_files <- paste0("seminar24/data/CHELSEA/tmax/CHELSA_tmax10_", 
                     sprintf("%02d", 1:12), "_1979-2013_V1.2_land.tif")
tmax_list <- lapply(tmax_files, rast)
tmax_stack <- rast(tmax_list)


prec_files <- paste0("seminar24/data/CHELSEA/prec/CHELSA_prec_", 
                     sprintf("%02d", 1:12), "_V1.2_land.tif")
prec_list <- lapply(prec_files, rast)
prec_stack <- rast(prec_list)

cov_stack_chelsea<-rast(list(bio_stack,tmin_stack,tmax_stack,tmean_stack,prec_stack))


# Making final dataset with covariates ######

extract_nearest <- function(raster_stack, points, max_distance = 5000) {
  # First try exact extraction
  extracted_values <- terra::extract(raster_stack, points)
  
  # Identify rows with NA values
  na_rows <- which(is.na(extracted_values[,2]))
  
  if(length(na_rows) > 0) {
    # Create a spatial vector of just the NA points
    points_na <- points[na_rows]
    
    # Loop through NA rows to find nearest point
    for(i in seq_along(na_rows)) {
      # Create a buffer around the point
      buffer <- terra::buffer(points_na[i], width = max_distance)
      
      # Try to extract values within the buffer
      buffer_extract <- terra::extract(raster_stack, buffer)
      
      # Remove NA values from buffer extraction
      valid_extracts <- buffer_extract[!is.na(buffer_extract[,2]),]
      
      if(nrow(valid_extracts) > 0) {
        # If valid extracts exist, replace the NA value
        extracted_values[na_rows[i],] <- valid_extracts[1,]
      }
    }
  }
  
  return(extracted_values)
}

#extracting points in dataset
points <- vect(data_cn, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")

#extracting points from the covariates raster, with a radius of 5 km for missing values
extr_dem<-extract_nearest(cov_stack_dem, points)[,-1]
extr_chels<-extract_nearest(cov_stack_chelsea, points)[,-1]
colnames(extr_dem)[4]<-"tpi"


#joining the datasets
data_cn <- cbind(data_cn, extr_dem, extr_chels)

#removing observations with NAs
df_nc <- na.omit(data_cn)

#saving dataset
write.csv(df_nc, "seminar24/data/model_data.csv", row.names = FALSE)

# Plotting the points of the Biome 6000 dataset #####


world <- ne_countries(scale = "medium", returnclass = "sf")

points_sf <- st_as_sf(df_nc, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(data = world) +
  geom_sf(fill = "antiquewhite", colour = "grey50") +
  geom_sf(
    data  = points_sf,
    colour = "darkgreen",
    size   = 0.3,                # very small points
    alpha  = 0.7
  ) +
  labs(
    title    = "Locations of Biome 6000 Samples"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", colour = NA),
    panel.grid       = element_line(colour = "white", linetype = "dashed"),
    plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle    = element_text(hjust = 0.5, size = 12)
  )

