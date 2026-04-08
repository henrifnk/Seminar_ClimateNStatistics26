library(terra)

# Data used in this file is not available on github due to size (around 2.5 GB per file),
# but can be downloaded from the following google drive folder:
# https://drive.google.com/drive/folders/1iCA2c7N7u4rhhmqVpUzfAXzlfQOI0S5B?usp=sharing


# Predictions #####

#loading covariates raster for predictions
#data not available in GitHub due to size (around 2.5 GB per file), 
#available form google drive under links:

#cov_pres <- rast("seminar24/data/cov_stack_europe_pres.tif")  
#cov_26<- rast("seminar24/data/cov_stack_europe_26.tif")
#cov_45<- rast("seminar24/data/cov_stack_europe_45.tif")
#cov_85<- rast("seminar24/data/cov_stack_europe_85.tif")

#loading model 

model <- readRDS("work/05-naturalveg/results/rf_model_prof_fin") 

#predicting probabilities for all scenarios, computationally intensive task
""

preds_pres <- rast("C:/Users/elena/Desktop/PNVmaps/seminar24/data/predictions_europe_present_final.tif")
preds_26 <- rast("C:/Users/elena/Desktop/PNVmaps/seminar24/data/preds_26.tif")
preds_45 <- rast("C:/Users/elena/Desktop/PNVmaps/seminar24/data/preds_45.tif")
preds_85 <- rast("C:/Users/elena/Desktop/PNVmaps/seminar24/data/preds_85.tif")
preds_pres <- predict(cov_pres, model, type = "response", na.rm = TRUE)
preds_26 <- predict(cov_26, model, type = "response", na.rm = TRUE)
preds_45 <- predict(cov_45, model, type = "response", na.rm = TRUE)
preds_85 <- predict(cov_85, model, type = "response", na.rm = TRUE)



# Save the prediction result as a TIFF file (around 1 GB per file)
# predictions are not on Github due to size, but can be downloaded from the google drive folder

writeRaster(preds_pres, "work/05-naturalveg/results/predictions_eu_present.tif", overwrite = TRUE)
writeRaster(preds_26, "work/05-naturalveg/results/predictions_eu_26.tif", overwrite = TRUE)
writeRaster(preds_45, "work/05-naturalveg/results/predictions_eu_45.tif", overwrite = TRUE)
writeRaster(preds_85, "work/05-naturalveg/results/predictions_eu_85.tif", overwrite = TRUE)





# Hard labels predictions ####

#layers names to ensure consistency:
levels_names_p<- c(
  "cold.deciduous.forest",
  "cold.evergreen.needleleaf.forest",  
  "temperate.deciduous.broadleaf.forest" ,
  "cool.temperate.evergreen.needleleaf.and.mixed.forest",
  "cool.evergreen.needleleaf.forest",
  "cool.temperate.rainforest", 
  "cool.mixed.forest", 
  "desert", 
  "erect.dwarf.shrub.tundra", 
  "graminoid.and.forb.tundra" , 
  "low.and.high.shrub.tundra", 
  "steppe", 
  "warm.temperate.evergreen.broadleaf.and.mixed.forest",
  "temperate.evergreen.needleleaf.open.woodland", 
  "xerophytic.woods.scrub",
  "tundra"
)

levels_names<- c(
  "cold.deciduous.forest",
  "cold.evergreen.needleleaf.forest",  
  "temperate.deciduous.broadleaf.forest" , 
  "cool.evergreen.needleleaf.forest",
  "cool.temperate.rainforest", 
  "cool.mixed.forest", 
  "desert", 
  "erect.dwarf.shrub.tundra", 
  "graminoid.and.forb.tundra" , 
  "low.and.high.shrub.tundra", 
  "steppe", 
  "warm.temperate.evergreen.broadleaf.and.mixed.forest",
  "temperate.evergreen.needleleaf.open.woodland", 
  "xerophytic.woods.scrub",
  "tundra"
)

hard_pres <- as.factor(which.max(preds_pres))
levels(hard_pres)[[1]][,2]<- levels_names_p

hard_26 <- as.factor(which.max(preds_26))
levels(hard_26)[[1]][,2]<- levels_names

hard_45 <- as.factor(which.max(preds_45))
levels(hard_45)[[1]][,2]<- levels_names

hard_85 <- as.factor(which.max(preds_85))
levels(hard_85)[[1]][,2]<- levels_names

# Save the hard classification result as a TIFF file

writeRaster(hard_pres, "work/05-naturalveg/results/hard_class_present.tif", overwrite = TRUE)
writeRaster(hard_26, "work/05-naturalveg/results/hard_class_26.tif", overwrite = TRUE)
writeRaster(hard_45, "work/05-naturalveg/results/hard_class_45.tif", overwrite = TRUE)
writeRaster(hard_85, "work/05-naturalveg/results/hard_class_85.tif", overwrite = TRUE)


#Results plots #######
# hard labels predictions are available on github for all scenarios and can be 
#loaded from the repo

hard_pres<-rast("work/05-naturalveg/results/hard_class_present.tif")
hard_26<-rast("work/05-naturalveg/results/hard_class_26.tif")
hard_45<-rast("work/05-naturalveg/results/hard_class_45.tif")
hard_85<-rast("work/05-naturalveg/results/hard_class_85.tif")



# colors for the map for the present 
class_colors_p <- c(
  "cold.deciduous.forest"                           = "#0B6623", 
  "cold.evergreen.needleleaf.forest"                = "#004225", 
  "temperate.deciduous.broadleaf.forest"            = "#556B2F",
  "cool.temperate.evergreen.needleleaf.and.mixed.forest" = "#32CD32",
  "cool.temperate.rainforest"                       = "#013220", 
  "cool.evergreen.needleleaf.forest"                = "#008080", 
  "cool.mixed.forest"                               = "#228B22",
  "desert"                                          = "lightgrey", 
  "erect.dwarf.shrub.tundra"                        = "#C2B280", 
  "graminoid.and.forb.tundra"                       = "#BDB76B", 
  "low.and.high.shrub.tundra"                       = "#8B4513", 
  "steppe"                                          = "#D2B48C", 
  "warm.temperate.evergreen.broadleaf.and.mixed.forest" = "#9ACD32", 
  "temperate.evergreen.needleleaf.open.woodland"    = "#3CB371", 
  "xerophytic.woods.scrub"                       ="#A0522D",
  "tundra"                                          = "#B0C4DE" 
)

# colors for future predcitions, class: 
#"cool.temperate.evergreen.needleleaf.and.mixed.forest" 
# was not predicted
class_colors <- c(
  "cold.deciduous.forest"                           = "#0B6623", 
  "cold.evergreen.needleleaf.forest"                = "#004225", 
  "temperate.deciduous.broadleaf.forest"            = "#556B2F",
  "cool.evergreen.needleleaf.forest"                = "#008080", 
  "cool.temperate.rainforest"                       = "#013220", 
  "cool.mixed.forest"                               = "#228B22",
  "desert"                                          = "lightgrey", 
  "erect.dwarf.shrub.tundra"                        = "#C2B280", 
  "graminoid.and.forb.tundra"                       = "#BDB76B", 
  "low.and.high.shrub.tundra"                       = "#8B4513", 
  "steppe"                                          = "#D2B48C", 
  "warm.temperate.evergreen.broadleaf.and.mixed.forest" = "#9ACD32", 
  "temperate.evergreen.needleleaf.open.woodland"    = "#3CB371", 
  "xerophytic.woods.scrub"                       ="#A0522D",
  "tundra"                                          = "#B0C4DE" 
)

windows()

plot(hard_pres, col = class_colors_p, main = "Biome Classes Present", maxcell = Inf, colNA = "lightblue")
plot(hard_26, col = class_colors, main = "Biome Classes - 2061-2080 - RCP 2.6", maxcell = Inf, colNA = "lightblue")
plot(hard_45, col = class_colors, main = "Biome Classes - 2041-2060 - RCP 4.5", maxcell = Inf, colNA = "lightblue")
plot(hard_85, col = class_colors, main = "Biome Classes - 2061-2080 - RCP 8.5", maxcell = Inf, colNA = "lightblue")



#Scaled Shannon Entropy Index (SSEI) ####

#function to compute SSEI
calculate_ssei <- function(probability_stack, b) {
  
  
  # no  log(0) through small values
  probability_stack[probability_stack == 0] <- 1e-10
  
  # First part of  SSEI: normalized on b
  entropy_part <- -sum(probability_stack * log(probability_stack, base=b), na.rm = TRUE)
  
  
  # Final SSEI
  ssei <- entropy_part 
  return(ssei)
}

#ssei raster for all scenarios
ssei_pres<-calculate_ssei(preds_pres,21)
ssei_26<-calculate_ssei(preds_26,21)
ssei_45<-calculate_ssei(preds_45,21)
ssei_85<-calculate_ssei(preds_85,21)

#saving files
writeRaster(ssei_pres, "work/05-naturalveg/results/ssei_present.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(ssei_26, "work/05-naturalveg/results/ssei_future_26.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(ssei_45, "work/05-naturalveg/results/ssei_future_45.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(ssei_85, "work/05-naturalveg/results/ssei_future_85.tif", filetype = "GTiff", overwrite = TRUE)

# ssei raster not available on GitHub due to size, 
#but can be downloaded from the google drive folder

#plotting results, ensuring all have same scale
windows()
plot(ssei_pres, main = "SSEI Present Predictions", maxcell = Inf, range = c(0, 0.9))
plot(ssei_26, main = "SSEI Predictions RCP 2.6", maxcell = Inf, range = c(0, 0.9))
plot(ssei_45, main = "SSEI Predictions RCP 4.5", maxcell = Inf, range = c(0, 0.9))
plot(ssei_85, main = "SSEI Predictions RCP 8.5", maxcell = Inf,  range = c(0, 0.9))











