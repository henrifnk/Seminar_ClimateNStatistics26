library(terra)



# Function to load rasters
load_rasters <- function(path, prefix, indices, suffix) {
  files <- paste0(path, "/", prefix, indices, suffix)
  rast_list <- lapply(files, rast)
  return(do.call(c, rast_list))  # Combine into a single SpatRaster
}


#RCP 2.6 ###########


base_path <- "seminar24/data/Climate_projection/rcp26_61_80"

# Bioclimatic variables (BIO1 to BIO18)
bio_stack_26 <- load_rasters(
  path   = file.path(base_path, "bio"),
  prefix = "CHELSA_bio_mon_CanESM2_rcp26_r1i1p1_g025.nc_",
  indices = 1:18,
  suffix = "_2061-2080_V1.2.tif"
)

# Precipitation 
prec_stack_26 <- load_rasters(
  path   = file.path(base_path, "prec"),
  prefix = "CHELSA_pr_mon_CanESM2_rcp26_r1i1p1_g025.nc_",
  indices = 1:12,
  suffix = "_2061-2080.tif"
)

# Tmax
tmax_stack_26 <- load_rasters(
  path   = file.path(base_path, "tmax"),
  prefix = "CHELSA_tasmax_mon_CanESM2_rcp26_r1i1p1_g025.nc_",
  indices = 1:12,
  suffix = "_2061-2080_V1.2.tif"
)

# Tmean
tmean_stack_26 <- load_rasters(
  path   = file.path(base_path, "tmean"),
  prefix = "CHELSA_tas_mon_CanESM2_rcp26_r1i1p1_g025.nc_",
  indices = 1:12,
  suffix = "_2061-2080_V1.2.tif"
)

# Tmin 
tmin_stack_26 <- load_rasters(
  path   = file.path(base_path, "tmean"),
  prefix = "CHELSA_tasmin_mon_CanESM2_rcp26_r1i1p1_g025.nc_",
  indices = 1:12,
  suffix = "_2061-2080_V1.2.tif"
)


cov_stack_chelsa_26<- c(bio_stack_26, tmin_stack_26, tmax_stack_26, tmean_stack_26,  prec_stack_26)


# RCP 4.5 ###########

path_base <- "seminar24/data/Climate_projection/rcp45_61_80"

# bio layers
bio_stack_45 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp45_61_80/bio",
  prefix = "CHELSA_bio_mon_CanESM2_rcp45_r1i1p1_g025.nc_",
  indices = 1:18,
  suffix = "_2061-2080_V1.2.tif"
)

prec_stack_45 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp45_61_80/prec",
  prefix = "CHELSA_pr_mon_CanESM2_rcp45_r1i1p1_g025.nc_",
  indices = 1:12,
  suffix = "_2061-2080.tif"
)

tmin_stack_45 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp45_61_80/tmin",
  prefix = "CHELSA_tasmin_mon_CanESM2_rcp45_r1i1p1_g025.nc_",
  indices = 1:12
  suffix = "_2061-2080_V1.2.tif"
)

tmax_stack_45 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp45_61_80/tmax",
  prefix = "CHELSA_tasmax_mon_CanESM2_rcp45_r1i1p1_g025.nc_",
  indices = 1:12,
  suffix = "_2061-2080_V1.2.tif"
)

tmean_stack_45 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp45_61_80/tmean",
  prefix = "CHELSA_tas_mon_CanESM2_rcp45_r1i1p1_g025.nc_",
  indices = 1:12,
  suffix = "_2061-2080_V1.2.tif"
)

cov_stack_chelsa_45<- c(bio_stack_45, tmin_stack_45, tmax_stack_45, tmean_stack_45,  prec_stack_45)


# RCP 8.5 ###########

base_path <- "seminar24/data/Climate_projection/rcp85_61_80"



bio_stack_85 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp85_61_80/bio",
  prefix = "CHELSA_bio_mon_CanESM2_rcp85_r1i1p1_g025.nc_",
  indices = 1:18,  # BIO1 to BIO18
  suffix = "_2061-2080_V1.2.tif"
)

prec_stack_85 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp85_61_80/prec",
  prefix = "CHELSA_pr_mon_CanESM2_rcp85_r1i1p1_g025.nc_",
  indices = 1:12,  # 1 to 12 (Months)
  suffix = "_2061-2080.tif"
)

tmax_stack_85 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp85_61_80/tmax",
  prefix = "CHELSA_tasmax_mon_CanESM2_rcp85_r1i1p1_g025.nc_",
  indices = 1:12,  # 1 to 12 (Months)
  suffix = "_2061-2080_V1.2.tif"
)

tmean_stack_85 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp85_61_80/tmean",
  prefix = "CHELSA_tas_mon_CanESM2_rcp85_r1i1p1_g025.nc_",
  indices = 1:12,  # 1 to 12 (Months)
  suffix = "_2061-2080_V1.2.tif"
)

tmin_stack_85 <- load_rasters(
  path = "seminar24/data/Climate_projection/rcp85_61_80/tmin",
  prefix = "CHELSA_tasmin_mon_CanESM2_rcp85_r1i1p1_g025.nc_",
  indices = 1:12,  # 1 to 12 (Months)
  suffix = "_2061-2080_V1.2.tif"
)

cov_stack_chelsa_85<- c(bio_stack_85, tmin_stack_85, tmax_stack_85, tmean_stack_85,  prec_stack_85)


## Europe covariate stack for all RCPs #######

#extent of Europe
ext_europe <- ext(-25, 45, 34, 72)
#names for the model
names_c<- c(
  "merged_dem_land", "slope_layer", "curvature", "tpi",
  "CHELSA_bio10_01", "CHELSA_bio10_02", "CHELSA_bio10_03", "CHELSA_bio10_04", "CHELSA_bio10_05", 
  "CHELSA_bio10_06", "CHELSA_bio10_07", "CHELSA_bio10_08", "CHELSA_bio10_09", "CHELSA_bio10_10", 
  "CHELSA_bio10_11", "CHELSA_bio10_12", "CHELSA_bio10_13", "CHELSA_bio10_14", "CHELSA_bio10_15", 
  "CHELSA_bio10_16", "CHELSA_bio10_17", "CHELSA_bio10_18",
  "CHELSA_tmin10_01_197", "CHELSA_tmin10_02_197", "CHELSA_tmin10_03_197", "CHELSA_tmin10_04_197", 
  "CHELSA_tmin10_05_197", "CHELSA_tmin10_06_197", "CHELSA_tmin10_07_197", "CHELSA_tmin10_08_197", 
  "CHELSA_tmin10_09_197", "CHELSA_tmin10_10_197", "CHELSA_tmin10_11_197", "CHELSA_tmin10_12_197", 
  "CHELSA_tmax10_01_197", "CHELSA_tmax10_02_197", "CHELSA_tmax10_03_197", "CHELSA_tmax10_04_197", 
  "CHELSA_tmax10_05_197", "CHELSA_tmax10_06_197", "CHELSA_tmax10_07_197", "CHELSA_tmax10_08_197", 
  "CHELSA_tmax10_09_197", "CHELSA_tmax10_10_197", "CHELSA_tmax10_11_197", "CHELSA_tmax10_12_197", 
  "CHELSA_temp10_01_197", "CHELSA_temp10_02_197", "CHELSA_temp10_03_197", "CHELSA_temp10_04_197", 
  "CHELSA_temp10_05_197", "CHELSA_temp10_06_197", "CHELSA_temp10_07_197", "CHELSA_temp10_08_197", 
  "CHELSA_temp10_09_197", "CHELSA_temp10_10_197", "CHELSA_temp10_11_197", "CHELSA_temp10_12_197", 
  "CHELSA_prec_01_V1.2_", "CHELSA_prec_02_V1.2_", "CHELSA_prec_03_V1.2_", "CHELSA_prec_04_V1.2_", 
  "CHELSA_prec_05_V1.2_", "CHELSA_prec_06_V1.2_", "CHELSA_prec_07_V1.2_", "CHELSA_prec_08_V1.2_", 
  "CHELSA_prec_09_V1.2_", "CHELSA_prec_10_V1.2_", "CHELSA_prec_11_V1.2_", "CHELSA_prec_12_V1.2_"
)
#dem stack, same for all 
cov_stack_dem_europe <- crop(cov_stack_dem, ext_europe)

#present 

cov_stack_chelsea_europe_pres <- crop(cov_stack_chelsa, ext_europe)
cov_stack_europe_pres <- c(cov_stack_dem_europe, cov_stack_chelsea_europe_pres)
names(cov_stack_europe_pres) <- names_c

#saving raster
writeRaster(cov_stack_europe_pres, "seminar24/data/cov_stack_europe_pres.tif", overwrite=TRUE)

# RCP 2.6

cov_stack_chelsea_europe_26 <- crop(cov_stack_chelsea_26, ext_europe)
cov_stack_europe_26 <- c(cov_stack_dem_europe, cov_stack_chelsea_europe_26)
names(cov_stack_europe_26) <- names_c  

#saving raster 
#writeRaster(cov_stack_europe_26, "seminar24/data/cov_stack_europe.tif", overwrite=TRUE)
writeRaster(cov_stack_europe_26, "work/05-naturalveg/data/cov_stack_europe_26.tif", overwrite=TRUE)

# RCP 4.5

cov_stack_chelsea_europe_45 <- crop(cov_stack_chelsa_45, ext_europe)
cov_stack_europe_45 <- c(cov_stack_dem_europe, cov_stack_chelsea_europe_45)
names(cov_stack_europe_45) <- names_c

writeRaster(cov_stack_europe_45, "work/05-naturalveg/data/cov_stack_europe_45.tif", overwrite=TRUE)


# RCP 8.5

cov_stack_chelsea_europe_85 <- crop(cov_stack_chelsa_85, ext_europe)
cov_stack_europe_85 <- c(cov_stack_dem_europe, cov_stack_chelsea_europe_85)
names(cov_stack_europe_85) <- names_c

writeRaster(cov_stack_europe_85, "work/05-naturalveg/data/cov_stack_europe_85.tif", overwrite=TRUE)








 