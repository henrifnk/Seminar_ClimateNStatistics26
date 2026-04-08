#### ERA5 CLIMATE PATTERN ANALYSIS ######

# PACKAGES - Load all required libraries
library(ggplot2)
library(terra)
library(dplyr)
library(maps)
library(ggrepel)
library(sf)
library(cowplot)
library(SPEI)
library(viridis)

#### DATA LOADING AND PREPROCESSING ####
era5 <- rast("./data/ERA5/1950_1990_monthly_mm.nc")
access <- rast("./data/ACCESS/pr_monthly_sums_ACCESS-ESM1-5_1950-1990.nc")

common_extent <- ext(
  max(c(ext(era5)[1], ext(access)[1])),   
  min(c(ext(era5)[2], ext(access)[2])),   
  max(c(ext(era5)[3], ext(access)[3])),   
  min(c(ext(era5)[4], ext(access)[4]))    
)

target_res <- rast(common_extent, resolution = c(2, 2))
print(target_res)

era5_regridded <- resample(era5, target_res, method = "bilinear")

plot(era5_regridded[[1]])
print(era5_regridded)

writeCDF(era5_regridded, "./data/ERA5/ERA5_1950-1990_regridded.nc", 
         overwrite = TRUE)

#### CALCULATE SPI (STANDARDIZED PRECIPITATION INDEX) ####
raster_data <- era5_regridded

spi_12_matrix <- matrix(NA, nrow = ncell(raster_data), ncol = 492)

for (i in 1:ncell(raster_data)) {
  cell_coords <- xyFromCell(raster_data, i)
  
  cell_values <- terra::extract(raster_data, cell_coords)
  
  spi_12_result <- spi(as.numeric(cell_values), scale = 12, 
                       distribution = "Gamma", fit = "ub-pwm", 
                       na.rm = TRUE, verbose = FALSE)
  
  spi_12_matrix[i, ] <- spi_12_result$fitted
}

spi_raster <- raster_data  
values(spi_raster) <- spi_12_matrix  

writeCDF(spi_raster, "./data/ERA5/SPI12_ERA5_1950-1990.nc", 
         overwrite = TRUE, varname = "spi12", 
         longname = "SPI-12", zname = "time")

#### CROP POLAR REGIONS ####
spi_era5_full <- rast("./data/ERA5/SPI12_ERA5_1950-1990.nc")
print(ext(spi_era5_full))

spi_era5_rotated <- rotate(spi_era5_full)
print(ext(spi_era5_rotated))

spi_no_poles <- crop(spi_era5_rotated, ext(-180, 180, -70, 70))
print(ext(spi_no_poles))

writeCDF(spi_no_poles, "./data/ERA5/SPI12_ERA5_no_poles.nc", 
         overwrite = TRUE, varname = "spi12", 
         longname = "SPI-12", zname = "time")

spi_era5_cropped <- rast("./data/ERA5/SPI12_ERA5_no_poles.nc")
print(ext(spi_era5_cropped))

jpeg(filename = "./output/spi_era5_plot.jpg", 
     width = 1200, height = 800, quality = 100, res = 120)
plot(spi_era5_cropped[[20]], main = "SPI-12 ERA5 (Time Step 20) - Poles Removed")
dev.off()


#### PCA AND VARIMAX ROTATION ANALYSIS ####
spi_data_cropped <- as.array(spi_era5_cropped)
dim(spi_data_cropped)

n_lon <- dim(spi_data_cropped)[1]
n_lat <- dim(spi_data_cropped)[2]
n_time <- dim(spi_data_cropped)[3]

spi_matrix <- matrix(spi_data_cropped, nrow = n_lon * n_lat, ncol = n_time)

spi_matrix_trimmed <- spi_matrix[, 12:ncol(spi_matrix)]

complete_rows <- complete.cases(spi_matrix_trimmed)
spi_matrix_clean <- spi_matrix_trimmed[complete_rows, ]
row_indices <- which(complete_rows)

rm(spi_data_cropped, spi_matrix, spi_matrix_trimmed)
gc()

spi_data <- t(spi_matrix_clean)

spi_data_centered <- scale(spi_data, center = TRUE, scale = FALSE)

cat("Running PCA...\n")
pca_res <- prcomp(spi_data_centered, center = FALSE, scale = FALSE)

max_comps <- min(30, ncol(pca_res$rotation))
pca_loadings <- pca_res$rotation[, 1:max_comps, drop = FALSE]
pca_scores   <- pca_res$x[, 1:max_comps, drop = FALSE]

rm(spi_data_centered, pca_res)
gc()

cat("Applying varimax rotation...\n")
varimax_res <- varimax(pca_loadings)
rotated_loadings <- varimax_res$loadings
rotated_scores   <- pca_scores %*% varimax_res$rotmat

rotated_variance <- apply(rotated_scores, 2, var)
total_variance <- sum(rotated_variance)
rotated_explained <- rotated_variance / total_variance

coords <- xyFromCell(spi_era5_cropped, row_indices)
max_idx <- apply(abs(rotated_loadings), 2, which.max)
max_coords <- coords[max_idx, ]

print("Explained variance of rotated components:")
print(rotated_explained)
print("Coordinates of maximum absolute loadings for each component:")
print(max_coords)

n_components <- 30

subset_rotated_scores <- rotated_scores[, 1:n_components, drop = FALSE]

subset_data <- as.data.frame(subset_rotated_scores)
subset_data$time <- seq_len(nrow(subset_data))

write.csv(subset_data, "./output/ERA5_subset_rotated_scores.csv", row.names = FALSE)

##################### VISUALIZATIONS #####################

####### 1. BARPLOT OF EXPLAINED VARIANCE ######
pca_sdev <- pca_loadings %>% 
  apply(2, function(x) sqrt(sum(x^2)))

pca_variance <- (pca_sdev^2) / sum(pca_sdev^2) * 100
pca_cumulative <- cumsum(pca_variance)

pca_data <- data.frame(
  Component = 1:length(pca_variance),
  Variance = pca_variance,
  Cumulative = pca_cumulative
)

varimax_variance <- rotated_explained * 100
varimax_cumulative <- cumsum(rotated_explained) * 100

varimax_data <- data.frame(
  Component = 1:length(varimax_variance),
  Variance = varimax_variance,
  Cumulative = varimax_cumulative
)

key_components <- c(2, 3, 5, 12, 14)
varimax_data$Highlight <- varimax_data$Component %in% key_components

varimax_plot_adjusted <- ggplot(varimax_data) +
  geom_col(aes(x = Component, y = Variance, fill = Highlight), 
           color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c("FALSE" = "#94c4df", "TRUE" = "#4393c3")) +
  scale_y_continuous(
    name = "Individual Variance (%)",
    limits = c(0, 6),
    breaks = seq(0, 6, 1),
  ) +
  labs(
    title = "Varimax Rotation: Redistributed Variance",
    subtitle = "Variance is redistributed to create more interpretable climate patterns",
    x = "Rotated Component"
  ) +
  geom_text(data = varimax_data %>% filter(Highlight),
            aes(x = Component, y = Variance + 0.3, 
                label = paste0("C", Component)),
            size = 3.5, fontface = "bold") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y.left = element_text(color = "black"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    panel.grid.major.y = element_line(color = "gray90", size = 0.2)
  )

windows()
plot(varimax_plot_adjusted)

ggsave("./output/varimax_rotation_variance_adjusted_scale.jpeg", 
       varimax_plot_adjusted, width = 10, height = 6, dpi = 300)


#################### 
dominant_component <- rep(NA, n_lon * n_lat)

for (i in seq_along(row_indices)) {
  cell_idx <- row_indices[i]
  loadings_for_cell <- abs(rotated_loadings[i, 1:n_components])
  dominant_component[cell_idx] <- which.max(loadings_for_cell)
}

rast_dims <- dim(spi_era5_cropped)
n_rows <- rast_dims[1]
n_cols <- rast_dims[2]

dominant_component_grid <- matrix(dominant_component, nrow=n_rows, ncol=n_cols)

cat("Dimensions of spi_era5_cropped:", n_rows, "rows ×", n_cols, "columns\n")
cat("Dimensions of dominant_component_grid:", nrow(dominant_component_grid), 
    "rows ×", ncol(dominant_component_grid), "columns\n")

assign_raster <- spi_era5_cropped[[1]]

values(assign_raster) <- dominant_component_grid

jpeg(filename = "./output/dominant_components_map.jpg", 
     width = 1200, height = 800, quality = 100, res = 120)
par(mar = c(4, 4, 4, 8))
plot(assign_raster, 
     main = "Dominant Climate Component by Region",
     col = viridis(n_components),
     legend = TRUE)

maps::map("world", add = TRUE, col = "gray40", lwd = 0.3)
dev.off()
#########################

####### 3. MAP OF KEY COMPONENTS ######
key_components <- c(2, 3, 5, 12, 14)
highlight_raster <- assign_raster

values(highlight_raster)[!values(highlight_raster) %in% key_components] <- NA

component_colors <- viridis(length(key_components))
names(component_colors) <- as.character(key_components)

windows()
jpeg(filename = "./output/key_climate_components.jpg", 
     width = 1200, height = 800, quality = 100, res = 120)
par(mar = c(4, 4, 4, 8))
plot(highlight_raster, 
     main = "Key Climate Components (1, 2, 3, 7, 13, 21)",
     col = component_colors,
     legend = FALSE)

maps::map("world", add = TRUE, col = "gray40", lwd = 0.3)

legend("right", 
       legend = paste("Component", key_components),
       fill = component_colors,
       title = "Major Climate\nPatterns",
       bty = "n",
       inset = c(-0.2, 0),
       xpd = TRUE)
dev.off()

####### 4. INDIVIDUAL COMPONENT MAPS ######
jpeg(filename = "./output/component_maps.jpg", 
     width = 1800, height = 1200, quality = 100, res = 120)
par(mfrow = c(3, 2), mar = c(2, 2, 3, 1))

for (comp in key_components) {
  comp_raster <- assign_raster
  values(comp_raster)[values(comp_raster) != comp] <- NA
  
  plot(comp_raster, 
       main = paste("Component", comp),
       col = viridis(1, option = "plasma"),
       legend = FALSE)
  
  maps::map("world", add = TRUE, col = "gray40", lwd = 0.3)
}
dev.off()

selected_components <- c(2, 3, 5, 12, 14)
selected_rotated_scores <- rotated_scores[, selected_components, drop = FALSE]

selected_data <- as.data.frame(selected_rotated_scores)
selected_data$time <- seq_len(nrow(selected_data))

write.csv(selected_data, "./output/ERA5_selected_components.csv", row.names = FALSE)