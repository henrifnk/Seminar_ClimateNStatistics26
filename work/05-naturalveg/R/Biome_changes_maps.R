library(terra)
library(ggplot2)
library(gridExtra)
library(grid)

# Load the hard classification rasters
hard_pres<- rast("work/05-naturalveg/results/hard_class_present.tif")
hard_26 <- rast("work/05-naturalveg/results/hard_class_26.tif")
hard_45 <- rast("work/05-naturalveg/results/hard_class_45.tif")
hard_85 <- rast("work/05-naturalveg/results/hard_class_85.tif")

#retrive biome code
levels(hard_pres)

extent<- ext(-15, 14, 34, 45)
"#D73027", "#FDAE61", "#FEE08B"

# function to plot biome changes for any biome at any extent

  plot_biome_changes_by_code <- function(  biome_code, biome_label, extent=NULL,
    change_colors=c("#E74C3C", "#8B4513", "#F39C12")) {
    
    if (!is.null(extent)) {
      hard_pres <- crop(hard_pres, extent)
      hard_26 <- crop(hard_26, extent)
      hard_45 <- crop(hard_45, extent)
      hard_85 <- crop(hard_85, extent)
    }
    
    land_mask <- hard_pres
    land_mask[!is.na(land_mask)] <- 1
    land_outline <- as.polygons(land_mask)
    
    
    # Extract biome for each scenario using the code
    biome_present <- extract_biome(hard_pres, biome_code)
    biome_26 <- extract_biome(hard_26, biome_code)
    biome_45 <- extract_biome(hard_45, biome_code)
    biome_85 <- extract_biome(hard_85, biome_code)
    
    # Create change rasters
    biome_change_26 <- create_change_raster(biome_present, biome_26)
    biome_change_45 <- create_change_raster(biome_present, biome_45)
    biome_change_85 <- create_change_raster(biome_present, biome_85)
    
    windows()
    # Layout for change maps and legend
    layout_matrix <- matrix(c(1, 2, 3, 4), nrow = 1, ncol = 4)
    layout(layout_matrix, widths = c(4, 4, 4, 2.5))
    
    
    par(mar = c(1, 0, 2, 0))
    plot(biome_change_26, main = paste(biome_label, "Change - RCP 2.6"), 
         col = change_colors, legend = FALSE, maxcell = Inf)
    plot(land_outline, add = TRUE, border = "black", lwd = 0.5)
    
    plot(biome_change_45, main = paste(biome_label, "Change - RCP 4.5"), 
         col = change_colors, legend = FALSE, maxcell = Inf)
    plot(land_outline, add = TRUE, border = "black", lwd = 0.5)
    
    plot(biome_change_85, main = paste(biome_label, "Change - RCP 8.5"), 
         col = change_colors, legend = FALSE, maxcell = Inf)
    plot(land_outline, add = TRUE, border = "black", lwd = 0.5)
    plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Legend")
    legend("center", 
           legend = c(paste("Lost", biome_label), paste("Stable", biome_label), paste("New", biome_label)), 
           fill = change_colors, 
           bty = "n", 
           cex = 1.2)
  }
  
  iberia <-ext(-12, 13,34, 47)
   col <- c("#A6CEE3", "#D8B365",  "#F46D43" )
    colo<-c("#FEE08B", "#FDAE61","#D73027" )
    
  plot_biome_changes_by_code(8, "Desert", extent=iberia)  
  colo<-c("#F39C12", "#8B4513", "#E74C3C")
  
  warm.temperate.evergreen.broadleaf.and.mixed.forest
  plot_biome_changes_by_code(13, "Warm Temperate Evergreen Broadleaf and Mixed Forest")
  
  