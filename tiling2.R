# Tiling Raster Input 

library(raster)
library(RStoolbox)
library(jpeg)
library(ggplot2)
##########################################################
# functions for splitting

getwd()
# setwd() # maybe you need to set your working directory, 


input <- stack("data/input_data.tif") # this file needs to be downloaded and stored accordingly
head(input)

OSM_mask <- raster("data/OSM/Small_ROI/OSM_wetlands_rasterized.tif") #  this file needs to be downloaded and stored accordingly


# remove na values and rescale to be able to plot RGB image
input[is.na(input[])] <- 0 
input <- rescaleImage(input, ymin=0, ymax=255)

OSM_mask[is.na(OSM_mask[])] <- 0 


# select the bands needed and rescale for RGB plot
min <- min(values(srtm_elev[[3]]))
max <- max(values(srtm_elev[[3]]))
input_data[[3]] <- rescaleImage(srtm_elev[[3]], xmin = min, xmax = max, ymin=0, ymax=255)
head(srtm_elev)


# image <- plotRGB(input_data, r=1, g=2, b=3)


# save RGB image as jpeg
jpeg(file = "data/input_data.jpg")
plotRGB(input_data, r=1, g=2, b=3)
dev.off()


# save OSM mask image as jpeg
jpeg(file = "data/input_mask.jpg")
plot(OSM_mask, bty="n", legend = FALSE, box = FALSE, axes = FALSE, col = grey(1:100/100))
dev.off()



# writeJPEG(merged_red_nir_srtm, "input_data.jpeg")
# writeRaster(merged_red_nir_srtm,"./Detecting_Moorland/input_data.tif" )

#########################################################
#########################################################
# function for tiling

split_raster <- function(r, nx, ny, buffer = c(0,0)) {
  ext <- extent(r)
  tiles <- vector("list", length = nx * ny)
  n <- 1L
  for (i in seq_len(nx) - 1L) {
    for (j in seq_len(ny) - 1L) {
      x0 <- ext@xmin + i * ((ext@xmax - ext@xmin) / nx) - buffer[1] * xres(r) 
      x1 <- ext@xmin + (i + 1L) * ((ext@xmax - ext@xmin) / nx) + buffer[1] * xres(r) # nolint
      y0 <- ext@ymin + j * ((ext@ymax - ext@ymin) / ny) - buffer[2] * yres(r) # nolint
      y1 <- ext@ymin + (j + 1L) * ((ext@ymax - ext@ymin) / ny) + buffer[2] * yres(r) # nolint
      tiles[[n]] <- extent(x0, x1, y0, y1)
      n <- n + 1L
    }
  }
  crop_tiles <- function(i, e, r) {
    ri <- crop(r, e[[i]])
    crs(ri) <- crs(r)
    return(ri)
  }
  tiles <- lapply(seq_along(tiles), function(i) crop_tiles(i, tiles, r)) 
  return(tiles)
}


##########################################################
##########################################################
# 50x50 Tiles
# 2500 tiles 
tiles <- split_raster(input_data, 50, 50)


for (i in 1:length(tiles)) {
  print(i)
  jpeg(file = paste0("tiles/tile_50_", i, ".jpg"))
  plotRGB(tiles[[i]], r=1, g=2, b=3)
  dev.off()
  #writeRaster(tiles[[i]],paste0("./Detecting_Moorland/tiles/tile_50_", i, ".tif" ))   #muss das nicht 50 sein?? 
}


mask <- split_raster(OSM_mask, 50, 50)

for (i in 1:length(mask)) {
  print(i)
  jpeg(file = paste0("mask/mask_50_", i, ".jpg"))
  plot(mask[[i]], bty="n", legend = FALSE, box = FALSE, axes = FALSE, col = grey(1:100/100))
  dev.off()
  #writeRaster(mask[[i]],paste0("./Detecting_Moorland/mask/mask_50_", i, ".tif" ), overwrite = TRUE)
}

