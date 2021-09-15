# Tiling Raster Input 

library(raster)
library(RStoolbox)
library(jpeg)
library(ggplot2)
##########################################################
# functions for splitting

getwd()


s2 <- stack("data/S2_median_30m_small_roi.tif")
head(s2)
s2 <- s2[[1:7]]
extent(s2)
srtm <- stack("data/srtm.tif")
extent(srtm)
OSM_mask <- raster("data/OSM/Small_ROI/OSM_wetlands_smallROI_rasterized.tif")
# OSM_mask <- stack("./Detecting_Moorland/OSM_wetlands_smallROI_rasterized.tif")

# remove na values and rescale to be able to plot RGB image
s2[is.na(s2[])] <- 0 
s2 <- rescaleImage(s2, ymin=0, ymax=255)

OSM_mask[is.na(OSM_mask[])] <- 0 


# select the bands needed and rescale for RGB plot
red <- s2[[3]]
nir <- s2[[7]]
srtm_elev <- srtm[[1]]
min <- min(values(srtm_elev))
max <- max(values(srtm_elev))
srtm_elev <- rescaleImage(srtm_elev, xmin = min, xmax = max, ymin=0, ymax=255)
head(srtm_elev)



# to be able to stack optical bands and srtm, the extent and pixels
extent(srtm_elev) <- extent(red)
extent(red)
extent(srtm_elev)
extent(OSM_mask)
srtm_elev <- resample(srtm_elev, red, 'ngb')

merged_red_nir_srtm <- stack(red, nir, srtm_elev)

# image <- plotRGB(merged_red_nir_srtm, r=1, g=2, b=3)


# save RGB image as jpeg
jpeg(file = "data/input_data.jpg")
plotRGB(merged_red_nir_srtm, r=1, g=2, b=3)
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
#Version 1 - 50x50 Tiles
#2500 tiles 
tiles <- split_raster(merged_red_nir_srtm, 50, 50)


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



#Version 2 - 100x100 Tiles

#tiles <- split_raster(merged, 100, 100)

#for (i in 1:length(tiles)) {
#  print(i)
#  writeRaster(tiles[[i]],"./output/tile_100_", i, ".tif" )
#}