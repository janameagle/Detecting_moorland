# Tiling Raster Input 

library(raster)
library(RStoolbox)
library(jpeg)
library(ggplot2)
library(SpaDES)


getwd()
setwd("enter your working direction here/Detecting_moorland")


#########################################################
# preprocessing and stacking the input data
s2 <- stack("Input/S2_median_30m.tif")

srtm <- raster("Input/srtm.tif")


# remove na values
s2[is.na(s2[])] <- 0 
s2 <- s2[1:7]
s2 <- rescaleImage(s2, ymin=0, ymax=255)


# select the bands needed and rescale for RGB plot
re <- s2[[4]]
nir <- s2[[7]]
min <- min(values(srtm))
max <- max(values(srtm))
srtm_elev <- rescaleImage(srtm, xmin = min, xmax = max, ymin=0, ymax=255)


merged_re_nir_srtm <- stack(re, nir, srtm_elev)

writeRaster(merged_re_nir_srtm, "Input/Input_RE_NIR_SRTM.tif", overwrite = TRUE)



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

#########################################################

#Load Input Data
input_data <- stack("./Input/Input_RE_NIR_SRTM.tif")
OSM_mask <- raster("./Input/OSM_mask.tif")

#tiling

img <- split_raster(input_data, 50, 50)
mask <- split_raster(OSM_mask, 50, 50)

#########################################################

for (i in 1:length(mask)) {
   if (1 %in% unique(mask[[i]])){
      
      writeJPEG(as.array(mask[[i]]), paste0("./temp/", i, "_x_mask.jpg"))
      writeJPEG(as.array(img[[i]]), paste0("./temp/", i, "_x_img.jpg"))
      
      } else{
         writeJPEG(as.array(mask[[i]]), paste0("./temp/", i, "_mask.jpg"))
         writeJPEG(as.array(img[[i]]), paste0("./temp/", i, "_img.jpg"))
         
}}

