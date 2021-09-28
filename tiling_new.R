#########################################################
# Create Raster Stack and tiling images
#########################################################

#Load Libraries and set working directory
library(raster)
library(RStoolbox)
library(jpeg)

#setwd("enter your working direction here/Detecting_moorland")
setwd("C:/Users/annik/Desktop/Detecting_moorland/")

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
#########################################################
# Preprocess Input Data 

#Load Sentinel 2 Scene and SRTM
s2 <- stack("./Input/S2_median_30m.tif")
srtm <- raster("./Input/srtm.tif")
osm_mask <- raster("./Input/OSM_mask.tif")


# Remove NA Values
s2[is.na(s2[])] <- 0 

# Rescale Input Data (Range 0-255)
re <- rescaleImage(s2[[4]], ymin=0, ymax=255)
nir <- rescaleImage(s2[[7]], ymin=0, ymax=255)
srtm <- rescaleImage(srtm, ymin=0, ymax=255)

#stack images
merged_re_nir_srtm <- stack(re, nir, srtm_elev)

#write raster with stacked input images
writeRaster(merged_re_nir_srtm, "Input/Input_RE_NIR_SRTM.tif", overwrite = TRUE)


#tiling

img <- split_raster(merged_re_nir_srtm, 50, 50)
mask <- split_raster(osm_mask, 50, 50)

#########################################################

tiles <- lapply(seq_along(tiles), function(i) crop_tiles(i, tiles, r)) 



for (i in 1:length(mask)) {
   if (1 %in% unique(mask[[i]])){
      
      writeJPEG(as.array(mask[[i]]), paste0("./mask/", i, "_x_mask.jpg"))
      writeJPEG(as.array(img[[i]]/255), paste0("./img/", i, "_x_img.jpg"))
      
      } else{
         writeJPEG(as.array(mask[[i]]), paste0("./mask/", i, "_mask.jpg"))
         writeJPEG(as.array(img[[i]]/255), paste0("./img/", i, "_img.jpg"))
         
      }}

#########################################################

