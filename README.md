# Detecting_moorland

Moors (also moorland / bog / swamp / mire) provide unique habitats for many plants and animals.
They alleviate floods and filter polluted water.
Furthermore they play a key role in the carbon cycle and thus in climate change (Guo et al, 2017).

Therefore it is important to have detailed and extensive data and knowledge about them and their distribution.

In this project we are training a machine learning model, more precisely a U-net model based on pre-trained VGG16, that will later give a probability mask of the distribution of moors. 



Input data: 3-band image of the wetland region in southern Germany 
              Bands: Sentinel-2 red, Sentinel-2 nir, SRTM 
            Binary mask of moor areas retreived from OSM data, same extent in southern Germany 
            Both files, as well as the non-stacked S2 and SRTM images, can be found here: https://drive.google.com/drive/folders/1fl3z8d9qsrOaf3YaX-IDbq9M1FQYAMZA?usp=sharing

Output: trained U-net model, which can be found here: 

Authors: Jana Maier, Nora Nieskens, Annika Ludwig 

Context: This project serves as final submission for the Advanced Programming Course 2021 at Julius-Maximilians-University Würzburg. 



################################################################################



First, a 3-band image consisting of Sentinel-2 red, Sentinel-2 nir, SRTM was created using Google Earth Engine and the tiling.R script.
Sentinel-2 images were used to create a cloud-free and cloud-shadow-free median composite of the summer period of 2020. Inside the tiling script the according bands are selected, resampled to fit each other and rescaled for creating RGB images.
The Google Earth Engine script can be found here: https://code.earthengine.google.com/090d981bb93db4d29db21f7166ac1453


The OSM mask was created using QGIS and OSM data with the label 'wetland', as we are working with data from southern Germany and all wetlands belong to moors.


For the input data the two Raster image (3-band image and OSM mask) are tiled into about 2500 individual tiles each, which can be found in the 'img' and 'mask' folder.
For details see tiling.R. Additionally the tiles are marked with an "_ x" to determine if they include a wetland. Only these tiles are then used for training of the model, to avoid biased input data by too many tile with no wetland at all.


For the model, the pre-trained VGG16 model is used and expanded manually to a U-net. 
Through adjustment of the model parameters learning rate, number of epochs and batch size, the resulting accuracy can be enhanced.
For details see unet.R




An overview about the workflow:


![Workflow_dl_moor](https://user-images.githubusercontent.com/57681769/134295716-9e25de9c-b680-44fd-b226-42c2b9cf9611.png)



