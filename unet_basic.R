library(keras)
library(tensorflow)
library(tfdatasets)
library(purrr)
library(ggplot2)
library(rsample)
library(stars)
library(raster)
library(reticulate)
library(mapview)
library(magick)

getwd()

# get the input shape
library(jpeg)
image = readJPEG("C:/Users/annik/Desktop/Detecting_moorland/img/6_x_img.jpg")
dim(image)




setwd("C:/Users/annik/Desktop/Detecting_moorland/")

# index data
data <- rbind(
  data.frame(
    img = list.files("img/", pattern="*_x_*", full.names = T),
    mask = list.files("mask/", pattern="*_x_*", full.names = T)
  )
)

#data <- data[1:500, ]
#class(data)

# split of data into training and testing
data <- initial_split(data, prop = 0.75)

# defining shape and batch size
input_shape <- c(448,448,3) # our tiles are 480, 480, 3
batch_size <- 10

# call training and testing data
training(data)
testing(data)


################################################################################
### training data
# create tensor slices from our training data
train_ds <- tfdatasets::tensor_slices_dataset(training(data))

# load images and masks values into the tensor slices (i.e. fill the tensors)
train_ds <- dataset_map(
  train_ds, function(x) 
    list_modify(x, 
                img = tf$image$decode_jpeg(tf$io$read_file(x$img)),
                mask = tf$image$decode_jpeg(tf$io$read_file(x$mask)))
) 

# make sure tensors have the correct datatype
train_ds <- dataset_map(
  train_ds, function(x) 
    list_modify(x, 
                img = tf$image$convert_image_dtype(x$img, dtype = tf$float32),
                mask = tf$image$convert_image_dtype(x$mask, dtype = tf$float32))
) 

# resize images in case they dont fit out input shape
train_ds <- dataset_map(
  train_ds, function(x) 
    list_modify(x,
                img = tf$image$resize(x$img, size = shape(input_shape[1], input_shape[2])),
                mask = tf$image$resize(x$mask, size = shape(input_shape[1], input_shape[2]))
    )
)




################################################################################
# data augmentation performed on training set only
if (train) {
  dataset <- dataset %>% 
    dataset_map(~.x %>% list_modify(
      img = random_bsh(.x$img)
    )) 
}

# shuffling on training set only
if (train) {
  dataset <- dataset %>% 
    dataset_shuffle(buffer_size = batch_size*128)
}

# train in batches; batch size might need to be adapted depending on
# available memory
dataset <- dataset %>% 
  dataset_batch(batch_size)

dataset %>% 
  # output needs to be unnamed
  dataset_map(unname) 
################################################################################

################################################################################



train_ds <- dataset_batch(train_ds, batch_size)
train_ds <- dataset_map(train_ds, unname)


################################################################################
### validation data
# preprocess validation as above, no augmentation
val_ds <- tensor_slices_dataset(testing(data))
val_ds <- dataset_map(
  val_ds, function(x) 
    list_modify(x, 
                img = tf$image$decode_jpeg(tf$io$read_file(x$img)),
                mask = tf$image$decode_jpeg(tf$io$read_file(x$mask)))
) 

val_ds <- dataset_map(
  val_ds, function(x) 
    list_modify(x, 
                img = tf$image$convert_image_dtype(x$img, dtype = tf$float32),
                mask = tf$image$convert_image_dtype(x$mask, dtype = tf$float32))
) 

val_ds <- dataset_map(
  val_ds, function(x) 
    list_modify(x,
                img = tf$image$resize(x$img, size = shape(input_shape[1], input_shape[2])),
                mask = tf$image$resize(x$mask, size = shape(input_shape[1], input_shape[2]))
    )
)
val_ds <- dataset_batch(val_ds, batch_size)
val_ds <-  dataset_map(val_ds, unname) 

# check dimensions
slices2list <- function(x){
  return(iterate(as_iterator(x)))
}
train_ds_list <- slices2list(train_ds)
length(train_ds_list) * batch_size # amount of samples
dim(train_ds_list[[1]][[1]])


################################################################################

## NETWORK DESIGN: UNET

################################################################################


#l2 <- 0.03 #0.02 0.01
## load pretrained vgg16 and use part of it as contracting path (feature extraction) ##
vgg16_feat_extr <- application_vgg16(weights = "imagenet", include_top = FALSE, input_shape = input_shape)

# optionally freeze first layers to prevent changing of their weights, either whole convbase or only certain layers
# freeze_weights(vgg16_feat_extr) #or:
# freeze_weights(vgg16_feat_extr, to = "block1_pool")

# we'll not use the whole model but only up to layer 15
unet_tensor <- vgg16_feat_extr$layers[[15]]$output


## add the second part of 'U' for segemntation ##

# "bottom curve" of U-net
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1024, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1024, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 1
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 512, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[14]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 512, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 512, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 2
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 256, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[10]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 3
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 128, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[6]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 4
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 64, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[3]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = 3, padding = "same", activation = "relu")

# final output
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1, kernel_size = 1, activation = "sigmoid")

# create model from tensors
unet_model <- keras_model(inputs = vgg16_feat_extr$input, outputs = unet_tensor)
unet_model


# compile the model
compile(
  unet_model,
  optimizer = optimizer_rmsprop(learning_rate = 1e-5),
  loss = "binary_crossentropy",
  metrics = c(metric_binary_accuracy)
)


# train it
run <- fit(
  unet_model,
  train_ds,
  epochs = 10, # !!change to 10
  validation_data = val_ds
)

keras::save_model_hdf5(unet_model, filepath = "moorland_imagenet_model.h5")

# load model:
#unet_model <- load_model_hdf5(filepath = "moorland_imagenet_model.h5")

# evaluation: accuracy and loss
evaluate(unet_model, val_ds)

#  tbc with extracting prediction maps
pred_ds <- predict(unet_model, val_ds)

pred_df <- data.frame(
  y = testing(data)$label,
  yhat = round(pred_ds)
)

table(pred_df) # here you see matching, false positive and false negatives



dim(pred_ds)
# pred_ds[1,,,]

i_image = 50
pred <- predict(object = unet_model,val_ds)[i_image,,,]
plot(
  image_append(c(
    image_append(image_read(testing(data)[i_image, 2]), stack = TRUE),
    image_append(image_read(testing(data)[i_image, 1]), stack = TRUE), 
    image_append(image_read(as.raster(pred)), stack = TRUE)
  )))



############################
#### from other script
pred <- data.frame(
  y = testing(data)$label,
  yhat = round(predict(unet_model, val_ds))
)

table(pred) # here you see matching, false positive and false negatives




dim(pred_ds)
# pred_ds[1,,,]

i_image = 10
pred <- predict(object = unet_model,val_ds)[i_image,,,]
plot(
  image_append(c(
    image_append(image_read(testing(data)[i_image, 2]), stack = TRUE),
    image_append(image_read(testing(data)[i_image, 1]), stack = TRUE), 
    image_append(image_read(as.raster(pred)), stack = TRUE)
  )))



############################
#### from other script
pred <- data.frame(
  y = testing(data)$label,
  yhat = round(predict(unet_model, val_ds))
)

table(pred) # here you see matching, false positive and false negatives

