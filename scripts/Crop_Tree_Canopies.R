#This script crops and masks an image by a shapefile. The output is a raster of each tree canopy for that image.

source("Functions/lecospectR.R")
library(terra)
library(raster)
library(rgdal)
library(hsdar)
library(rgeos)
library(sf)

#Define file paths
SR1_0_image_path = "G:/Sullivan/100547_sullivan_2022_07_20_18_26_38/raw_0_rd_rf_or"
SR1_1428_image_path = "G:/Sullivan/100547_sullivan_2022_07_20_18_26_38/raw_1428_rd_rf_or"
CL2_1426_image_path= "G:/CL22022/100539_cl2_2022_2022_07_20_16_30_54/raw_1426_rd_rf_or"
CL2_3615_image_path = "G:/CL22022/100539_cl2_2022_2022_07_20_16_30_54/raw_3426_rd_rf_or"
HB1_9252_image_path = "E:/Hewes_Brook_1/Imaging_Spectrometer/100464_hb1_2022_flight1_2022_06_28_15_53_13/raw_9253_rd_rf_or"
HB1_9918_image_path = "E:/Hewes_Brook_1/Imaging_Spectrometer/100464_hb1_2022_flight1_2022_06_28_15_53_13/raw_9918_rd_rf_or"

canopies_path = "raw_data/geometry/hand_drawn/"
list.files(canopies_path)
#bring in test image
img <- brick(image_path)

#bring in vector of canopies
#canopies_vec <- readOGR(dsn = paste0(canopies_path, "100038_PEF_SM_4_110m_2019_06_18_15_16_13_ortho_raw_7492_rd_rf_or.shp"))
canopies_full <- readOGR(dsn = paste0(canopies_path, "full_Howland_100006_2000.shp"))
canopies_light <- readOGR(dsn = paste0(canopies_path, "light_Howland_100006_2000.shp"))
canopies_shadow <- readOGR(dsn = paste0(canopies_path, "shadow_Howland_100006_2000.shp"))

#Now, crop and mask image by each canopy shapefile. Functions are split into
  #full canopies, illuminated only, and shadowed only
#FULL CANOPIES#
lapply(1:length(canopies_full),  
       function(x) {
         tst_img <- brick(image_path)
         tst_names<-names(tst_img)
         tst_quads<-canopies_full[x,]
         tst_crop <- raster::crop(tst_img, tst_quads)
         tst_mask <- raster::mask(tst_crop, tst_quads)
         metadata(tst_mask)<-as.list(canopies_full[x,]$CLASS_NAME)
         bandnames(tst_mask)<-tst_names
         writeRaster(tst_mask, paste("M:\\MSGC_DATA\\Howland\\Cropped_ROIs_spectra\\", canopies_full[x,]$CLASS_NAME, sep=""), format = "raster", overwrite = TRUE)
       })