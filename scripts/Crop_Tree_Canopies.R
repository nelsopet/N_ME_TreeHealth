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
HB3_5624_image_path = "E:/Hewes_Brook_1/Imaging_Spectrometer/100471_hewes_brook_3_2022_flight2_2022_06_28_18_20_28/raw_5624_rd_rf_or"
HB3_7624_image_path = "E:/Hewes_Brook_1/Imaging_Spectrometer/100471_hewes_brook_3_2022_flight2_2022_06_28_18_20_28/raw_7624_rd_rf_or"
RR1_2250_image_path = "E:/Red_River_1/Imaging_Spectrometer/100479_rr1_2022_flight2_2022_06_28_19_58_04/raw_2250_rd_rf_or"
RR1_4267_image_path = "E:/Red_River_1/Imaging_Spectrometer/100479_rr1_2022_flight2_2022_06_28_19_58_04/raw_4267_rd_rf_or"

spruce_imgs<-c(
CL2_1426_image_path
,CL2_3615_image_path
,HB1_9252_image_path
,HB1_9918_image_path
,HB3_5624_image_path
,HB3_7624_image_path
,RR1_2250_image_path
,RR1_4267_image_path
,SR1_0_image_path
,SR1_1428_image_path)

canopies_path = "raw_data/geometry/hand_drawn/"
canopies_sites<-list.files(canopies_path)[grepl("*.shp$",list.files(canopies_path))]
#canopies<-terra::vect(paste(canopies_sites[1],canopies_sites[1])
canopies<-lapply(1:length(canopies_sites), function(x) {terra::vect(paste(canopies_path, canopies_sites[x], sep=""))})
#canopies<-Reduce(c,canopies)
#Now, crop and mask image by each canopy shapefile. Functions are split into
  #full canopies, illuminated only, and shadowed only
#FULL CANOPIES#
lapply(1:length(spruce_imgs),  
       function(x) {
         tst_img <- terra::rast(spruce_imgs[x])
         tst_names<-names(tst_img)
         tst_quads<-canopies[[x]]
         tst_crop <- terra::crop(tst_img, tst_quads)
         tst_mask <- terra::mask(tst_crop, tst_quads)
         #metadata(tst_mask)<-tst_quads$CLASS_NAME
         bandnames(tst_mask)<-tst_names
            lapply(1:length(tst_quads), function (x) {writeRaster(tst_mask, paste("output/canopy_spectra/", tst_quads[x]$CLASS_NAME, ".ENVI",sep=""), overwrite = TRUE)})
       })
