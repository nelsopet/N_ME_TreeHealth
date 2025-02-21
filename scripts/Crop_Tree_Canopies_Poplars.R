#This script crops and masks an image by a shapefile. The output is a raster of each tree canopy for that image.

source("Functions/lecospectR.R")
library(terra)
library(raster)
library(rgdal)
library(hsdar)
library(rgeos)
library(sf)

#Define file paths
image_path_01 = "E:/Cross_Lake_1/100411_Cross_Lake_1_2021_06_16_17_42_33/raw_4431_rd_rf_or"
image_path_02 = "E:/Cross_Lake_1/100411_Cross_Lake_1_2021_06_16_17_42_33/raw_7106_rd_rf_or"
image_path_03 = "E:/Cross_Lake_2/100429_Cross_Lake_2_2021_06_16_19_41_13/raw_10176_rd_rf_or"
image_path_04 = "E:/Cross_Lake_2/100429_Cross_Lake_2_2021_06_16_19_41_13/raw_8176_rd_rf_or"
image_path_05 = "E:/Hewes_Brook_1/Imaging_Spectrometer/100445_hewes_brook_1_2021_07_24_15_29_35/raw_7988_rd_rf_or"
image_path_06 = "E:/Hewes_Brook_2/100450_Hewes_Broiok_2_2021_07_24_19_14_29/raw_0_rd_rf_or"
image_path_07 = "E:/Hewes_Brook_2/100450_Hewes_Broiok_2_2021_07_24_19_14_29/raw_1336_rd_rf_or"
image_path_08 = "E:/Hewes_Brook_2/100450_Hewes_Broiok_2_2021_07_24_19_14_29/raw_4956_rd_rf_or"
image_path_09 = "E:/Hewes_Brook_3/Imaging_Spectrometer/100471_hewes_brook_3_2022_flight2_2022_06_28_18_20_28/raw_5624_rd_rf_or"
image_path_10 = "E:/Red_River_1/Imaging_Spectrometer/100449_Red_River_1_v2_2021_07_24_17_46_16/raw_6569_rd_rf_or"

imgs<-c(
image_path_01,
image_path_02,
image_path_03,
image_path_04,
image_path_05,
image_path_06,
image_path_07,
image_path_08,
image_path_09,
image_path_10)

canopies_path = "raw_data/geometry/hand_drawn/Poplars/"
canopies_sites<-list.files(canopies_path)[grepl("*.shp$",list.files(canopies_path))]
canopies<-lapply(1:length(canopies_sites), function(x) {terra::vect(paste(canopies_path, canopies_sites[x], sep=""))})

X11()
plot(canopies[[1]])
terra::rast(imgs[1]) %>% plot


lapply(1:length(canopies), function(x) {ROI_name<-canopies[[x]]$CLASS_NAME;  return(ROI_name)})
#, add=TRUE)

#FULL CANOPIES#
lapply(1:length(imgs),  
       function(x) {
         tst_img <- terra::rast(imgs[x])
         tst_names<-names(tst_img)
         tst_quads<-canopies[[x]]
         #metadata(tst_mask)<-tst_quads$CLASS_NAME
            lapply(1:length(tst_quads), function (x) {
                tst_crop <- terra::crop(tst_img, tst_quads[x])
                tst_mask <- terra::mask(tst_crop, tst_quads[x])
                bandnames(tst_mask)<-tst_names
                writeRaster(tst_mask, paste("output/canopy_spectra/Poplars/", tst_quads[x]$CLASS_NAME, ".ENVI",sep=""), overwrite = TRUE)
                rm(tst_crop)
                rm(tst_mask)})
       rm(tst_img)
       rm(tst_quads)
       gc()
       })

tst_canopy_files<-list.files("output/canopy_spectra/Poplars/")
tst_canopy<-terra::rast(paste("output/canopy_spectra/Poplars/", tst_canopy_files[1], sep=""))