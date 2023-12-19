#Load required packages
require(tidyverse)
require(measurements)
require(leaflet)
require(rgdal)
require(terra)
setwd("N_ME_TreeHealth")

#Load data from K.R.N.
#NOTE: These data were hand edited to correct two rows (one lat and one long) that had incorrect values.
#tree_locs<-read.csv("raw_data/UMFK_MEIFSCI_tree_locations_20220614.csv", header=TRUE)
#tree_locs_2022<-read.csv("raw_data/UMFK_MEIF_SPRUCE_2022_Active.csv", header=TRUE, encoding ='UTF-8')
#tree_locs_2023<-read.csv("raw_data/UMFK_MEIF_2023_Active_09062023.csv", header=TRUE, encoding ='UTF-8')

raw_files<-list.files("raw_data")
raw_files
Picea_2022<-read.csv(paste("raw_data/",raw_files[2],sep=""), header=TRUE, encoding ='UTF-8')
Populus_2021<-read.csv(paste("raw_data/",raw_files[1],sep=""), header=TRUE, encoding ='UTF-8')

colnames(Picea_2022)
colnames(Populus_2021)

#Get CRS from Headwall image for reprojecting shapefile coordinates
#Getting CRS from AK image not working ! Duh. Use Maine image.
#img<-terra::rast("/Users/peternelson 1/Documents/Schoodic/lecospec_at_schoodic/Git/lecospec/Data/Ground_Validation/Imagery/BisonGulchQuads.envi")
terra::crs(img)

#Filter out trees without location information
Populus_2021<-
  Populus_2021 %>% 
  dplyr::filter(Location_deg_min_sec_North_nosym>0) 

Picea_2022<-
  Picea_2022 %>% 
  dplyr::filter(Location_deg_min_sec_North_nosym>0) %>%
  dplyr::filter(Location_deg_min_sec_West.1>0)

dim(Picea_2022)
dim(Populus_2021)

#Use function from Stackoverflow to manually change DMS to DD
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

#Convert latitude from DMS to DD in a new column
#tree_locs$lat_dd<-angle2dec(tree_locs$Latitude)
#tree_locs_2022$lat_dd<-angle2dec(tree_locs_2022$Latitude)
#tree_locs_2023$lat_dd<-angle2dec(tree_locs_2023$Location_deg_min_sec_North)
Populus_2021$lat_dd<-angle2dec(Populus_2021$Location_deg_min_sec_North_nosym)
Populus_2021$lon_dd<-angle2dec(Populus_2021$Location_deg_min_sec_West_nosym)*-1

Picea_2022$lat_dd<-angle2dec(Picea_2022$Location_deg_min_sec_North_nosym)
Picea_2022$lon_dd<-angle2dec(Picea_2022$Location_deg_min_sec_West.1)*-1

#Convert longitude from DMS to DD, multiply by -1 to make is W hemisphere, in a new column
Populus_2021<-filter(Populus_2021, is.na(lon_dd)==FALSE)
Picea_2022<-filter(Picea_2022, is.na(lon_dd)==FALSE)

names_2023<-c(
"Tree_ID",
"lat_dd",
"lon_dd",
"Site_Code",
"Genus",
"Species",
"Sampling_Date",
"dbh_cm")

#tree_locs_2023<-filter(tree_locs_2023, is.na(lon_dd)==FALSE) %>% 
#  dplyr::select(all_of(names_2023)) %>%
#  dplyr::rename(Date = Sampling_Date,
#                DBH = dbh_.0.1_in)

Populus_2021<-filter(Populus_2021, is.na(lon_dd)==FALSE) %>% 
  dplyr::select(names_2023) %>%
  dplyr::rename(Date = Sampling_Date,
                DBH = dbh_cm)
Picea_2022<-filter(Picea_2022, is.na(lon_dd)==FALSE) %>% 
  dplyr::select(names_2023) %>%
  dplyr::rename(Date = Sampling_Date,
                DBH = dbh_cm)

#Add NAD83 datum to lat_dd and lon_dd
#Populus_2021_sf<-sf::st_as_sf(Populus_2021,coords = c("lon_dd","lat_dd"), dim="XY",crs = 4269)
Populus_2021_vect<-terra::vect(Populus_2021,geom = c("lon_dd","lat_dd"))
terra::crs(Populus_2021_vect)<-"EPSG:4326"
Populus_2021_vect_WGS_84<-terra::project(Populus_2021_vect, "EPSG:4328")
#Populus_2021_vect_WGS_84_vec_proj<-terra::project(Populus_2021_vect_WGS_84, img)
terra::crs(Populus_2021_vect_WGS_84)#_vec_proj)
proj_crs<-"EPSG:32619" #WGS84 projected inZone 19N in Maine
Populus_2021_vect_WGS_84_UTM<-terra::project(Populus_2021_vect_WGS_84, proj_crs)
terra::crs(Populus_2021_vect_WGS_84_UTM)
#Populus_2021_sf_WGS84<-sf::st_transform(Populus_2021_sf, crs = 4328)
#Populus_2021_sf_WGS84_vec<-vect(Populus_2021_sf_WGS84)

Picea_2022_sf<-sf::st_as_sf(Picea_2022,coords = c("lon_dd","lat_dd"), dim="XY",crs = 4269)
Picea_2022_sf_WGS84<-sf::st_transform(Picea_2022_sf, crs = 4326)
Picea_2022_sf_WGS84_vec<-vect(Picea_2022_sf_WGS84)
Picea_2022_sf_WGS84_vec_proj<-terra::project(Picea_2022_sf_WGS84_vec, proj_crs)

terra::crs(Picea_2022_sf_WGS84_vec_proj)

#crs_use<-terra::geom(vect(Populus_2021_sf_WGS84))
#tree_locs_2023_vec<-terra::vect(tree_locs_2023, geom = c("lon_dd", "lat_dd"), crs = crs_use, keepgeom  = TRUE)

#Cast to spatial points dataframe for writing to shapefile
tree_locs_sf_spat<-sf::as_Spatial(tree_locs_sf) 
tree_locs_2022_sf_spat<-sf::as_Spatial(tree_locs_2022_sf) 
tree_locs_2022_sf_WGS84_spat<-sf::as_Spatial(tree_locs_2022_sf_WGS84) 

#tree_locs_2023_sf_spat<-sf::as_Spatial(tree_locs_2023_sf) 
tree_locs_2023_sf_vec<-terra::vect(tree_locs_2023_sf) 
tree_locs_2023_sf_WGS84_vec<-terra::vect(tree_locs_2023_sf_WGS84) 
plot(tree_locs_2023_sf_vec)
#Map tree locations to verify there are more or less correct
leaflet(tree_locs_sf_spat) %>% 
  addMarkers() %>% 
  addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(minZoom = 0.1, maxZoom = 10))

leaflet(tree_locs_2022_sf_WGS84_spat) %>% 
  addMarkers() %>% 
  addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           options = providerTileOptions(minZoom = 0.1, maxZoom = 1000))

#leaflet(tree_locs_2023_sf_WGS84_spat) %>% 
#  addMarkers() %>% 
#  addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
#           options = providerTileOptions(minZoom = 0.1, maxZoom = 1000))
#Write shapfile to disk for use in a GIS for downstream analyses 
#writeOGR(tree_locs_sf_spat, "output/", "UMFK_MEIFSCI_tree_locations", driver = "ESRI Shapefile")
#sf::st_write(tree_locs_2022_sf, "output/UMFK_MEIFSCI_tree_locations_2022.shp", dsn = "geometry", driver = "ESRI Shapefile", append = FALSE)
#sf::st_write(tree_locs_2022_sf_WGS84, "output/UMFK_MEIFSCI_tree_2022_WGS84.shp", driver = "ESRI Shapefile", append = FALSE)

#sf::st_write(tree_locs_2023_sf, "output/UMFK_MEIFSCI_tree_locations_2023.shp", dsn = "geometry", driver = "ESRI Shapefile", append = TRUE)
#sf::st_write(tree_locs_2023_sf_WGS84, "output/UMFK_MEIFSCI_tree_2023_WGS84.shp", driver = "ESRI Shapefile", append = TRUE)
terra::writeVector(Populus_2021_vect_WGS_84_UTM, filename = "output/Populus_2021_sf_WGS84.shp", filetype= "ESRI Shapefile", overwrite = TRUE)
#terra::writeVector(Populus_2021_vect_WGS_84, filename = "output/Populus_2021_sf_WGS84.json", filetype= "GeoJSON", overwrite = TRUE)

terra::writeVector(Picea_2022_sf_WGS84_vec_proj, filename = "output/Picea_2022_sf_WGS84.shp", filetype= "ESRI Shapefile", overwrite = TRUE)

