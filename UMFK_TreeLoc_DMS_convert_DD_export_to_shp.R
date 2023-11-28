#Load required packages
require(tidyverse)
require(measurements)
require(leaflet)
require(rgdal)
#Laod data from K.R.N.
#NOTE: These data were hand edited to correct two rows (one lat and one long) that had incorrect values.
tree_locs<-read.csv("raw_data/UMFK_MEIFSCI_tree_locations_20220614.csv", header=TRUE)
tree_locs_2022<-read.csv("raw_data/UMFK_MEIF_SPRUCE_2022_Active.csv", header=TRUE, encoding ='UTF-8')
tree_locs_2023<-read.csv("raw_data/UMFK_MEIF_2023_Active_09062023.csv", header=TRUE, encoding ='UTF-8')
colnames(tree_locs_2023)
#Filter out trees without location information
tree_locs<-
  tree_locs %>% 
  dplyr::filter(Latitude>0) 

tree_locs_2022<-
  tree_locs_2022 %>% 
  dplyr::filter(Latitude>0) %>%
  dplyr::filter(Longitude>0)

  tree_locs_2023<-
  tree_locs_2023 %>% 
  dplyr::filter(Location_deg_min_sec_North>0) %>%
  dplyr::filter(Location_deg_min_sec_West>0)
str(tree_locs_2023)
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
head(tree_locs_2022)
head(tree_locs_2023)

#Convert latitude from DMS to DD in a new column
tree_locs$lat_dd<-angle2dec(tree_locs$Latitude)
tree_locs_2022$lat_dd<-angle2dec(tree_locs_2022$Latitude)
tree_locs_2023$lat_dd<-angle2dec(tree_locs_2023$Location_deg_min_sec_North)
      
#Convert longitude from DMS to DD, multiply by -1 to make is W hemisphere, in a new column
tree_locs$lon_dd<-angle2dec(tree_locs$Longitude)*-1
tree_locs_2022$lon_dd<-angle2dec(tree_locs_2022$Longitude)*-1
tree_locs_2022<-filter(tree_locs_2022, is.na(lon_dd)==FALSE)
tree_locs_2023$lon_dd<-angle2dec(tree_locs_2023$Location_deg_min_sec_West)*-1

names_2023<-c(
"Tree_ID",
"lat_dd",
"lon_dd",
"Site_Code",
"Genus",
"Species",
"Sampling_Date",
"dbh_.0.1_in")

tree_locs_2023<-filter(tree_locs_2023, is.na(lon_dd)==FALSE) %>% 
  dplyr::select(all_of(names_2023)) %>%
  dplyr::rename(Date = Sampling_Date,
                DBH = dbh_.0.1_in)

unique(tree_locs_2023$lon_dd) %>% range()
unique(tree_locs_2023$lat_dd) %>% range()

#Add NAD83 datum to lat_dd and lon_dd
tree_locs_sf<-sf::st_as_sf(tree_locs,coords = c("lon_dd","lat_dd"), dim="XY",crs = 4269)
tree_locs_2022_sf<-sf::st_as_sf(tree_locs_2022,coords = c("lon_dd","lat_dd"), dim="XY",crs = 4269) #%>% sf::st_transform(crs = 4328)
tree_locs_2022_sf_WGS84<-sf::st_transform(tree_locs_2022_sf, crs = 4328)

tree_locs_2023_sf<-sf::st_as_sf(tree_locs_2023,coords = c("lon_dd","lat_dd"), dim="XY",crs = 4269) #%>% sf::st_transform(crs = 4328)
tree_locs_2023_sf_WGS84<-sf::st_transform(tree_locs_2023_sf, crs = 4328)
crs_use<-terra::geom(tree_locs_2023_vec)
tree_locs_2023_vec<-terra::vect(tree_locs_2023, geom = c("lon_dd", "lat_dd"), crs = crs_use, keepgeom  = TRUE)

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
writeOGR(tree_locs_sf_spat, "output/", "UMFK_MEIFSCI_tree_locations", driver = "ESRI Shapefile")
sf::st_write(tree_locs_2022_sf, "output/UMFK_MEIFSCI_tree_locations_2022.shp", dsn = "geometry", driver = "ESRI Shapefile", append = FALSE)
sf::st_write(tree_locs_2022_sf_WGS84, "output/UMFK_MEIFSCI_tree_2022_WGS84.shp", driver = "ESRI Shapefile", append = FALSE)

sf::st_write(tree_locs_2023_sf, "output/UMFK_MEIFSCI_tree_locations_2023.shp", dsn = "geometry", driver = "ESRI Shapefile", append = TRUE)
sf::st_write(tree_locs_2023_sf_WGS84, "output/UMFK_MEIFSCI_tree_2023_WGS84.shp", driver = "ESRI Shapefile", append = TRUE)
terra::writeVector(tree_locs_2023_vec, filename = "output/UMFK_MEIFSCI_tree_2023_WGS84.shp", filetype= "ESRI Shapefile", overwrite = TRUE)

plot(tree_locs_2023_vec)
