source("Functions/lecospectR.R")

#Set directory
path <- ("output/canopy_spectra/")

#list all .grd files of FULL canopies
allfiles <- list.files(path) 
imgs <- subset(allfiles, grepl(".ENVI$", allfiles)==TRUE)# & grepl("full", allfiles)==TRUE)

  
####PARSE LOOP####
Canopy_labeled<-lapply(1:length(imgs), function(x){ 
  #extract individual canopy names
  imgs_names<-str_match(imgs[x], ".*ENVI") %>%
    as.data.frame()
  
  #bring in image
  tst<-terra::rast(paste(path,imgs[x], sep=""))
  #count bands
  band_count<-names(tst) %>% length()
  #create df with bands and values
  #df <- raster::rasterToPoints(tst) %>% 
  #  as.data.frame()%>%
  #  dplyr::select(-x,-y)
  df<-as.data.frame(tst)
  #extract and apply band names to expand df
  new_names<-extract_bands(df)
  names(df)<-new_names
  df <- filter_bands(df)
  df <- df_to_speclib(df, type="spectrolab")
  df<-spectrolab::resample(df, new_bands = seq(398, 999, 5), parallel = FALSE)
  
  #parse metadata from file name
  imgs_names<-gsub(".ENVI","", imgs_names) %>% as.data.frame()
  TrID<-separate(data.frame(A = imgs_names), col = "." , into = c("Site","TreeID", "Canopy_Type"), sep = "_")
  TrID<-as.data.frame(TrID)
  TrID$File <- paste0(path, imgs[x])
  #apply metadata to df
  meta(df)<-rep(TrID,length(df))
  return(df)
})

Canopy_image_spectra<-Reduce(spectrolab::combine,Canopy_labeled)
(as.data.frame(Canopy_image_spectra)) %>% group_by(Site, TreeID, Canopy_Type) %>% tally() %>% print(n=300)

#Write full spectra
write.csv(as.data.frame(Canopy_image_spectra), "output/speclib/spruce_canopy_speclib.csv")
saveRDS(Canopy_image_spectra,"output/speclib/spruce_canopy_speclib.rds")

