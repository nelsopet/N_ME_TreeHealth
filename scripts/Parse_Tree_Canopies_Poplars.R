source("Functions/lecospectR.R")

#Set directory
path <- ("output/canopy_spectra/Poplars/")

#list all .grd files of FULL canopies
allfiles <- list.files(path) 
imgs_all <- subset(allfiles, grepl(".ENVI$", allfiles)==TRUE)# & grepl("full", allfiles)==TRUE)
imgs_cal<-subset(imgs_all, grepl("_cal", imgs_all)==TRUE)
imgs_val<-subset(imgs_all, grepl("_val", imgs_all)==TRUE)
imgs<-c(imgs_cal,imgs_val)
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
  df<-spectrolab::resample(df, new_bands = seq(398, 999, 1), parallel = FALSE)
  
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
(as.data.frame(Canopy_image_spectra)) %>% dim #group_by(Site, TreeID, Canopy_Type) %>% tally() %>% print(n=300)
as.data.frame(Canopy_image_spectra) %>% dplyr::select(-names_drop) %>%
    dplyr::filter(Site == "HB3") %>% 
    dplyr::filter(Canopy_Type == "val") %>% dim
#Write full spectra
names_drop<-colnames(as.data.frame(Canopy_image_spectra)[,5:17])

Canopy_image_spectra_5nm<-spectrolab::resample(Canopy_image_spectra, new_bands = seq(398, 999, 5), parallel = FALSE)
Canopy_image_spectra_10nm<-spectrolab::resample(Canopy_image_spectra, new_bands = seq(398, 999, 10), parallel = FALSE)
Canopy_image_spectra_15nm<-spectrolab::resample(Canopy_image_spectra, new_bands = seq(398, 999, 15), parallel = FALSE)


write.csv(as.data.frame(Canopy_image_spectra) %>% dplyr::select(-names_drop),      "output/speclib/poplar_canopy_speclib.csv")
write.csv(as.data.frame(Canopy_image_spectra_5nm) %>% dplyr::select(-names_drop),  "output/speclib/poplar_canopy_speclib_5nm.csv")
write.csv(as.data.frame(Canopy_image_spectra_10nm) %>% dplyr::select(-names_drop), "output/speclib/poplar_canopy_speclib_10nm.csv")
write.csv(as.data.frame(Canopy_image_spectra_15nm) %>% dplyr::select(-names_drop), "output/speclib/poplar_canopy_speclib_15nm.csv")

saveRDS(Canopy_image_spectra,"output/speclib/poplar_canopy_speclib.rds")

#tst<-rast("output/canopy_spectra/HB3_R19_val.ENVI")
#tst
#plot(tst["483.389 nm"])
as.data.frame(Canopy_image_spectra) %>% 
  dplyr::select(-names_drop) %>% 
  group_by(Site, TreeID, Canopy_Type) %>% 
  tally %>% print(n=300)
