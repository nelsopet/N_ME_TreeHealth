source("Functions/lecospectR.R")

#Canopy spectra
#canopy_spectra_dir<-"output/canopy_spectra/"
#All calibration files
#cal_canopies<-list.files(canopy_spectra_dir)[grepl("*cal.ENVI$",list.files(canopy_spectra_dir))]
#All validation files
#val_canopies<-list.files(canopy_spectra_dir)[grepl("*val.ENVI$",list.files(canopy_spectra_dir))]

canopy_spectra<-readRDS("output/speclib/spruce_canopy_speclib.rds")
df_canopy<-as.data.frame(canopy_spectra)
chem_canopy<-read.csv("raw_data/UMFK_MEIF_2023_Active_09062023.csv")
chem_canopy<-chem_canopy %>% rename(TreeID = Tree_ID, Site = Site_Code)
chem_canopy$TreeID<-gsub("-","",chem_canopy$TreeID)
#Unique names for key fields
colnames(df_canopy)
colnames(chem_canopy)

#Join canopy spectra to chem
spec_chem_canopy<-(inner_join(df_canopy, chem_canopy, by = c("TreeID", "Site")))

head(spec_chem_canopy)

  spec_raw = df_canopy
  spec_mat = as.matrix(spectra)
  spec_df1 = as.data.frame(spec_raw)
  
  #combine relevant metadata
  spec_df2 = as.data.frame(spec_mat)
  spec_df2 = cbind(spec_df2, spec_df1[className])
  colnames(spec_df2)[colnames(spec_df2) == className] <- className
  uniqueNames = unique(spec_df1[[className]])
  
 # if (include_age == TRUE) {
 #   spec_df2$age = spec_df1$age
 #   age = 'with-age'
  #} else {
    age = 'no-age'
  #}
  
  #create data partition: 70% of data for training, 30% for testing
  inTrain <- caret::createDataPartition(
    y = spec_df2[[className]],
    p = .7,
    list = FALSE
  )
  
  training <- spec_df2[inTrain,]
  testing <- spec_df2[-inTrain,]