source("Functions/lecospectR.R")
set.seed(1928374)
#Canopy spectra
canopy_spectra<-read.csv("output/speclib/poplar_canopy_speclib_5nm.csv")
df_canopy<-as.data.frame(canopy_spectra)
colnames(df_canopy)
#Remove redundant columns
df_canopy<-df_canopy[,-1]
band_names<-colnames(df_canopy[,5:ncol(df_canopy)])

df_canopy %>% group_by(Site,TreeID) %>% tally %>% print(n=200)

#Leaf-level chemistry
chem_canopy<-read.csv("raw_data/MEIF-SCI Forest Remote Sensing 2021 POPLAR Data PRELIM FOR MODELING.csv")
colnames(chem_canopy)

chem_canopy<-chem_canopy %>% rename(TreeID = Tree_ID, Site = Site_Code)
chem_canopy$TreeID<-gsub("-","",chem_canopy$TreeID)

unique(chem_canopy$TreeID)

chem_canopy$GenusSpecies<-ifelse(grepl("*Bal",chem_canopy$TreeID), "Balsam_Poplar", ifelse(grepl("*Asp",chem_canopy$TreeID), "Aspen", "Big_Tooth_Aspen"))
chem_canopy %>% group_by(GenusSpecies) %>% tally
#Unique names for key fields
colnames(df_canopy)
colnames(chem_canopy)
head(chem_canopy)

unique(chem_canopy$Site)
unique(df_canopy$Site)
#Change Site to match
chem_canopy$Site<-gsub("-","",chem_canopy$Site)

#Check to make sure sites match
setdiff(unique(chem_canopy$Site),unique(df_canopy$Site))

#Compare TreeIDs between the two canopy spectra and leaf chemistry
setdiff(unique(chem_canopy$TreeID),unique(df_canopy$TreeID))

#Join canopy spectra to chem
spec_chem_canopy<-(inner_join(df_canopy, chem_canopy, by = c("TreeID", "Site"))) %>% #dim
    dplyr::select(colnames(df_canopy[,1:4]), colnames(chem_canopy), band_names)

dim(spec_chem_canopy)  
  windows()
  hist(as.numeric(spec_chem_canopy$Area_per_needle_cm2))
  

write.csv(spec_chem_canopy, "output/speclib/poplar_canopy_speclib_5nm_wChem.csv")
