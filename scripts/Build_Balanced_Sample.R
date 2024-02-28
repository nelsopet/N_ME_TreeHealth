source("Functions/lecospectR.R")
set.seed(1928374)
#Canopy spectra
#canopy_spectra<-readRDS("output/speclib/spruce_canopy_speclib.rds")
canopy_spectra<-read.csv("output/speclib/spruce_canopy_speclib_5nm.csv")
df_canopy<-as.data.frame(canopy_spectra)
colnames(df_canopy)
#Remove redundant columns
df_canopy<-df_canopy[,-1]
band_names<-colnames(df_canopy[,5:ncol(df_canopy)])

df_canopy %>% group_by(Site,TreeID) %>% tally %>% print(n=200)

chem_canopy<-read.csv("raw_data/UMFK_MEIF_2023_Active_09062023.csv")
chem_canopy<-chem_canopy %>% rename(TreeID = Tree_ID, Site = Site_Code)
chem_canopy$TreeID<-gsub("-","",chem_canopy$TreeID)
chem_canopy$GenusSpecies<-ifelse(grepl("*R",chem_canopy$TreeID), "Red_Spruce", ifelse(grepl("*W",chem_canopy$TreeID), "White_Spruce", "Black_Spruce"))
chem_canopy %>% group_by(GenusSpecies) %>% tally
#Unique names for key fields
colnames(df_canopy)
colnames(chem_canopy)
head(chem_canopy)
#lapply(1:length(colnames(chem_canopy)), function (x) {length(is.na(chem_canopy[,x]==FALSE))})
#Join canopy spectra to chem
spec_chem_canopy<-(inner_join(df_canopy, chem_canopy, by = c("TreeID", "Site"))) %>% 
    dplyr::select(colnames(df_canopy[,1:4]), colnames(chem_canopy), band_names)
  windows()
  hist(as.numeric(spec_chem_canopy$Area_per_needle_cm2))
  

write.csv(spec_chem_canopy, "output/speclib/spruce_canopy_speclib_5nm_wChem.csv")
