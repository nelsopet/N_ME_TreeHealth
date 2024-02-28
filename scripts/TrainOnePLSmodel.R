source("Functions/lecospectR.R")

#Read in data
spec_chem_canopy<-read.csv("output/speclib/spruce_canopy_speclib_5nm_wChem.csv")

#Set seed for stable cal/val split
set.seed(1234)

#List of band names
band_names<-colnames(spec_chem_canopy[,29:ncol(spec_chem_canopy)])

#List columns names available for modeling 
  str(spec_chem_canopy[,11:28])
#Set the response variable for modeling
className = "Average_STARCH_percent_DW"
#Filter data to include rows with the response variable
#spec_chem_canopy_n25<-spec_chem_canopy %>% 
#    group_by(Site, TreeID, GenusSpecies, Canopy_Type) %>% 
#    #dplyr::filter(Canopy_Type == "cal") %>%
#    #dplyr::filter(is.na(Vigor_class)==F) %>%
#    dplyr::filter(is.na(Average_STARCH_percent_DW)==F) %>%
#    #dplyr::filter(GenusSpecies == "Red_Spruce") %>%
#    dplyr::mutate(Area_per_needle_cm2 = as.numeric(Area_per_needle_cm2))# %>%
        #mutate(Vigor_class = as.factor(Vigor_class)) %>%

    #slice_sample(n =15, replace = F)
spec_chem_canopy_n25<-spec_chem_canopy[is.na(spec_chem_canopy[className])==F,]# %>% dim
  #tally(spec_chem_canopy_n25 %>% group_by(Site, TreeID, GenusSpecies,Canopy_Type)) %>% print(n=100)

  #Create a test and train split
  inTrain <- caret::createDataPartition(
    y = spec_chem_canopy_n25[[className]],
    p = 0.7,
    list = FALSE#,
    #na.rm = TRUE
  )
  
training <- spec_chem_canopy_n25[inTrain,]  %>% ungroup %>% dplyr::select(className, band_names)
testing <- spec_chem_canopy_n25[-inTrain,]  %>% ungroup %>% dplyr::select(className, band_names)

#Alternatively, use cal and val polygons  
#cal<-spec_chem_canopy_n25 %>%
#    dplyr::filter(Canopy_Type == "cal")  %>% ungroup %>% dplyr::select(className, band_names) 
#val<-spec_chem_canopy_n25 %>%
#    dplyr::filter(Canopy_Type == "val")  %>% ungroup %>% dplyr::select(className, band_names)

#dim(cal)
#dim(val) 


################# Ranger models aka RF
n=1000
     rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),
    data = training,num.trees = n)
    rf_mod_pred<-predict(rf_mod, testing)
    windows()
    plot(hexbin::hexbin(rf_mod_pred$predictions, testing$Average_STARCH_percent_DW))
    #abline(lm(rf_mod_pred$predictions~ testing$dbh_cm))
    #abline(0,1)
    R2(rf_mod_pred$predictions, testing$Average_STARCH_percent_DW, formula = "corr")
 
#################Partial least squares regression 
  #tune model: 10-fold cross-validation repeated 3 times
  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = 10,
    #sampling = "down",
    repeats = 3)
  ncomp = 30
  #Fit model. Note max iterations set to 100000 to allow model convergence
  plsFit <- caret::train(
    as.formula(paste(className, "~.")),
    data = training,
    maxit = 10000,
    method = "pls",
    trControl = ctrl,
    tuneLength = ncomp)

   plsFit_pred<- predict(plsFit, newdata = testing)
    #corrplot::corrplot(plsFit_pred)    
   #windows()
   caret::confusionMatrix(plsFit)
   accuracy = sum(ifelse(plsFit_pred==val$GenusSpecies, 1,0))/length(val$GenusSpecies)
    accuracy    
    plot(plsFit_pred ~ testing$Average_Condensed_TANNINS_percent_DW)
    summary(lm(plsFit_pred ~ testing$Average_Condensed_TANNINS_percent_DW))
    R2(plsFit_pred,testing$Fv_Fm)

caret::R2(plsFit_pred, val$GenusSpecies)
caret::RMSE()
xyplot(plsFit)
median(plsFit$resample$Rsquared)













