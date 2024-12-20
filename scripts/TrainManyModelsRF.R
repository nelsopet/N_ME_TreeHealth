source("Functions/lecospectR.R")
require(hexbin)
require(yardstick)

#Read in data
spec_chem_canopy<-read.csv("output/speclib/spruce_canopy_speclib_5nm_wChem.csv") 
canopy_VIs<-read.csv("output/speclib/spruce_VIs_5nm.csv") 

#Make a list of  band names for data filtering later
band_names<-colnames(spec_chem_canopy[,30:ncol(spec_chem_canopy)])
VI_names<-colnames(canopy_VIs[,6:ncol(canopy_VIs)])
#Join spectra with vegetation indices and Set the dataframe name to match the one used in the workflow
spec_chem_canopy<-spec_chem_canopy %>% 
    inner_join( canopy_VIs, by = c("X"="X","TreeID"="TreeID", "sample_name"="sample_name", "Site"="Site", "Canopy_Type"="Canopy_Type"), keep=FALSE) 
#sample_test <- spec_chem_canopy %>% #[is.na(dfa[className])==F,] %>% 
#    mutate(GenusSpecies = as.factor(GenusSpecies)) %>%
#    group_by(Site, TreeID, GenusSpecies) %>% 
#    mutate(UID = paste(Site,TreeID, sep="_")) %>% #, paste(className)) %>% #tally %>% dplyr::select(n) %>% ungroup() %>% summarise(min_pix= min(n), max_pix = max(n), median_pix = median(n))
#    slice_sample(n =80, replace = F)
#length(unique(sample_test$UID))
#windows()
#plot(sample_test$Average_TN_percent_DW~sample_test$Average_Condensed_TANNINS_percent_DW)


#Summarize all spectra to the plot/date level
spec_chem_canopy_mean<- spec_chem_canopy %>%
    #Remove rows with missing spectra
    dplyr::filter(is.na(Vogelmann4)==F) %>% 
    mutate(UID = paste(Site,TreeID, sep="_")) %>%
    group_by(Site, TreeID, UID) %>%
    #group_by(Site, TreeID, GenusSpecies) %>%
      summarise(across(X398:Vogelmann4, ~ median(.x, na.rm = TRUE))) 

Resp_names<-as.data.frame(colnames(spec_chem_canopy[,11:29]))[1:19,]
#Resp_names<-Resp_names[-21:-22]
Resp_data_only<-as.data.frame(unique(spec_chem_canopy[c(Resp_names,"Site","TreeID")]))
spec_chem_canopy_mean<-inner_join(Resp_data_only, spec_chem_canopy_mean, by=c("Site","TreeID")) 

#Set seed for stable cal/val split
set.seed(1234)

#List columns names available for modeling 
#colnames(spec_chem_canopy)
#vars<-colnames(spec_chem_canopy[,c(8,11:29)])

vars_pretty<-c(
#"Species",
"Diameter at breast height",# (cm), #[1] "dbh_cm"
"Stem Canker", # [2] "No_stem_cankers"
"Tree Height",# (m),# [3] "Total_tree_height_m"
"Crown Density", # [4] "Crown_density_percent"
"Crown Die Back", # [5] "Crown_dieback_percent"
"Vigor Class", # [6] "Vigor_class"
"Uncompacted Live Crown",# [7] "Uncompacted_live_crown_ratio"
"Diameter Squared x Height",# [8] "diameter_squared_x_height_cubic_cm"
"Specific Leaf Area",# [9] "SLA_cm2_per_g"
"Area Per Needle",#[10] "Area_per_needle_cm2"
"Fv/Fm",#[11] "Fv_Fm"
"Amax", #(umol/m2/sec),#[12] "Amax_normalized_umol_per_m2_sec"
"Stomatal conductance",# (gsw, umol/m2/sec), #[13] "gsw_normalized_umol_per_m2_sec"
"Condensed Tannins", #(% dry mass),#[15] "Average_Condensed_TANNINS_percent_DW"
"Nitrogen",# (% dry mass),#[14] "Fv_Fm.1"
"Carbon",# (% dry mass),
"Sugar", #(% dry mass),#[16] "Average_SUGAR_percent_DW"
"Starch", #(% dry mass))#[17] "Average_STARCH_percent_DW"
"Species of Spruce")
#spec_chem_canopy[spec_chem_canopy == "not sampled"] <- NA
#spec_chem_canopy<-apply(spec_chem_canopy,2,as.numeric)

lapply(1:length(vars_pretty), function(x) {
 x=19
 className=vars[x]

  
#is.character(spec_chem_canopy[className])==TRUE
#df<-spec_chem_canopy[spec_chem_canopy[className]!="not sampled"]
#df[className]<-as.numeric(df[className])

#Pixels as sample units
#dfa<-spec_chem_canopy

#Trees as sample units
dfa<-spec_chem_canopy_mean
#dim(df)
dfa<-dfa %>% subset(dfa[className] != "not sampled")
dfa<-dfa[is.na(dfa[className])==F,]
#dfa<- dfa %>% mutate(UID = paste(Site,TreeID, sep="_"))

##IF STATEMENT NEEDED
##Use block below for responses that are continuous
dfa[className]<-as.numeric(dfa[[className]])

##IF STATEMENT NEEDED
##Use block below for responses that are factors like vigor class
  #dfa[className]<-as.factor(dfa[[className]])

lapply(1:100, function(ty), {
  print(ty)
   iteration = ty
    UID_list<-unique(dfa$UID)
    rNum<-sample(seq(1, length(UID_list), by=1), length(UID_list), replace=FALSE)
    UID_filt<-as.data.frame(cbind(UID_list,rNum))
    dfa<-dfa %>% inner_join(UID_filt, by = c("UID"="UID_list"), keep=F)

    training<-
    dfa  %>% filter(rNum < (0.5*length(UID_list))) %>% #dplyr::select(UID) %>% unique() %>% dim() #tally() %>% print(n=200)
    ungroup %>% dplyr::select(className, band_names, VI_names)
    #dim(training)
    #unique( dfa  %>% filter(rNum<(0.7*length(UID_list))) %>% dplyr::select(UID))

    testing<-
    dfa  %>% filter(rNum>(0.5*length(UID_list))) %>% #dplyr::select(UID) %>% unique() %>% dim() #tally() %>% print(n=200)
    ungroup %>% dplyr::select(className, band_names, VI_names)
    #dim(testing)
    #unique( dfa  %>% filter(rNum>(0.7*length(UID_list))) %>% dplyr::select(UID))

    #train_UIDs<-dfa  %>% filter(rNum <= (0.8*length(UID_list))) %>% ungroup() %>% dplyr::select(UID) %>% unique
    #test_UIDs<-dfa %>% filter(rNum>(0.8*length(UID_list))) %>% ungroup() %>% dplyr::select(UID)  %>% unique


#spec_chem_canopy_df<-dfa %>% 
##    subset(AVG_Fv_Fm != "not sampled") %>%
#    mutate(#AVG_Fv_Fm = as.numeric(AVG_Fv_Fm), 
#      Vigor_class = as.factor(Vigor_class), 
#      GenusSpecies = as.factor(GenusSpecies)) %>%
#      #Filter outliers by response
#        #subset(SLA_cm2_per_g < 149) %>%
#        #subset(Branch_1_Amax_normalized_umol_per_m2_sec <87) %>%
#        #subset(Branch_1_gsw_normalized_umol_per_m2_sec<1.99) %>%
#    #For continuous variables, balance sample by site/TreeID/species
#    group_by(Site, TreeID, GenusSpecies) %>%#, dfa[[className]]) %>% #tally %>% dplyr::select(n) %>% ungroup() %>% summarise(min_pix= min(n), max_pix = max(n), median_pix = median(n))
#        slice_sample(n =80, replace = F)
#
#    #For categorical variables, balance sample by level
#      #group_by(GenusSpecies) %>%
#      #slice_sample(n =3000, replace = F)
#    #check data size
#    dim(spec_chem_canopy_df)
#    #check if there are NAs
#    sum(is.na(spec_chem_canopy_df[className]))
#    #check number of samples by species
#    spec_chem_canopy_df %>% group_by(GenusSpecies) %>% tally

        #Partion based on clase name using pixel-based approach
        #Create a test and train split
        #inTrain <- caret::createDataPartition(
        #  y = spec_chem_canopy_df[[className]],
        #  p = 0.7,
        #  list = FALSE#,
        #  #na.rm = TRUE
        #)
        #training <- spec_chem_canopy_df[inTrain,]  %>% ungroup %>% dplyr::select(className, band_names)
        #  #Check dimensions
        #  dim(training)
        #testing <- spec_chem_canopy_df[-inTrain,]  %>% ungroup %>% dplyr::select(className, band_names)
        #  #Check dimensions
        #  dim(testing)
        #    #Variable Importance RF model
        #      #n=10000
        #      #rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),
        #      #data = training,num.trees = n, importance = "impurity_corrected")
        #      #plot(sort(rf_mod$variable.importance))
             testing_labels <- #df[-inTrain,] %>% 
                dfa  %>% filter(rNum>(0.5*length(UID_list))) %>%
                ungroup %>% dplyr::select(className, Site, GenusSpecies, TreeID, UID)  #%>% rename(Field=Field.x)
        
    #Prediction RF model
    n=5000
    rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),data = training, num.trees = n)
    #Save model to disk
    #saveRDS(rf_mod, paste("WymansNutrientTrial2023/output/mle/mods/ds_only/", className,"_", y, sep=""))       
    #Predict validation data and calculate fit
    rf_mod_pred<-predict(rf_mod, testing, na.rm=TRUE)
    fit<-round(R2(rf_mod_pred$predictions, testing[className], formula = "corr"),2)
    fit
    #Write predictions, observed values to disk
    #pred_out<-as.data.frame(cbind(rf_mod_pred$predictions, testing[className]))
    testing_labels$Predictions<-rf_mod_pred$predictions
    testing_labels<-testing_labels[-1]
    testing_labels$Observed<-testing[[className]]
    #colnames(pred_out)<-c("prediction, observed")
    testing_labels$var<-className
    testing_labels$mod_num<-paste(yt)
    write.csv(testing_labels,paste("output/models/predictions/",className,"_median_10nmBands_",yt,".csv",sep=""))
    })
      #Old way
      #print(paste("building ranger model for ",className,sep="")
      #rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),
      #data = training,num.trees = n)
      #rf_mod_pred <-predict(rf_mod, testing)
    
      #training %>% group_by(Vigor_class) %>% tally
    
    ##PLS
    #ctrl <- caret::trainControl(
    #method = "repeatedcv",
    #number = 10,
    #sampling = "down",
    #repeats = 3)

    #ncomp=30
  #Fit model. Note max iterations set to 100000 to allow model convergence
  #plsFit <- caret::train(
  #  as.formula(paste(className, "~.")),
  #  data = training,
  #  maxit = 10000,
  #  method = "pls",
  #  trControl = ctrl,
  #  tuneLength = ncomp)
#
  # plsFit_pred<- predict(plsFit, newdata = testing)
  
  # plsFit
    

    #rpd = round(2 * abs(testing[className] - rf_mod_pred$predictions) / (rf_mod_pred$predictions + testing[className]),2) 
    
    ##Use R2 for continuous variables
    R2stat = round(R2(rf_mod_pred$predictions, testing[[className]], formula = "corr"),2)
    ##Use block below for responses that are factors like vigor class
    obs = testing[[className]]
    pred = rf_mod_pred$predictions
    conf_mat_plt<-data.frame(obs, pred)
    cm<-conf_mat(conf_mat_plt, obs, pred)
    conf_mat_stats<-caret::confusionMatrix(conf_mat_plt$obs, conf_mat_plt$pred)   
    acc = round(conf_mat_stats$overall[1],3)

    CR = colorRampPalette((c("blue","green")))

    ##Use plotting block below for continuous variables
    #jpeg(paste("output/models/figs/", vars[x], "cal_v_val_density.jpg"))#, width = 700, height = 600)
    hex<-hexbin::hexbin(rf_mod_pred$predictions, testing[[className]], xbins = 30)
    plot(hex)#, 
        ##Use block for continuous variables
        main = paste(vars_pretty[x], "  R2  =  ",R2stat, sep=""),
        ##Use block for factors
        main = paste(vars_pretty[x], "  Accuracy  =  ",acc, sep=""),
        xlab = paste("Calibration  ", vars_pretty[x], sep=""), 
        ylab = paste("Validation  ", vars_pretty[x], sep=""),
        colramp = CR,
        colorcut = 10)
    
    #par(cex.axis=3, cex.main=3, cex.sub=3)

    ##Use block below for viz of factors
    #jpeg(paste("output/models/figs/", vars[x], "cal_v_val_density.jpg", sep=""), width = 700, height = 600)
    #    autoplot(cm, type = "heatmap") + 
    #      scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
    #        labs(x = paste("Calibration  ", vars_pretty[x], sep=""), y = paste("Validation  ", vars_pretty[x], sep=""), title = paste(vars_pretty[x], "  Accuracy =  ",acc, sep="")) +
    #          theme(plot.title = element_text(hjust = 0.5, size =16, face="bold"),
    #            axis.text=element_text(size=14, face="bold"),
    #            axis.title=element_text(size=14,face="bold"))
    #
    
    dev.off()
    print(vars[x])
    })

lapply(1:length(vars), function(x){ctrl <- caret::trainControl(
    className
   })