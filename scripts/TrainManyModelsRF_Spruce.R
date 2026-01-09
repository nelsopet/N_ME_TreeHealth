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


#Set seed for stable cal/val split
seed =sample(1:1E12, size=1)
set.seed = seed

#Set number of models to build per variable
models_per_var = 50

#Set number of pixels to randomly select per tree
pixels_per_tree = 50

#Summarize all spectra to the plot/date level
spec_chem_canopy_mean<- spec_chem_canopy %>%
    #Filter species
    dplyr::filter(GenusSpecies == "Red_Spruce") %>%
    #Remove rows with missing spectra
    dplyr::filter(is.na(Vogelmann4)==F) %>% 
    mutate(UID = paste(Site,TreeID, sep="_")) %>%
    group_by(Site, TreeID, UID) %>% #tally %>% dplyr::select(n) %>% print(n=200)
    slice_sample(n =pixels_per_tree, replace = F) %>%
    #group_by(Site, TreeID, GenusSpecies) %>% 
    summarise(across(X398:Vogelmann4, ~ median(.x, na.rm = TRUE)))

dim(spec_chem_canopy_mean)

Resp_names<-as.data.frame(colnames(spec_chem_canopy[,11:29]))[1:19,]
#Resp_names<-Resp_names[-21:-22]
Resp_data_only<-as.data.frame(unique(spec_chem_canopy[c(Resp_names,"Site","TreeID")]))
spec_chem_canopy_mean<-inner_join(Resp_data_only, spec_chem_canopy_mean, by=c("Site","TreeID")) 

#List columns names available for modeling 
colnames(spec_chem_canopy_mean)
vars<-colnames(spec_chem_canopy_mean[,c(1:18)])

lapply(1:(length(vars_pretty)-1), function(x) {

 #x=18
 className=vars[x]
 className
  
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

lapply(1:models_per_var, function(ty) {
#ty=1
  print(ty)
   iteration = ty
    UID_list<-unique(dfa$UID)
    rNum<-sample(seq(1, length(UID_list), by=1), length(UID_list), replace=FALSE)
    UID_filt<-as.data.frame(cbind(UID_list,rNum))
    dfb<-dfa %>% inner_join(UID_filt, by = c("UID"="UID_list"), keep=F) %>% mutate(rNum = as.numeric(rNum))
    
    thresh = 0.8
    #thresh  = median(as.numeric(UID_filt$rNum))
    #dfb %>% dplyr::filter(rNum>thresh) %>% dim
    #dfb %>% dplyr::filter(rNum<thresh) %>% dim
    #setdiff(tstnames,tstnames2)    
    
    training<-
    dfb  %>% filter(rNum < (thresh*length(UID_list))) %>% #dplyr::select(UID) %>% unique() %>% dim() #tally() %>% print(n=200)
    ungroup %>% dplyr::select(className, band_names, VI_names)
    dim(training)
    #unique( dfa  %>% filter(rNum<(0.7*length(UID_list))) %>% dplyr::select(UID))

    testing<-
    dfb  %>% filter(rNum > (thresh*length(UID_list))) %>% #dplyr::select(UID) %>% unique() %>% dim() #tally() %>% print(n=200)
    ungroup %>% dplyr::select(className, band_names, VI_names)
    dim(testing)
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
                dfb  %>% filter(rNum>(thresh*length(UID_list))) %>%
                ungroup %>% dplyr::select(className, Site, GenusSpecies, TreeID, UID)  #%>% rename(Field=Field.x)
        
    #Prediction RF model
    n=5000
    rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),data = training, num.trees = n)
    #Save model to disk
    #saveRDS(rf_mod, paste("WymansNutrientTrial2023/output/mle/mods/ds_only/", className,"_", y, sep=""))       
    #Predict validation data and calculate fit
    rf_mod_pred<-predict(rf_mod, testing, na.rm=TRUE)
    fit<-round(R2(rf_mod_pred$predictions, testing[className], formula = "corr"),2)
    print(paste(className, " , R2 =", fit, sep=""))
    #Write predictions, observed values to disk
    #pred_out<-as.data.frame(cbind(rf_mod_pred$predictions, testing[className]))
    testing_labels$Predictions<-rf_mod_pred$predictions
    testing_labels<-testing_labels[-1]
    testing_labels$Observed<-testing[[className]]
    #colnames(pred_out)<-c("prediction, observed")
    testing_labels$var<-className
    testing_labels$mod_num<-paste(ty)
    write.csv(testing_labels,paste("output/models/predictions/picea/tree_sample_unit/red_spruce/",className,"_median_5nmBands_",ty,".csv",sep=""))
    })
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
    


#Merge predictions across models
#TODO
all_out<-list.files("output/models/predictions/picea/tree_sample_unit/")
    #Filter out only dates desired
    #all_out<-all_out[grepl("_median_5nmBands",all_out)]
    all_out<-all_out[grepl(".csv",all_out)]
all_preds<-lapply(1:length(all_out), function(x) 
    {read.csv(paste("output/models/predictions/picea/tree_sample_unit/",all_out[x],sep=""))
    })
all_preds<-Reduce(rbind, all_preds) 
dim(all_preds)
unique(all_preds$GenusSpecies)

all_preds$Obs_vs_Pred<-abs(all_preds$Observed/all_preds$Predictions)

all_preds %>% group_by(var) %>% 
  #subset(var == "Branch_1_gsw_normalized_umol_per_m2_sec") %>%
  subset(Obs_vs_Pred>5) %>% 
  dplyr::select(var, UID) %>% unique %>% print(n=25)

#all_preds<-all_preds %>% subset(Obs_vs_Pred<5)

all_preds_mean<-all_preds %>% #dim
#Filter outliers
#dplyr::anti_join(outliers, by=c("UID"="UID","JDate"="JDate")) %>% #dim
group_by(var) %>% 
    summarise(R2 = round(R2(Predictions, Observed, formula = "corr"),2),
         cor2 = cor(Predictions, Observed)^2,
         MAE = round(Metrics::mae(Predictions, Observed),3),
         MSE = round(Metrics::mse(Predictions, Observed),3),
         RMSE = round(Metrics::rmse(Predictions, Observed),3),
         mean = round(mean(Observed),3),
         min_obs = min(Observed),
         max_obs = max(Predictions),
         IQR_75 = quantile(Observed, probs = c(0.75)),
         IQR_25 = quantile(Observed, probs = c(0.25)),
         range_obs = abs(max_obs-min_obs),
         nRMSE_range_obs = RMSE/range_obs,
         nRMSE_mean = RMSE/mean,
         nRMSE_IQR = round(RMSE/(IQR_75-IQR_25),2)
          ) 
#write.csv(all_preds_mean,"output/mle/summaries/mod_stats_2023_2024_70cal30val_median5nm_2runs_allpx_JDate.csv")
all_preds_mean %>% print(n=40)

all_preds_stats<-all_preds %>% 
group_by(var) %>%
summarise(R2 = round(R2(Predictions, Observed, formula = "corr"),2),
         cor2 = cor(Predictions, Observed)^2,
         MAE = round(Metrics::mae(Predictions, Observed),3),
         MSE = round(Metrics::mse(Predictions, Observed),3)
          ) %>% dplyr::filter(R2>=0.3) #%>% print(n=25)
all_preds_stats

#Summarize number of times a UID is used in testing
plot_mod_use_count<-all_preds %>% group_by(UID,var, mod_num) %>% tally %>% dplyr::select(n) #%>% max #tidyr::spread(var, n)
range(plot_mod_use_count$n)
all_preds %>% dplyr::anti_join(outliers, by=c("UID"="UID","JDate"="JDate"))%>% dim

##Make obs vs pred across all models
#Remove outliers
all_preds_df = all_preds #%>% subset(Obs_vs_Pred<5) 
dim(all_preds_df)
#dplyr::anti_join(outliers, by=c("UID"="UID","JDate"="JDate")) #dim
head(all_preds_mean)
unique(all_preds_mean$GenusSpecies)

all_preds_size_id<-all_preds_df %>% ungroup() %>% 
  dplyr::select(UID,var,Observed) %>% subset(var=="dbh_cm") %>% 
  unique %>% as.data.frame() %>% dplyr::select(-var) %>% rename(dbh=Observed)

all_preds_df<-all_preds_df %>% inner_join(all_preds_size_id, by="UID")
unique(all_preds_df$GenusSpecies)

lapply(1:length(Resp_names), function(x) {
#x=3
className = Resp_names[x]
resp_fit = all_preds_mean[all_preds_mean$var == className,]
df_var<-all_preds_df[all_preds_df$var == className,]
unique(df_var$GenusSpecies)
df_var_site_id<-df_var %>% ungroup() %>% dplyr::select(Site) %>% unique %>% as.data.frame()
df_var_site_id$Site_Num<-c(seq(1:length(df_var_site_id$Site)))

df_var_sp_id<-df_var %>% ungroup() %>% dplyr::select(GenusSpecies) %>% unique %>% as.data.frame()
df_var_sp_id$Sp_Num<-c(seq(1:length(df_var_site_id$GenusSpecies)))

df_var<-df_var %>% inner_join(df_var_site_id, by="Site", keep=FALSE) %>% inner_join(df_var_sp_id, by="GenusSpecies", keep=FALSE)

print(className)
print(paste("Model R2 = ", all_preds_mean[x,]$R2, sep=""))
print(paste("Model MAE = ", all_preds_mean[x,]$MAE, sep=""))

jpeg(paste("output/models/figs/picea/",className,"_median_5nmBands_5nmVIs_80cal_20val",".jpg", sep=""), height = 500, width=550)

#Uncomment when using JDate as a predictor
plot(df_var$Predictions,df_var$Observed, col = df_var$Site_Num, pch=df_var$Sp_Num, cex = log(df_var$dbh,base=4),
#plot(df_var$Predictions,df_var$Observed, pch = df_var$Field_Num,
main = paste("Observed vs Predicted ", className,",\n  R-squared = ",resp_fit$R2, ", nRMSE = ", resp_fit$nRMSE_IQR, sep=""), 
    xlab = paste("predicted ",className,sep=""), 
    ylab = paste("observed ",className, sep="")) 
graphics::legend("bottomright", legend = unique(df_var$Site),col=unique(df_var$Site_Num), pch = rep(5,length((df_var$Site))))
graphics::legend("topleft", legend = unique(df_var$GenusSpecies),pch = unique(df_var$Sp_Num))

#Uncomment out when using JDate in the models

abline(a=0,b=1)
dev.off()
#Make plot-level summaries of predictions
#testing_labels_pred <-testing_labels %>% mutate(predictions = rf_mod_pred$predictions)
})






#  #rpd = round(2 * abs(testing[className] - rf_mod_pred$predictions) / (rf_mod_pred$predictions + testing[className]),2) 
#  
#  ##Use R2 for continuous variables
#  R2stat = round(R2(rf_mod_pred$predictions, testing[[className]], formula = "corr"),2)
#  ##Use block below for responses that are factors like vigor class
#  obs = testing[[className]]
#  pred = rf_mod_pred$predictions
#  conf_mat_plt<-data.frame(obs, pred)
#  cm<-conf_mat(conf_mat_plt, obs, pred)
#  conf_mat_stats<-caret::confusionMatrix(conf_mat_plt$obs, conf_mat_plt$pred)   
#  acc = round(conf_mat_stats$overall[1],3)
#
#  CR = colorRampPalette((c("blue","green")))
#
#  ##Use plotting block below for continuous variables
#  #jpeg(paste("output/models/figs/", vars[x], "cal_v_val_density.jpg"))#, width = 700, height = 600)
#  hex<-hexbin::hexbin(rf_mod_pred$predictions, testing[[className]], xbins = 30)
#  plot(hex)#, 
#      ##Use block for continuous variables
#      main = paste(vars_pretty[x], "  R2  =  ",R2stat, sep=""),
#      ##Use block for factors
#      main = paste(vars_pretty[x], "  Accuracy  =  ",acc, sep=""),
#      xlab = paste("Calibration  ", vars_pretty[x], sep=""), 
#      ylab = paste("Validation  ", vars_pretty[x], sep=""),
#      colramp = CR,
#      colorcut = 10)
#  
#  #par(cex.axis=3, cex.main=3, cex.sub=3)
#
#  ##Use block below for viz of factors
#  #jpeg(paste("output/models/figs/", vars[x], "cal_v_val_density.jpg", sep=""), width = 700, height = 600)
#  #    autoplot(cm, type = "heatmap") + 
#  #      scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
#  #        labs(x = paste("Calibration  ", vars_pretty[x], sep=""), y = paste("Validation  ", vars_pretty[x], sep=""), title = paste(vars_pretty[x], "  Accuracy =  ",acc, sep="")) +
#  #          theme(plot.title = element_text(hjust = 0.5, size =16, face="bold"),
#  #            axis.text=element_text(size=14, face="bold"),
#  #            axis.title=element_text(size=14,face="bold"))
#  #
