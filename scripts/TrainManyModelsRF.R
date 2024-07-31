source("Functions/lecospectR.R")
require(hexbin)
require(yardstick)

#Read in data
spec_chem_canopy<-read.csv("output/speclib/spruce_canopy_speclib_5nm_wChem.csv")
band_names<-colnames(spec_chem_canopy[,29:ncol(spec_chem_canopy)])

#Set seed for stable cal/val split
set.seed(1234)

#List columns names available for modeling 

vars<-colnames(spec_chem_canopy[,11:28])

vars_pretty<-c(
"Diameter at breast height (cm)", #[1] "dbh_cm"
"Stem Canker", # [2] "No_stem_cankers"
"Tree Height (m)",# [3] "Total_tree_height_m"
"Crown Density (%)", # [4] "Crown_density_percent"
"Crown Die Back (%)", # [5] "Crown_dieback_percent"
"Vigor Class", # [6] "Vigor_class"
"Uncompacted Live Crown (ratio)",# [7] "Uncompacted_live_crown_ratio"
"Diameter Squared x Height (cm3)",# [8] "diameter_squared_x_height_cubic_cm"
"Specific Leaf Area (cm2/g)",# [9] "SLA_cm2_per_g"
"Area Per Needle (cm2)",#[10] "Area_per_needle_cm2"
"Fv/Fm",#[11] "Fv_Fm"
"Amax (umol/m2/sec)",#[12] "Amax_normalized_umol_per_m2_sec"
"Stomatal conductance (gsw, umol/m2/sec)", #[13] "gsw_normalized_umol_per_m2_sec"
"Condensed Tannins (% dry mass)",#[15] "Average_Condensed_TANNINS_percent_DW"
"Nitrogen (% dry mass)",#[14] "Fv_Fm.1"
"Carbone (% dry mass)",
"Sugar (% dry mass)",#[16] "Average_SUGAR_percent_DW"
"Starch (% dry mass)")#[17] "Average_STARCH_percent_DW"
#"Species of Spruce")
#spec_chem_canopy[spec_chem_canopy == "not sampled"] <- NA
#spec_chem_canopy<-apply(spec_chem_canopy,2,as.numeric)

lapply(18:18, function(x) {
 #x=6
className=vars[x]

  
#is.character(spec_chem_canopy[className])==TRUE
#df<-spec_chem_canopy[spec_chem_canopy[className]!="not sampled"]
#df[className]<-as.numeric(df[className])
dfa<-spec_chem_canopy
#dim(df)
dfa<-dfa %>% subset(dfa[className] != "not sampled")
dfa[className]<-as.numeric(dfa[[className]])

##Use block below for responses that are factors like vigor class
#dfa[className]<-as.factor(dfa[[className]])

spec_chem_canopy_df<-dfa[is.na(dfa[className])==F,] %>% 
#    subset(AVG_Fv_Fm != "not sampled") %>%
    mutate(#AVG_Fv_Fm = as.numeric(AVG_Fv_Fm), 
      Vigor_class = as.factor(Vigor_class), 
      GenusSpecies = as.factor(GenusSpecies)) %>%
    group_by(Site, TreeID, GenusSpecies) %>% #tally %>% dplyr::select(n) %>% ungroup() %>% summarise(min_pix= min(n), max_pix = max(n), median_pix = median(n))
    slice_sample(n =80, replace = F)
sum(is.na(spec_chem_canopy_df[className]))
  #Create a test and train split
  inTrain <- caret::createDataPartition(
    y = spec_chem_canopy_df[[className]],
    p = 0.7,
    list = FALSE#,
    #na.rm = TRUE
  )
training <- spec_chem_canopy_df[inTrain,]  %>% ungroup %>% dplyr::select(className, band_names)
testing <- spec_chem_canopy_df[-inTrain,]  %>% ungroup %>% dplyr::select(className, band_names)
    #Variable Importance RF model
    #n=10000
    #rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),
    #data = training,num.trees = n, importance = "impurity_corrected")
    #plot(sort(rf_mod$variable.importance))
    #Prediction RF model
    n=1000
    #print(paste("building ranger model for ",className,sep="")
    rf_mod <- ranger::ranger(as.formula(paste(className, "~.")),
    data = training,num.trees = n)
    rf_mod_pred<-predict(rf_mod, testing)
    
    ##Use block below for responses that are factors like vigor class
    #acc = round(conf_mat$overall[1],3)
    #cm<-conf_mat(conf_mat_plt, obs, pred)
    
    #jpeg(paste("output/models/figs/", vars[x], "cal_v_val_density.jpg"), width = 700, height = 600)
    #autoplot(cm, type = "heatmap") + 
    #  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")+
    #    labs(x = paste("Calibration  ", vars_pretty[x], sep=""), y = paste("Validation  ", vars_pretty[x], sep=""), title = paste(vars_pretty[x], "  Accuracy =  ",acc, sep="")) +
    #      theme(plot.title = element_text(hjust = 0.5, size =16, face="bold"),
    #        axis.text=element_text(size=14, face="bold"),
    #        axis.title=element_text(size=14,face="bold"))
#
    #    dev.off()  

    #rpd = round(2 * abs(testing[className] - rf_mod_pred$predictions) / (rf_mod_pred$predictions + testing[className]),2) 
    R2 = round(R2(rf_mod_pred$predictions, testing[[className]], formula = "corr"),2)
  
    CR = colorRampPalette((c("blue","green")))

    jpeg(paste("output/models/figs/", vars[x], "cal_v_val_density.jpg"), width = 700, height = 600)
    hex<-hexbin::hexbin(rf_mod_pred$predictions, testing[[className]], xbins = 30)
    plot(hex, 
        main = paste(vars_pretty[x], "  R2  =  ",R2, sep=""),
        xlab = paste("Calibration  ", vars_pretty[x], sep=""), 
        ylab = paste("Validation  ", vars_pretty[x], sep=""),
        colramp = CR,
        colorcut = 10)
        ;
    par(cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    dev.off()
    print(vars[x])
    })

