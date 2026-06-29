#Run settings for the Hanninghof dataset
#
#Authors: AGT Schut and W. Reymann
#         Plant Production Systems group 
#         Wageningen University, 2021

require(reshape2)
source("0. Hahnhof figures.r")
source('RC_KP_model.R')
source('RC_KP_rotation.R')

#INPUT=============================================================================================
RotationFile = "..//Data//Processed//1. HanningHof_rotation_NPK_offtakes_kgha.csv"
RotationFileLMpred = "..//Data//Processed//1. HanningHof_rotation_NPK_offtakes_kgha_MLpredicted.csv"

#NPK or PK treatment needs to be the first in the list!!!
#Initial P pools are estimated from measured uptakes in NK treatment
#Initial K pools are estimated from measured uptakes in NP treatment
#The first listed treatment needs to include both K and P applications!!
#This to ensure that initial pools are derived from treatment with PK fertilization
parListNamesAll <- c("NPK"  ,  "PK",     "NK", "Control",    "N",   "NP",  "NPKMg",  "FYMNPK",   "FYM",   "FYMPK",     "FYMNK",       "FYMNP",  "FYMNPKMg")
parListIsFYM    <- c(      0,     0,        0,       0,        0,      0,        0,         1,       1,         1,           1,           1,             1)
cols            <- c("green", "blue","orange",     "red", "grey", "cyan","magenta",    "gold", "black",     "blue",   "orange",      "cyan",   "goldenrod")
pchs            <- c(      1,     1,        1,       1,        1,      1,        1,         2,       2,          2,          2,           2,             2)
lwds            <- c(      2,     2,        2,       2,        2,      2,        2,         2,       2,          2,          2,           2,             2)
ltys            <- c(      1,     1,        1,       1,        1,      1,        1,         5,       5,          5,          5,           5,             5)

#Some key settings for this experiment. 1958 is first year
#In the first years, the K pools seems to be extremely high due to previous applications.
#The assumption of balance between pools INS and SOL in the first year seems invalid..
TESTYEARS <- seq(1961,2009,1) #21 sequences of crop rotation with potato : rye : oats
ParSET <- "LMpred_FYM_Hanninghof" #Options: "original_Hanninghof" "LMpred_Hanninghof"
CROPS <- c("potato","oats","rye")

parListNames <- c("NPK"  ,  "PK",     "NK", "Control",    "N",   "NP",  "NPKMg",
                  "FYMNPK",   "FYM",   "FYMPK",     "FYMNK",       "FYMNP",    "FYMNPKMg")


if(ParSET == "LMpred_Hanninghof"){
  parListNames <- c("NPK"  ,  "PK",     "NK", "Control",    "N",   "NP",  "NPKMg")
}

indexNames = NULL
for(pn in parListNames){
  indexNames <- c(indexNames, which(parListNamesAll == pn))
}
parListIsFYM <- parListIsFYM[indexNames]
cols <- cols[indexNames]
pchs <- pchs[indexNames]
lwds <- lwds[indexNames]
ltys <- ltys[indexNames]
#INPUT=============================================================================================


  
  #Read in pre-processed data on uptake
  #1958 was first year with potato.
  #Rotation was potato, rye, oats
  #In 2009 introduction of silage maize as part of the Crop rotation (before only potato, winter rye and)
  #After 2008 the rotation was: silage maize, rye, silage maize, rye, potato
  if(!is.null(strfind(ParSET,"original"))){#original values
    Ydata <- read.csv(RotationFile, as.is = TRUE)
  }else{
    #Read data with LM predictions added (without year effects)
    Ydata <- read.csv(RotationFileLMpred, as.is= TRUE)
  }
  #Use Trt as column name for treatments
  colnames(Ydata)[colnames(Ydata)=="Description"] <- "Trt"

  Ydata <- subset(Ydata, Year >= min(TESTYEARS) & Year < max(TESTYEARS))
  
  #Change treatment names for FYM+ 
  Ydata[Ydata[,"Trt"] == "FYM+N","Trt"] <- "FYMN"
  Ydata[Ydata[,"Trt"] == "FYM+PK","Trt"] <- "FYMPK"
  Ydata[Ydata[,"Trt"] == "FYM+NK","Trt"] <- "FYMNK"
  Ydata[Ydata[,"Trt"] == "FYM+NP","Trt"] <- "FYMNP"
  Ydata[Ydata[,"Trt"] == "FYM+NPK","Trt"] <- "FYMNPK"
  Ydata[Ydata[,"Trt"] == "FYM+NPKMg","Trt"] <- "FYMNPKMg"

  Ydata[Ydata[,"Trt"] == "FYM+N","Trt"] <- "FYMN"
  
  param        = RCKP_param(type = "TSP",Crops = CROPS, UseMeasuredNUptake = FALSE)
  
  #Convert fresh to dry: t/ha to kg DM/ha
  for(cr in param$CropQUEFTS_Dry.crop){
    ii <- which(Ydata[,"Crop"] == cr) #crop to select
    jj <- which(param$CropQUEFTS_Dry.crop == cr) #crop to select
    Ydata[ii,"Yield.kg.ha"] <- 1000 * Ydata[ii,"Yield.t.ha"] * param$CropQUEFTS_Dry.DMc[jj] 
  }
  #Add linear regression and predict yields
  fit <- lm(Yield.kg.ha ~  Crop * (Year  + Trt), Ydata)
  print(summary(fit))
  print(anova(fit))
  Ydata[,"LMpred.yield.kg.ha"] <- predict(fit,Ydata)
  
  
  if(!is.null(strfind(ParSET,"original"))){#original values
    UptDataPK <- subset(Ydata, Year >= min(TESTYEARS) & Year < max(TESTYEARS) , select = c("Crop","Trt","Year","N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")) 
  }else{  #LM predicted values
    UptDataPK <- subset(Ydata, Year >= min(TESTYEARS) & Year < max(TESTYEARS) , select = c("Crop","Trt","Year","LMpred.N.offtake.kg.ha","LMpred.P.offtake.kg.ha","LMpred.K.offtake.kg.ha","Yield.t.ha")) 
    colnames(UptDataPK) <- c("Crop","Trt","Year","N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")
  }
  
  UptN <- dcast(UptDataPK, Year + Crop  ~ Trt, value.var= c("N.offtake.kg.ha"))
  UptP <- dcast(UptDataPK, Year + Crop  ~ Trt, value.var= c("P.offtake.kg.ha"))
  UptK <- dcast(UptDataPK, Year + Crop  ~ Trt, value.var= c("K.offtake.kg.ha"))
  
  #Estimate P and K pools in the first year
  Kupt_US = UptK[1,"NP"]   #K uptake from unfertilized soil, uptake in NP treatment
  Pupt_US = UptP[1,"NK"]   #P uptake from unfertilized soil, uptake in NK treatment
  
 
  #Prepare list with parameter sets
  parList <- PrepareListParameterSets(parListNames = parListNames, CROPS = CROPS, 
                                      ParSET= ParSET, 
                                      Trt_PK = "PK",
                                      Ydata = Ydata,
                                      UseMeasuredUptake = USE_MEASURED_UPTAKES)
  
    
  #Call this function for all the computations!
  listStates <- RCKP_rotation_Hanninghof(parList = parList,
                                         Pupt_US = Pupt_US,
                                         Kupt_US = Kupt_US,
                                         YEARS = c(TESTYEARS, 2009)) #add 2009 to run through 2008!
  #Call this function for all the computations!
  out <- Determine_PK_errors(parList = parList,
                             listStates = listStates,
                             UptN = UptN,
                             UptP = UptP,
                             UptK = UptK,
                             ShowErrors = TRUE)  
  print(c(N_NSE = out$N_NSE, P_NSE = out$P_NSE, K_NSE = out$K_NSE))
  print(c(RMSE_N = out$RMSE_N, RMSE_P = out$RMSE_P, RMSE_K = out$RMSE_K))
  
  #Write results to file
  #Save results in file
  Write_Meas_Pred_NPK(ScriptNr =4,
                      Dataset = paste0("Hanninghof_",ParSET,"_"),
                      parListNames = parListNames,
                      listStates = listStates,
                      UptN = out$UptN,
                      UptP = out$UptP,
                      UptK = out$UptK,
                      cols = cols,
                      pchs = pchs)  
  
#To test mass balances
#Run with step <1, e.g. 0.1
#bal<-RCKP_MassBalance(iniStates = listStates[[2]][1,], States=listStates[[2]])
#plot(bal[,"time"], bal[,"MB_P"], col="black")  
#plot(bal[,"time"], bal[,"MB_K"], col="red")    
  PoolChanges <- NULL
  for (i in 1:length(parListNames)) {
    fyr <- subset(listStates[[i]], time == min(TESTYEARS), select=c("P_labile", "P_stable","P_labile_FYM", 
                                                                    "K_labile", "K_stable", "K_labile_FYM"))
    lyr <- subset(listStates[[i]], time == max(TESTYEARS), select=c("P_labile", "P_stable", "P_labile_FYM", 
                                                                    "K_labile", "K_stable", "K_labile_FYM"))
    diff <- round(lyr[1,] - fyr[1,], 1)
    diff[,"Trt"] <- parListNames[i]
    PoolChanges <- rbind(PoolChanges,diff)
  }
  print(PoolChanges)
  write.csv(PoolChanges, paste("..//Results//3. Estimated pool changes for parameter set ",ParSET, ".csv"))
  
  for (i in 1:length(parListNames)) {
    write.csv(listStates[[i]], paste("..//Results//3. Estimated pools for treatment ", parListNames[i]," and parameter set ",ParSET, ".csv"))
  }
  
  write.csv(out$UptN, paste("..//Results//3. Estimated N uptake for parameter set ",ParSET, ".csv"))
  write.csv(out$UptP, paste("..//Results//3. Estimated P uptake for parameter set ",ParSET, ".csv"))
  write.csv(out$UptK, paste("..//Results//3. Estimated K uptake for parameter set ",ParSET, ".csv"))
  
  
#PLOT RESULTS========================================================================================
# windows(width=15, height=10)
#   plot_firstyear_meas_vs_pred_PKuptake()
#   
#   windows(width=15, height=10)
#   plot_state_results()
# 
# windows(width=15, height=10)
#   plot_meas_pred_uptake_time()

windows()
  plot_meas_vs_pred_NPKuptake()

# windows(width=15, height=20)
#   plot_cumsum_PK_per_crop()

#Save result in PDF
pdf(paste("..//Results//3. States and dynamics with measured vs predicted for parameter set ",ParSET, ".pdf"))
  plot_state_results()
  plot_meas_pred_uptake_time()
  plot_meas_vs_pred_PKuptake()
  plot_meas_vs_pred_PKuptake(ShowLegend = FALSE)
  plot_meas_vs_pred_NPKuptake()
  plot_cumsum_PK()
  plot_cumsum_PK(ShowLegend = FALSE)
  plot_cumsum_PK_per_crop()
  Visualise_effect_Mg_on_cumulative_uptake(UptP = out$UptP, UptK = out$UptK)
dev.off()

tiff("..//Results//18. Schut et al. Figure 4.tif",
     width = 12, height = 12, units = "cm",
     compression = c("lzw"),
     bg = "white", res = 600, pointsize = 8,
     restoreConsole = TRUE, family= "serif",
     type = "windows",
     symbolfamily="default")  

plot_cumsum_PK()

dev.off()


pdf(paste("..//Results//3. Measured vs predicted uptake ratios for parameter set ",ParSET, ".pdf"))
  plot_ratio(ratio="KN")
  plot_ratio(ratio="PN")
  plot_ratio(ratio="NK")
  plot_ratio(ratio="PK")
  plot_ratio(ratio="NP")
  plot_ratio(ratio="KP")
dev.off()


pdf(paste("..//Results//3. Mass balances for parameter set ",ParSET, ".pdf"))
  #Determine and show P and K mass balances
  for (i in 1:length(parListNames)) {
    states_MB <- RCKP_MassBalance(iniStates = listStates[[i]][1,], States = listStates[[i]])
  }
dev.off()
