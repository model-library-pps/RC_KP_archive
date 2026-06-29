#Run settings for the Hanninghof dataset
#
#Authors: AGT Schut and W. Reymann
#         Plant Production Systems group 
#         Wageningen University, 2021

require(pracma)
require(reshape2)
source('RC_KP_model.R')
source('RC_KP_rotation.R')
source("12. Broadbalk figures.r")

#INPUT=============================================================================================
RotationFile = "..//Data//Processed//11. Broadbalk_rotation_NPK_offtakes_kgha.csv"
RotationFileLMpred = "..//Data//Processed//11. Broadbalk_rotation_NPK_offtakes_kgha_MLpredicted.csv"

#NPK or PK treatment needs to be the first in the list!!!
#Initial P pools are estimated from differences in measured uptakes in NK and NPK treatments
#Initial K pools are estimated from differences in measured uptakes in NP and NPK treatments
#The first treatment needs to include both K and P applications!!
#This to ensure that initial pools are derived from treatment with PK fertilization
parListNamesAll <- c(    "N1PKMg",    "N2PKMg","PKMg", "Control",    "N",   "NKMg",  "NP",  "NPK",    "N4PKMg", "N5PKMg","FYMNPK",   "FYM")
parListIsFYM    <- c(           0,           0,     0,         0,      0,        0,     0,      0,           0,       0,        1,       1)
cols            <- c("lightgreen",     "green","blue",     "red", "grey", "orange","cyan","green", "darkgreen", "magenta", "gold", "black")
pchs            <- c(           1,          16,    16,         1,      1,       16,      1,    16,          1,         16,     16,      17)
lwds            <- c(           2,           2,     2,         2,      2,        2,      2,     2,          2,          2,      2,       2)
ltys            <- c(           1,           2,     1,         1,      1,        1,      1,     1,          1,          1,      5,       5)

#Some key settings for this experiment. 1852 is the first year.
#Only wheat until 1968, after that a rotation started.
#
#In the first years of the rotation, K pools seems to be extremely high due to previous applications.
#The assumption of balance between pools INS and SOL in the first year seems invalid..

#1958 was first year with potato.
#1968-1979: Rotation was wheat, potato, beans, wheat, potato, beans
#1979-1986: Rotation was potato, wheat, fallow, potato, wheat, fallow
#1986-1998: Rotation was fallow, potato, wheat, wheat, wheat
#1999-2017: Rotation was oats, silage maize, wheat, wheat, wheat

SelectedSection <- 4 #Use only section 4 (5 works but has limited nr of years)
TESTYEARS <- seq(1968, 2016,1) #21 sequences of crop rotation including wheat, potato, beans
ParSET <- "LMpred_BBalk" #Options: "original_BBalk"   "LMpred_BBalk"
CROPS <- c("potato","wheat","beans","oats","silage maize")

#NKMg is only present in sections 0 and 1
parListNames <- c("N1PKMg",  "N2PKMg",  "N4PKMg", "N5PKMg", "PKMg", "Control",    "N",   "NP",  "NPK", "FYMNPK",   "FYM")

indexNames = NULL
for (pn in parListNames) {
  indexNames <- c(indexNames, which(parListNamesAll == pn))
}
parListIsFYM <- parListIsFYM[indexNames]
cols <- cols[indexNames]
pchs <- pchs[indexNames]
lwds <- lwds[indexNames]
ltys <- ltys[indexNames]
#INPUT=============================================================================================


#Get all required parameters
param        = RCKP_param(type = "TSP",
                          Crops = CROPS, 
                          CalSetName = ParSET, 
                          UseMeasuredNUptake = USE_MEASURED_UPTAKES )

#Read in pre-processed data on uptake
if (ParSET == "original_BBalk") {#original values
  Ydata <- read.csv(RotationFile, as.is = TRUE)
}else {
  #Read data with LM predictions added (without year effects)
  Ydata <- read.csv(RotationFileLMpred, as.is = TRUE)
}
#rename maize to silage maize
ii <- which(Ydata[,"Crop"] == "maize")
Ydata[ii,"Crop"] <- "silage maize"

#Change column name for Year to Year
cn <- colnames(Ydata)
cn[cn == "year"] <- "Year"
colnames(Ydata) <- cn

#Change column names for FYM+ 
Ydata[Ydata[,"Trt"] == "FYM+NPK","Trt"] <- "FYMNPK"

#Convert fresh to dry: t/ha to kg DM/ha
Ydata[ii,"Yield.kg.ha"] <- 1000 * Ydata[ii,"Yield.tDM.ha"]

#Estimate initial pools using section 1 with continuous wheat 
#For 1968 NKMg misses K uptake, so use 1969 instead
if (ParSET == "original_BBalk") { #orginal values
  UptWheat68P <- subset(Ydata, section == 1 & Year == 1969 & Trt == "NKMg", select = c("Crop","Trt","Year","P.offtake.kg.ha","Yield.t.ha")) 
  UptWheat68K <- subset(Ydata, section == 1 & Year == 1969 & Trt == "NP", select = c("Crop","Trt","Year","K.offtake.kg.ha","Yield.t.ha")) 
  Kupt_US = UptWheat68K[1,"K.offtake.kg.ha"]  #K uptake from unfertilized soil #3 (uptake in NP treatment)
  Pupt_US = UptWheat68P[1,"P.offtake.kg.ha"]
}else {#LM predicted values
  UptWheat68P <- subset(Ydata, section == 1 & Year == 1968 & Trt == "NKMg", 
                        select = c("Crop","Trt","Year","P.offtake.kg.ha","LMpred.P.offtake.kg.ha","Yield.t.ha")) 
  UptWheat68K <- subset(Ydata, section == 1 & Year == 1968 & Trt == "NP", 
                        select = c("Crop","Trt","Year","K.offtake.kg.ha","LMpred.K.offtake.kg.ha","Yield.t.ha")) 
  Kupt_US = UptWheat68K[1,"LMpred.K.offtake.kg.ha"]  #K uptake from unfertilized soil #3 (uptake in NP treatment)
  Pupt_US = UptWheat68P[1,"LMpred.P.offtake.kg.ha"]
}


#Use for now only section 4, which is mostly complete, to avoid complexity 
#due to more than 1 observation for a crop / year. E.g. wheat is grown 3 
#years in a rotation, so occurs on 3 sections in the same year.
if (ParSET == "original_BBalk") {#original values
  UptDataPK <- subset(Ydata, section == SelectedSection & Year >= min(TESTYEARS) & Year < max(TESTYEARS), 
                      select = c("Crop","Trt","Year","N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")) 
}else {#LM predicted values
  UptDataPK <- subset(Ydata, section == SelectedSection & Year >= min(TESTYEARS) & Year < max(TESTYEARS), 
                      select = c("Crop","Trt","Year","LMpred.N.offtake.kg.ha","LMpred.P.offtake.kg.ha","LMpred.K.offtake.kg.ha","Yield.t.ha")) 
}
colnames(UptDataPK) <- c("Crop","Trt","Year","N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")

UptN <- dcast(UptDataPK, Year + Crop  ~ Trt, value.var = c("N.offtake.kg.ha"))
UptP <- dcast(UptDataPK, Year + Crop  ~ Trt, value.var = c("P.offtake.kg.ha"))
UptK <- dcast(UptDataPK, Year + Crop  ~ Trt, value.var = c("K.offtake.kg.ha"))

  
  #Prepare list with parameter sets
  parList <- PrepareListParameterSets(parListNames = parListNames, CROPS = CROPS, 
                                      ParSET= ParSET, 
                                      Trt_PK = "PKMg",
                                      Ydata = subset(Ydata, section == SelectedSection & Year >= min(TESTYEARS) & Year < max(TESTYEARS)),
                                      UseMeasuredUptake = USE_MEASURED_UPTAKES)
  

  #Solve the model
  listStates <- RCKP_rotation_Broadbalk(parList = parList,
                                        Pupt_US = Pupt_US,
                                        Kupt_US = Kupt_US,
                                        YEARS = TESTYEARS)
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
  Write_Meas_Pred_NPK(ScriptNr =14,
                      Dataset = paste0("Broadbalk_",ParSET,"_"),
                      parListNames = parListNames,
                      listStates = listStates,
                      UptN = out$UptN,
                      UptP = out$UptP,
                      UptK = out$UptK,
                      cols = cols,
                      pchs = pchs)  
  
  
    
#PLOT RESULTS========================================================================================
# windows(width=15, height=10)
#   plot_firstyear_meas_vs_pred_PKuptake()
#   
# windows(width=15, height=10)
#   plot_state_results()
# 
# windows(width=15, height=10)
#   plot_meas_pred_uptake_time()

# windows(width=15, height=20)
#   plot_cumsum_PK_per_crop()

windows()
  plot_meas_vs_pred_NPKuptake()
  
#Save result in PDF
pdf(paste("..//Results//14. States and dynamics with measured vs predicted for parameter set ",ParSET, ".pdf"))
  plot_firstyear_meas_vs_pred_PKuptake()
  plot_state_results()
  plot_meas_pred_uptake_time()
  plot_meas_vs_pred_PKuptake()
  plot_meas_vs_pred_NPKuptake()
  plot_cumsum_PK()
  plot_cumsum_PK_per_crop()
  Visualise_effect_Mg_on_cumulative_uptake(UptP = out$UptP, UptK = out$UptK)
dev.off()

tiff("..//Results//18. Schut et al. Figure 5.tif",
     width = 12, height = 12, units = "cm",
     compression = c("lzw"),
     bg = "white", res = 600, pointsize = 8,
     restoreConsole = TRUE, family= "serif",
     type = "windows",
     symbolfamily="default")  
    
    plot_cumsum_PK()

dev.off()

#Check if calculated ratios are reasonable

pdf(paste("..//Results//14. Measured vs predicted uptake ratios for parameter set ",ParSET, ".pdf"))
  plot_ratio(ratio="KN")
  plot_ratio(ratio="PN")
  plot_ratio(ratio="NK")
  plot_ratio(ratio="PK")
  plot_ratio(ratio="NP")
  plot_ratio(ratio="KP")
dev.off()


#To check: if uptake is set, difference between predicted and actual uptake must be really small!!
pdf(paste("..//Results//14. Measured vs predicted N uptake for parameter set ",ParSET, ".pdf"))
  for (i in 1:length(parList)){
    if(parList[[i]]$Scenario == "PK"){
      plot_meas_vs_pred_Nuptake(out$UptN, col_meas_N_UP = "PK", col_pred_N_UP = "pred_N_UP_PK")
    }else if(parList[[i]]$Scenario == "PKMg"){
      plot_meas_vs_pred_Nuptake(out$UptN, col_meas_N_UP = "PKMg", col_pred_N_UP = "pred_N_UP_PKMg")
    }else if(parList[[i]]$Scenario == "FYM"){
      plot_meas_vs_pred_Nuptake(out$UptN, col_meas_N_UP = "FYM", col_pred_N_UP = "pred_N_UP_FYM")
    }
  }
dev.off()

pdf(paste("..//Results//14. Mass balances for parameter set ",ParSET, ".pdf"))
#Determine and show P and K mass balances
for (i in 1:length(parListNames)) {
  states_MB <- RCKP_MassBalance(iniStates = listStates[[i]][1,], States = listStates[[i]])
}
dev.off()


PoolChanges <- NULL
for (i in 1:length(parListNames)) {
  fyr <- subset(listStates[[i]], time == min(TESTYEARS), select=c("P_labile", "P_stable","P_labile_FYM", 
                                                                  "K_labile", "K_stable", "K_labile_FYM"))
  lyr <- subset(listStates[[i]], time == max(TESTYEARS), select=c("P_labile", "P_stable", "P_labile_FYM", 
                                                                  "K_labile", "K_stable", "K_labile_FYM"))
  diff <- round(lyr[1,] - fyr[1, ], 1)
  diff[,"Trt"] <- parListNames[i]
  PoolChanges <- rbind(PoolChanges,diff)
}
print(PoolChanges)
write.csv(PoolChanges, paste("..//Results//14. Estimated pool changes for parameter set ",ParSET, ".csv"))


for (i in 1:length(parListNames)) {
  write.csv(listStates[[i]], paste("..//Results//14. Estimated pools for treatment ", parListNames[i]," and parameter set ",ParSET, ".csv"))
}

write.csv(out$UptN, paste("..//Results//14. Estimated N uptake for parameter set ",ParSET, ".csv"))
write.csv(out$UptP, paste("..//Results//14. Estimated P uptake for parameter set ",ParSET, ".csv"))
write.csv(out$UptK, paste("..//Results//14. Estimated K uptake for parameter set ",ParSET, ".csv"))

