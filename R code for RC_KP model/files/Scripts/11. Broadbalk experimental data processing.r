require(openxlsx)
require(lme4)
require(lmerTest)
source("12. Broadbalk figures.r")

#Broadbalk started in 1843 with continuous wheat cropping, 
#comparing C with FYM and fertilizer as factors in strips. Only yield components and soil data were recorded. 
#Since 1926, the setup included 5 "sections" comparing continuous wheat with wheat and fallow. 
#Strips with treatments remained placed orthogonal to these sections.
#In 1968, the experiment was split into 10 sections. Sections 0 and 1 continued with wheat. 
#In sections 2-9 crop rotations were introduced. Treatments changed strongly.
#The key treatments for RC-KP to initialise pools (including NK, PK, NP and NPK with or without Mg),
#are only all present in the wheat section. NKMg is not included in sections 2-9.



#INPUT
filename <- "..//Data//Broadbalk_trt.xlsx"
Plot = read.xlsx(filename,sheet = "Plot")
Fert = read.xlsx(filename,sheet = "Fertilizers")
FYM = read.xlsx(filename,sheet = "FYM")

PotatoYield = read.delim("..//Data//Broadbalk//PotatoYieldAll.txt", header = TRUE, sep = ",", skip = 0)
WheatYield = read.delim("..//Data//Broadbalk//WheatAll.txt", header = TRUE, sep = ",", skip = 0)
OatsYield = read.delim("..//Data//Broadbalk//OatsYieldAll.txt", header = TRUE, sep = ",", skip = 0)
BeansYield = read.delim("..//Data//Broadbalk//BeansYieldAll.txt", header = TRUE, sep = ",", skip = 0)
MaizeYield = read.delim("..//Data//Broadbalk//MaizeAll.txt", header = TRUE, sep = ",", skip = 0)


PotatoNutrients = read.delim("..//Data//Broadbalk//PotatoNutrientsAll.txt", header = TRUE, sep = ",", skip = 0)
#WheatNutrients = read.delim("..//Data//Broadbalk//WheatNutrientsAll.txt", header = TRUE, sep = ",", skip = 0)
WheatNutrients = read.delim("..//Data//Broadbalk//WheatNutrientsAll - Corrected22NOV2022.txt", header = TRUE, sep = ",", skip = 0)
OatsNutrients = read.delim("..//Data//Broadbalk//OatsNutrientsAll.txt", header = TRUE, sep = ",", skip = 0)
BeansNutrients = read.delim("..//Data//Broadbalk//BeansNutrientsAll.txt", header = TRUE, sep = ",", skip = 0)
MaizeNutrients = read.delim("..//Data//Broadbalk//MaizeNutrientsAll.txt", header = TRUE, sep = ",", skip = 0)

#Nutrient content data for P in section 2, wheat straw are incorrect. 
#See email from Res e-RA <res.e-ra@rothamsted.ac.uk>, received on 2 Nov 2022.
#A couple of errors in the Broadbalk crop nutrient data have been brought to my attention:
#  1)	2017 - plot numbers are incorrect in the database - the are complete rubbish!
#  3)	2004 - minor errors in the % Na values, due to high blanks giving some negative values. These should be excluded. 
WheatNutrients[which(WheatNutrients[,"year"] == 2017), c("k_gr", "k_st", "n_gr", "n_st", "p_gr", "p_st")] <- NA
OatsNutrients[ which(OatsNutrients[,"year"]  == 2017), c("k_gr", "k_st", "n_gr", "n_st", "p_gr", "p_st")] <- NA
BeansNutrients[which(BeansNutrients[,"year"] == 2017), c("k_gr", "k_st", "n_gr", "n_st", "p_gr", "p_st")] <- NA
MaizeNutrients[which(MaizeNutrients[,"year"] == 2017), c("k_gr", "k_st", "n_gr", "n_st", "p_gr", "p_st")] <- NA

#OUTPUT FILENAMES
PotatoFileOut = "..//Data//Processed//11. Broadbalk_potato_NPK_offtakes_kgha.csv"
OatsFileOut = "..//Data//Processed//11. Broadbalk_oats_NPK_offtakes_kgha.csv"
BeansFileOut = "..//Data//Processed//11. Broadbalk_beans_NPK_offtakes_kgha.csv"
WheatFileOut = "..//Data//Processed//11. Broadbalk_wheat_NPK_offtakes_kgha.csv"
MaizeFileOut = "..//Data//Processed//11. Broadbalk_maize_NPK_offtakes_kgha.csv"
RotationFileOut = "..//Data//Processed//11. Broadbalk_rotation_NPK_offtakes_kgha.csv"
RotationFileOutLMpred = "..//Data//Processed//11. Broadbalk_rotation_NPK_offtakes_kgha_MLpredicted.csv"

MeasuredOfftakesPerYearPotato = "..//Results//11. HanningHof_measured_offtakes_potato_kgha.pdf"
MeasuredOfftakesPerYearBeans = "..//Results//11. Broadbalk_measured_offtakes_beans_kgha.pdf"
MeasuredOfftakesPerYearOats = "..//Results//11. Broadbalk_measured_offtakes_oats_kgha.pdf"
MeasuredOfftakesPerYearWheat = "..//Results//11. Broadbalk_measured_offtakes_wheat_kgha.pdf"
MeasuredOfftakesPerYearMaize = "..//Results//11. Broadbalk_measured_offtakes_maize_kgha.pdf"
PredictedOfftakesPerYear = "..//Results//11. Broadbalk_LMpredicted_offtakes_rotation_kgha.pdf"
CumPredictedOfftakes = "..//Results//11. Broadbalk_cumulative_LMpredicted_offtakes_rotation_kgha.pdf"
MeasuredRecoveriesPerYear = "..//Results//11. Broadbalk_measured_recoveries.pdf"
PredictedRecoveriesPerYear = "..//Results//11. Broadbalk_LMpredicted_recoveries.pdf"
CumUptakeRecoveryPerYear = "..//Results//11. Broadbalk_cumulative_LMpredicted_uptakes_recoveries_peryear.pdf"

#FUNCTIONS
PrepareCrop <- function(DataYield, DataNutrients, Plotdetails, FYMdetails,
                        Crop, col_fw_tha, col_dmc_perc,
                        col_N_perc = NA, col_P_perc = NA, col_K_perc = NA,
                        col_straw_fw_tha = NA, col_straw_dmc_perc = NA,
                        col_N_perc_grain = NA, col_P_perc_grain = NA, col_K_perc_grain = NA,
                        col_N_perc_straw = NA, col_P_perc_straw = NA, col_K_perc_straw = NA){
  
  #Add rows with NA for nutrients if not measured in the section for a plot to prevent that it is lost with merging
  dfNA <- DataNutrients[1,] 
  dfNA[1,] <- NA
  for(i in 1:nrow(DataYield)){
    ii <- which(DataNutrients[,"plot"] == DataYield[i,"plot"] 
              & DataNutrients[,"section"] == DataYield[i,"section"]
              & DataNutrients[,"year"] == DataYield[i,"year"])
    if(length(ii)==0){
      dfna <- dfNA
      dfna[,"plot"] <- DataYield[i,"plot"]
      dfna[,"section"] <- DataYield[i,"section"]
      dfna[,"year"] <- DataYield[i,"year"]
      DataNutrients <- rbind(DataNutrients, dfna)
    }
  }
  Data <- merge(subset(DataYield, !is.na(plot)), 
                subset(DataNutrients, !is.na(plot)), by = c("plot","section","year"))
  Data[,"Crop"] <- Crop
  Data[,"Year"] <- Data[,"year"]
  #Convert factors to numerics
  Data[,"Yield.t.ha"] <- as.numeric(as.character(Data[,col_fw_tha]))
  Data[,"Yield.tDM.ha"] <- as.numeric(as.character(Data[,col_fw_tha])) * 0.01 * as.numeric(as.character(Data[,col_dmc_perc]))
  if(!is.na(col_straw_fw_tha)){
    Data[,"Straw.t.ha"] <- as.numeric(as.character(Data[,col_straw_fw_tha]))
    Data[,"Straw.tDM.ha"] <- as.numeric(as.character(Data[,col_straw_fw_tha])) * 0.01 * as.numeric(as.character(Data[,col_straw_dmc_perc]))
  }else{
    Data[,"Straw.t.ha"] <- NA
    Data[,"Straw.tDM.ha"] <- NA
  }
  if (!is.na(col_N_perc)) {
    Data[,"N.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_N_perc])) * 1000 * Data[,"Yield.tDM.ha"]
  }else if (!is.na(col_N_perc_grain) & !is.na(col_N_perc_straw)) {
    Data[,"N.offtake.kg.ha"] <- (0.01 * as.numeric(as.character(Data[,col_N_perc_grain])) * 1000 * Data[,"Yield.tDM.ha"]
                                + 0.01 * as.numeric(as.character(Data[,col_N_perc_straw])) * 1000 * Data[,"Straw.tDM.ha"])
  }
  Data[,"Grain.N.offtake.kg.ha"] <- NA
  if (!is.na(col_N_perc_grain)) {
    Data[,"Grain.N.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_N_perc_grain])) * 1000 * Data[,"Yield.tDM.ha"]
  }
  Data[,"Straw.N.offtake.kg.ha"] <- NA
  if (!is.na(col_N_perc_straw)) {
    Data[,"Straw.N.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_N_perc_straw])) * 1000 * Data[,"Straw.tDM.ha"]
  }
  
  if (!is.na(col_P_perc)) {
    Data[,"P.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_P_perc])) * 1000 * Data[,"Yield.tDM.ha"]
  }else if (!is.na(col_P_perc_grain) & !is.na(col_P_perc_straw)) {
    Data[,"P.offtake.kg.ha"] <- (0.01 * as.numeric(as.character(Data[,col_P_perc_grain])) * 1000 * Data[,"Yield.tDM.ha"]
                                + 0.01 * as.numeric(as.character(Data[,col_P_perc_straw])) * 1000 * Data[,"Straw.tDM.ha"])
  }
  Data[,"Grain.P.offtake.kg.ha"] <- NA
  if (!is.na(col_P_perc_grain)) {
    Data[,"Grain.P.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_P_perc_grain])) * 1000 * Data[,"Yield.tDM.ha"]
  }
  Data[,"Straw.P.offtake.kg.ha"] <- NA
  if (!is.na(col_P_perc_straw)) {
    Data[,"Straw.P.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_P_perc_straw])) * 1000 * Data[,"Straw.tDM.ha"]
  }
  
  if (!is.na(col_K_perc)) {
    Data[,"K.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_K_perc])) * 1000 * Data[,"Yield.tDM.ha"]
  }else if (!is.na(col_K_perc_grain) & !is.na(col_K_perc_straw)) {
    Data[,"K.offtake.kg.ha"] <- (0.01 * as.numeric(as.character(Data[,col_K_perc_grain])) * 1000 * Data[,"Yield.tDM.ha"]
                                + 0.01 * as.numeric(as.character(Data[,col_K_perc_straw])) * 1000 * Data[,"Straw.tDM.ha"])
  }
  
  Data[,"Grain.K.offtake.kg.ha"] <- NA
  if (!is.na(col_K_perc_grain)) {
    Data[,"Grain.K.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_K_perc_grain])) * 1000 * Data[,"Yield.tDM.ha"]
  }
  Data[,"Straw.K.offtake.kg.ha"] <- NA
  if (!is.na(col_K_perc_straw)) {
    Data[,"Straw.K.offtake.kg.ha"] <- 0.01 * as.numeric(as.character(Data[,col_K_perc_straw])) * 1000 * Data[,"Straw.tDM.ha"]
  }  
  
  Data <- subset(Data, select = c("Year", "section", "plot", "Crop",
                                  "Yield.t.ha", "Yield.tDM.ha", "Straw.t.ha", "Straw.tDM.ha",
                                  "N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha",
                                  "Grain.N.offtake.kg.ha","Grain.P.offtake.kg.ha","Grain.K.offtake.kg.ha",
                                  "Straw.N.offtake.kg.ha","Straw.P.offtake.kg.ha","Straw.K.offtake.kg.ha"))
  #Add treatment information
  Data <- getTreatment(plot = Plotdetails, cropDF = Data)
  Data <- getManure(FYM = FYMdetails, cropDF = Data)

  return(Data)
}

getTreatment <- function(plot, cropDF){
  cropDF[,"Trt"] <- NA
  for(i in 1:nrow(cropDF)) {
    ii <- which(plot[,"First.year"] <= cropDF[i,"Year"] 
                & plot[,"Last.year"] >= cropDF[i,"Year"]
                & trimws(plot[,"plot"]) == trimws(cropDF[i,"plot"]))
    if(length(ii) == 1) {
      cropDF[i,"Treatment"] <- plot[ii,"Treatment"]
      cropDF[i,"Trt"] <- plot[ii,"Trt"]
      cropDF[i,"N.rate.kg.ha"] <- plot[ii,"N.kg.ha"]
      cropDF[i,"P.rate.kg.ha"] <- plot[ii,"P.kg.ha"]
      cropDF[i,"K.rate.kg.ha"] <- plot[ii,"K.kg.ha"]
      cropDF[i,"FYM.tFM.ha"] <- plot[ii,"FYM.tFM.ha"]
#    }else{
#      print(paste0("Missing treatment: ", cropDF[i,"Year"], " ", cropDF[i,"plot"]))
    }
  }
  #Filter out all rows where Trt was not found
  cropDF <- cropDF[which(!is.na(cropDF[,"Trt"])),]

  return(cropDF)
}
getManure <- function(FYM, cropDF){
  meanFYMN <- mean(as.numeric(FYM[,"gN.100gDM"]), na.rm=TRUE)
  meanFYMP <- mean(as.numeric(FYM[,"gP.100gDM"]), na.rm=TRUE)
  meanFYMK <- mean(as.numeric(FYM[,"gK.100gDM"]), na.rm=TRUE)
  meanFYMDM <- mean(as.numeric(FYM[,"DM.%"]), na.rm=TRUE)
  for(i in 1:nrow(cropDF)){
    #use mean concentrations if not measured or available
    FYMN <- meanFYMN
    FYMP <- meanFYMP
    FYMK <- meanFYMK
    FYMDM <- meanFYMDM
    ii <- which(FYM[,"Year"] == cropDF[i,"Year"])
    if(length(ii) == 1){
      if(!is.na(as.numeric(FYM[ii,"gN.100gDM"]))){
        FYMN <- as.numeric(FYM[ii,"gN.100gDM"])
      }
    }
    if(length(ii) == 1) {
      if(!is.na(as.numeric(FYM[ii,"gP.100gDM"]))){
        FYMP <- as.numeric(FYM[ii,"gP.100gDM"])
      }
    }
    if(length(ii) == 1){
      if(!is.na(as.numeric(FYM[ii,"gK.100gDM"]))){
        FYMK <- as.numeric(FYM[ii,"gK.100gDM"])
      }
    }
    if(length(ii) == 1){
      if(!is.na(as.numeric(FYM[ii,"DM.%"]))){
        FYMDM <- as.numeric(FYM[ii,"DM.%"])
      }
    }
    cropDF[i,"FYMN.rate.kg.ha"] <- 1000 * as.numeric(cropDF[i,"FYM.tFM.ha"]) * 0.01 * FYMDM * 0.01 * FYMN
    cropDF[i,"FYMP.rate.kg.ha"] <- 1000 * as.numeric(cropDF[i,"FYM.tFM.ha"]) * 0.01 * FYMDM * 0.01 * FYMP
    cropDF[i,"FYMK.rate.kg.ha"] <- 1000 * as.numeric(cropDF[i,"FYM.tFM.ha"]) * 0.01 * FYMDM * 0.01 * FYMK
  }
  return(cropDF)
}

#Merge plot with fertilizer and FYM info
PlotFert <- merge(Plot, Fert, by = "Treatment")

#Prepare crops
Potato <- PrepareCrop(Plotdetails = PlotFert, FYMdetails = FYM,
                      Crop = "potato", 
                      DataYield = PotatoYield, 
                      col_fw_tha = "fw_tubers", col_dmc_perc = "dmtubers",
                      DataNutrients = PotatoNutrients,
                      col_N_perc = "n_tu", col_P_perc = "p_tu", col_K_perc = "k_tu")

Maize <- PrepareCrop(Plotdetails = PlotFert, FYMdetails = FYM,
                      Crop = "maize", 
                      DataYield = MaizeYield, 
                      col_fw_tha = "total_yield", col_dmc_perc = "dry_matter",
                      DataNutrients = MaizeNutrients,
                      col_N_perc = "n", col_P_perc = "p", col_K_perc = "k")

Wheat <- PrepareCrop(Plotdetails = PlotFert, FYMdetails = FYM,
                     Crop = "wheat", 
                     DataYield = WheatYield, 
                     col_fw_tha = "totalgrain85", col_dmc_perc = "dmgrain",
                     col_straw_fw_tha = "totalstraw85", col_straw_dmc_perc = "dmstraw",
                     DataNutrients = WheatNutrients,
                     col_N_perc_grain = "n_grain", col_P_perc_grain = "p_grain", col_K_perc_grain = "k_grain",
                     col_N_perc_straw = "n_straw", col_P_perc_straw = "p_straw", col_K_perc_straw = "k_straw")
#Estimate straw uptake and test accuracy of estimated uptakes

estimate_missing_offtakes <- function(Wheat, indexCal, indexVal = NULL){
  fitN <- lm(Straw.N.offtake.kg.ha ~ Grain.N.offtake.kg.ha + Grain.P.offtake.kg.ha +Grain.K.offtake.kg.ha 
             + Yield.tDM.ha + Year*N.rate.kg.ha + P.rate.kg.ha + K.rate.kg.ha + Year*Trt, Wheat[indexCal,])
  print(summary(fitN))
  print(anova(fitN))
  fitP <- lm(Straw.P.offtake.kg.ha ~ Grain.N.offtake.kg.ha + Grain.P.offtake.kg.ha +Grain.K.offtake.kg.ha
             + Yield.tDM.ha + Year*N.rate.kg.ha + P.rate.kg.ha + K.rate.kg.ha + Year*Trt, Wheat[indexCal,])
  print(summary(fitP))
  print(anova(fitP))
  fitK <- lm(Straw.K.offtake.kg.ha ~ Grain.N.offtake.kg.ha + Grain.P.offtake.kg.ha +Grain.K.offtake.kg.ha
             + Yield.tDM.ha + Year*N.rate.kg.ha + P.rate.kg.ha + K.rate.kg.ha + Year*Trt, Wheat[indexCal,])
  print(summary(fitK))
  print(anova(fitK))
  RMSEC<-c(RMSEC_N_Offtake_straw = sqrt(mean( fitN$residuals^2)),
           RMSEC_P_Offtake_straw = sqrt(mean( fitP$residuals^2)),
           RMSEP_K_Offtake_straw = sqrt(mean( fitK$residuals^2)))
  print(RMSEC) 
  
  if(!is.null(indexVal)){
    dfVal <- data.frame(Grain.N.offtake.kg.ha = Wheat[indexVal,"Grain.N.offtake.kg.ha"],
                        Grain.P.offtake.kg.ha = Wheat[indexVal,"Grain.P.offtake.kg.ha"],
                        Grain.K.offtake.kg.ha = Wheat[indexVal,"Grain.K.offtake.kg.ha"],
                        Straw.N.offtake.kg.ha = Wheat[indexVal,"Straw.N.offtake.kg.ha"],
                        Straw.P.offtake.kg.ha = Wheat[indexVal,"Straw.P.offtake.kg.ha"],
                        Straw.K.offtake.kg.ha = Wheat[indexVal,"Straw.K.offtake.kg.ha"],
                        pStraw.N.offtake.kg.ha = predict(fitN, Wheat[indexVal,]),
                        pStraw.P.offtake.kg.ha = predict(fitP, Wheat[indexVal,]),
                        pStraw.K.offtake.kg.ha = predict(fitK, Wheat[indexVal,]),
                        N.offtake.kg.ha = Wheat[indexVal,"N.offtake.kg.ha"],
                        P.offtake.kg.ha = Wheat[indexVal,"P.offtake.kg.ha"],
                        K.offtake.kg.ha = Wheat[indexVal,"K.offtake.kg.ha"])
    dfVal[,"pN.offtake.kg.ha"] <- dfVal[,"Grain.N.offtake.kg.ha"] + dfVal[,"pStraw.N.offtake.kg.ha"]
    dfVal[,"pP.offtake.kg.ha"] <- dfVal[,"Grain.P.offtake.kg.ha"] + dfVal[,"pStraw.P.offtake.kg.ha"]
    dfVal[,"pK.offtake.kg.ha"] <- dfVal[,"Grain.K.offtake.kg.ha"] + dfVal[,"pStraw.K.offtake.kg.ha"]
    
    par(mfrow=c(2,3),mar=c(2,2,1,1), oma=c(2,2,2,0.5))
    plot(dfVal[,"Straw.N.offtake.kg.ha"], dfVal[,"pStraw.N.offtake.kg.ha"], main="N")
    abline(a=0,b=1,col="red")
    plot(dfVal[,"Straw.P.offtake.kg.ha"], dfVal[,"pStraw.P.offtake.kg.ha"], main="P")
    abline(a=0,b=1,col="red")
    plot(dfVal[,"Straw.K.offtake.kg.ha"], dfVal[,"pStraw.K.offtake.kg.ha"], main="K")
    abline(a=0,b=1,col="red")
    RMSEP<-c(RMSEP_N_Offtake_straw = sqrt(mean( (dfVal[,"Straw.N.offtake.kg.ha"] - dfVal[,"pStraw.N.offtake.kg.ha"])^2, na.rm=TRUE)),
             RMSEP_P_Offtake_straw = sqrt(mean( (dfVal[,"Straw.P.offtake.kg.ha"] - dfVal[,"pStraw.P.offtake.kg.ha"])^2, na.rm=TRUE)),
             RMSEP_K_Offtake_straw = sqrt(mean( (dfVal[,"Straw.K.offtake.kg.ha"] - dfVal[,"pStraw.K.offtake.kg.ha"])^2, na.rm=TRUE)))
    print("Validation for straw offtake")
    print(RMSEP) 
    plot(dfVal[,"N.offtake.kg.ha"], dfVal[,"pN.offtake.kg.ha"], main="N")
    abline(a=0,b=1,col="red")
    plot(dfVal[,"P.offtake.kg.ha"], dfVal[,"pP.offtake.kg.ha"], main="P")
    abline(a=0,b=1,col="red")
    plot(dfVal[,"K.offtake.kg.ha"], dfVal[,"pK.offtake.kg.ha"], main="K")
    abline(a=0,b=1,col="red")
    mtext("Measured straw or total offtakes, kg/ha", side = 1, line=0, outer = TRUE)
    mtext("Predicted straw offtakes, kg/ha", side = 2, line=0, outer = TRUE, adj=0.85)
    mtext("Predicted offtakes, kg/ha", side = 2, line=0, outer = TRUE, adj=0.15)
  }else{
    #Predict only for missing data
    ii <- is.na(Wheat[indexCal,"Straw.N.offtake.kg.ha"])
    indexReplace = indexCal[ii]
    Wheat[indexReplace,"Straw.N.offtake.kg.ha"] = predict(fitN, Wheat[indexReplace,])
    Wheat[indexReplace,"Straw.P.offtake.kg.ha"] = predict(fitP, Wheat[indexReplace,])
    Wheat[indexReplace,"Straw.K.offtake.kg.ha"] = predict(fitK, Wheat[indexReplace,])
    Wheat[indexReplace,"N.offtake.kg.ha"] <- Wheat[indexReplace,"Grain.N.offtake.kg.ha"] + Wheat[indexReplace,"Straw.N.offtake.kg.ha"]
    Wheat[indexReplace,"P.offtake.kg.ha"] <- Wheat[indexReplace,"Grain.P.offtake.kg.ha"] + Wheat[indexReplace,"Straw.P.offtake.kg.ha"]
    Wheat[indexReplace,"K.offtake.kg.ha"] <- Wheat[indexReplace,"Grain.K.offtake.kg.ha"] + Wheat[indexReplace,"Straw.K.offtake.kg.ha"]

    return(Wheat)
  }
}

#Straw for wheat in rotational sections 2, 4, 7 not always measured
Wheat[,"section"]<-as.factor(Wheat[,"section"])
estimate_missing_offtakes(Wheat, 
                  indexCal = which(Wheat[,"section"]== 2 | Wheat[,"section"]== 7), 
                  indexVal = which(Wheat[,"section"]== 4))
#Now estimate missing values for plots where grain yield was measured!
Wheat <- estimate_missing_offtakes(Wheat, 
                           indexCal = which(Wheat[,"section"]== 2 | Wheat[,"section"]== 4 | Wheat[,"section"]== 7), 
                           indexVal = NULL)

Oats <- PrepareCrop(Plotdetails = PlotFert, FYMdetails = FYM,
                     Crop = "oats", 
                     DataYield = OatsYield, 
                     col_fw_tha = "total_grain", col_dmc_perc = "dmgrain",
                     col_straw_fw_tha = "total_straw", col_straw_dmc_perc = "dmstraw",
                     DataNutrients = OatsNutrients,
                     col_N_perc_grain = "n_gr", col_P_perc_grain = "p_gr", col_K_perc_grain = "k_gr",
                     col_N_perc_straw = "n_st", col_P_perc_straw = "p_st", col_K_perc_straw = "k_st")
Beans <- PrepareCrop(Plotdetails = PlotFert, FYMdetails = FYM,
                    Crop = "beans", 
                    DataYield = BeansYield, 
                    col_fw_tha = "total_grain", col_dmc_perc = "dmgrain",
                    col_straw_fw_tha = "total_straw", col_straw_dmc_perc = "dmstraw",
                    DataNutrients = BeansNutrients,
                    col_N_perc_grain = "n_gr", col_P_perc_grain = "p_gr", col_K_perc_grain = "k_gr",
                    col_N_perc_straw = "n_st", col_P_perc_straw = "p_st", col_K_perc_straw = "k_st")


#Oats:
#No N or FYM to oats, 1996-2017 according to information
#However, uptakes are really high (>100 kg N/ha), suggesting that normal N + FYM was applied!
ii<-which(Oats[, "Year"] >= 1996 & Oats[, "Year"] <= 2017)
#Oats[ii,"N.rate.kg.ha"] <- 0
#Oats[ii,"FYMN.rate.kg.ha"] <- 0
#Oats[ii,"FYMP.rate.kg.ha"] <- 0
#Oats[ii,"FYMK.rate.kg.ha"] <- 0
#From 2018 N to oats at ½ rate, as a single application (mid-April)
ii<-which(Oats[, "Year"] >= 2018)
Oats[ii,"N.rate.kg.ha"] <- 0.5 * Oats[ii,"N.rate.kg.ha"]
Oats[ii,"FYMN.rate.kg.ha"] <- 0
Oats[ii,"FYMP.rate.kg.ha"] <- 0
Oats[ii,"FYMK.rate.kg.ha"] <- 0
#No N or FYM to beans from 2018
#Up to 2018, beans received FYM and NPK at the same rate as wheat and potatoes
ii<-which(Beans[, "Year"] >= 2018)
Beans[ii,"N.rate.kg.ha"] <- 0
Beans[ii,"FYMN.rate.kg.ha"] <- 0
Beans[ii,"FYMP.rate.kg.ha"] <- 0
Beans[ii,"FYMK.rate.kg.ha"] <- 0



Rotation <- rbind(Potato, Oats, Wheat, Beans, Maize)
ssR <- sort(Rotation[,"Year"], index.return = TRUE)
sortRotation <- Rotation[ssR$ix,]

#Add indicator for the fertilizer regime periods
sortRotation[, "FertRegime"] <- "A"
sortRotation[sortRotation[,"Year"] >= 1968, "FertRegime"] <- "B"
sortRotation[sortRotation[,"Year"] >= 1986, "FertRegime"] <- "C"
sortRotation[sortRotation[,"Year"] >= 2001, "FertRegime"] <- "D"
sortRotation[sortRotation[,"Year"] >= 2002, "FertRegime"] <- "E"
sortRotation[sortRotation[,"Year"] >= 2007, "FertRegime"] <- "F"
#Set it to a factor
sortRotation[, "FertRegime"] <- as.factor(sortRotation[, "FertRegime"])

#Apparent recovery per year (one crop per year only) and treatment
#There are many sections on the field, each with plots with the same treatment but a different "Year"of the rotation
#Contents are determined for one of these sections, so not all.
sortRotation[,"rec_N"] <- NA
sortRotation[,"rec_P"] <- NA
sortRotation[,"rec_K"] <- NA
sortRotation[,"rec_FYMN"] <- NA
sortRotation[,"rec_FYMP"] <- NA
sortRotation[,"rec_FYMK"] <- NA
for (year in unique(sortRotation[,"Year"])){
  #Use N2 for N recovery, higher value than for N5...
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "PKMg")
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "N2PKMg")
  sortRotation[jj,"rec_N"] <- ifelse(sortRotation[jj,"N.rate.kg.ha"] == 0, 0,
                                     (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"N.rate.kg.ha"])
  #Use N5 for P recovery, N not limiting...
  #NKMg only present in sections 0 and 1 with continuous wheat
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "NKMg" & sortRotation[,"section"] <= 1) 
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "N5PKMg" & sortRotation[,"section"] <= 1) 
  sortRotation[jj,"rec_P"] <- ifelse(sortRotation[jj,"P.rate.kg.ha"] == 0, 0,
                                     (sortRotation[jj,"P.offtake.kg.ha"] - sortRotation[ii,"P.offtake.kg.ha"]) / sortRotation[jj,"P.rate.kg.ha"])
  #For other sections and crops
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "N" & sortRotation[,"section"] > 1) 
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "N5PKMg" & sortRotation[,"section"] > 1) 
  sortRotation[jj,"rec_P"] <- ifelse(sortRotation[jj,"P.rate.kg.ha"] == 0, 0,
                                     (sortRotation[jj,"P.offtake.kg.ha"] - sortRotation[ii,"P.offtake.kg.ha"]) / sortRotation[jj,"P.rate.kg.ha"])
  #Use N5 for K recovery, N not limiting...
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "NP")
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "N5PKMg")
  sortRotation[jj,"rec_K"] <- ifelse(sortRotation[jj,"K.rate.kg.ha"] == 0, 0,
                                     (sortRotation[jj,"K.offtake.kg.ha"] - sortRotation[ii,"K.offtake.kg.ha"]) / sortRotation[jj,"K.rate.kg.ha"])
  
  #The FYM+PK treatment does not exist in the Broadbalk experiment
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "PKMg")
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "FYM")
  sortRotation[jj,"rec_FYMN"] <- ifelse(sortRotation[jj,"FYMN.rate.kg.ha"] == 0, 0, 
                                        (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"FYMN.rate.kg.ha"])
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "NKMg" & sortRotation[,"section"] <= 1)
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "FYM" & sortRotation[,"section"] <= 1)
  sortRotation[jj,"rec_FYMP"] <- ifelse(sortRotation[jj,"FYMN.rate.kg.ha"] == 0, 0, 
                                        (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"FYMN.rate.kg.ha"])
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "N" & sortRotation[,"section"] > 1)
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "FYM" & sortRotation[,"section"] > 1)
  sortRotation[jj,"rec_FYMP"] <- ifelse(sortRotation[jj,"FYMN.rate.kg.ha"] == 0, 0, 
                                        (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"FYMN.rate.kg.ha"])
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "NP")
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "FYM")
  sortRotation[jj,"rec_FYMK"] <- ifelse(sortRotation[jj,"FYMN.rate.kg.ha"] == 0, 0, 
                                        (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"FYMN.rate.kg.ha"])
  
  #Use N4 for N recovery, this equates to n2 and N4 as used in FYM+NPK
  ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "FYM")
  jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Trt"] == "FYM+NPK")
  sortRotation[jj,"rec_N_FYM"] <- ifelse(sortRotation[jj,"FYMN.rate.kg.ha"] == 0, 0, 
                                   (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"N.rate.kg.ha"])
  sortRotation[jj,"rec_P_FYM"] <- ifelse(sortRotation[jj,"FYMP.rate.kg.ha"] == 0, 0, 
                                        (sortRotation[jj,"P.offtake.kg.ha"] - sortRotation[ii,"P.offtake.kg.ha"]) / sortRotation[jj,"P.rate.kg.ha"])
  sortRotation[jj,"rec_K_FYM"] <- ifelse(sortRotation[jj,"FYMK.rate.kg.ha"] == 0, 0, 
                                        (sortRotation[jj,"K.offtake.kg.ha"] - sortRotation[ii,"K.offtake.kg.ha"]) / sortRotation[jj,"K.rate.kg.ha"])
}

#Write to file
write.csv(Potato,PotatoFileOut)
write.csv(Oats,OatsFileOut)
write.csv(Wheat,WheatFileOut)
write.csv(Beans,BeansFileOut)
write.csv(Maize,MaizeFileOut)
write.csv(sortRotation,RotationFileOut)


###Do some plotting
 plotCropOfftakes(df = sortRotation, crop = "potato")
 
 pdf(MeasuredOfftakesPerYearPotato, width = 12, height = 12)
   plotCropOfftakes(df = sortRotation, crop = "potato")
 dev.off()
 pdf(MeasuredOfftakesPerYearWheat, width = 12, height = 12)
   plotCropOfftakes(df = sortRotation, crop = "wheat")
 dev.off()
 pdf(MeasuredOfftakesPerYearBeans, width = 12, height = 12)
   plotCropOfftakes(df = sortRotation, crop = "beans")
 dev.off()
 pdf(MeasuredOfftakesPerYearMaize, width = 12, height = 12)
  plotCropOfftakes(df = sortRotation, crop = "maize")
 dev.off()
 pdf(MeasuredOfftakesPerYearOats, width = 12, height = 12)
  plotCropOfftakes(df = sortRotation, crop = "oats")
 dev.off()
 # 

####SOME ANALYSIS ON THIS DATASET
#Needs sections 1 for NK and NP treatments in 1968 and 1969
#sortRotationPred <- sortRotation
#Sections 2,3,4,5,7 are with crop rotations
sortRotationPred <- rbind(subset(sortRotation, section == 1 & Year >=1968 & Year <= 1969),
                          subset(sortRotation, section == 2),
                          subset(sortRotation, section == 3),
                          subset(sortRotation, section == 4),
                          subset(sortRotation, section == 5),
                          subset(sortRotation, section == 7))


#Crop * year interaction is not significant for N and P!!!
fitN <- lm(N.offtake.kg.ha ~ FertRegime * Trt + Crop * (Year  + Trt), sortRotationPred)
fitN <- lm(N.offtake.kg.ha ~ FertRegime * Trt + Crop * (Year  + Trt) + section * Year , sortRotationPred)
print(summary(fitN))
print(anova(fitN))
#fitP <- lm(P.offtake.kg.ha ~ FertRegime * Trt + Crop * (Year  + Trt), sortRotationPred)
fitP <- lm(P.offtake.kg.ha ~ FertRegime * Trt + Crop * (Year  + Trt) + section * Year , sortRotationPred)
print(summary(fitP))
print(anova(fitP))
#fitK <- lm(K.offtake.kg.ha ~ FertRegime * Trt + Crop * (Year  + Trt), sortRotationPred)
fitK <- lm(K.offtake.kg.ha ~ FertRegime * Trt + Crop * (Year  + Trt) + section * Year , sortRotationPred)
print(summary(fitK))
print(anova(fitK))
resultDF <- data.frame(Nutrient = c("N", "P", "K"),
                       R2 = c(summary(fitN)$r.squared, summary(fitP)$r.squared, summary(fitK)$r.squared),
                       RMSE = c( sqrt(mean(fitN$residuals^2)), sqrt(mean(fitP$residuals^2)),sqrt(mean(fitK$residuals^2)))          )
resultDF[,"R2"]<- round(resultDF[,"R2"],2)
resultDF[,"RMSE"]<- round(resultDF[,"RMSE"],2)
print(resultDF)

ii <- which(sortRotationPred[,"Year"] >= 1968)
sortRotationPred[ii,"LMpred.N.offtake.kg.ha"] <- predict(fitN,sortRotationPred[ii,])
sortRotationPred[ii,"LMpred.P.offtake.kg.ha"] <- predict(fitP,sortRotationPred[ii,])
sortRotationPred[ii,"LMpred.K.offtake.kg.ha"] <- predict(fitK,sortRotationPred[ii,])



pdf(CumUptakeRecoveryPerYear, width=12, height=12)
  ii <- which(sortRotationPred[,"section"] == 4)
  plotCumOfftakes_LMpred(df = sortRotationPred[ii,])
dev.off()



#recovery
sortRotationPred[,"LMpred.rec_N"] <- 0
sortRotationPred[,"LMpred.rec_P"] <- 0
sortRotationPred[,"LMpred.rec_K"] <- 0
sortRotationPred[,"LMpred.rec_FYMN"] <- 0
sortRotationPred[,"LMpred.rec_FYMP"] <- 0
sortRotationPred[,"LMpred.rec_FYMK"] <- 0
sortRotationPred[,"LMpred.rec_N_FYM"] <- 0
sortRotationPred[,"LMpred.rec_P_FYM"] <- 0
sortRotationPred[,"LMpred.rec_K_FYM"] <- 0


ii <- which(sortRotationPred[,"Trt"] == "N2PKMg")
#fitN <- lm(rec_N ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
fitN <- lm(rec_N ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
print(summary(fitN))
print(anova(fitN))
jj <- which(sortRotationPred[,"N.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_N"] <- predict(fitN,sortRotationPred[jj,])

ii <- which(sortRotationPred[,"Trt"] == "N5PKMg")
fitP <- lm(rec_P ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
#fitP <- lm(rec_P ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
print(summary(fitP))
print(anova(fitP))
jj <- which(sortRotationPred[,"P.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_P"] <- predict(fitP,sortRotationPred[jj,])

ii <- which(sortRotationPred[,"Trt"] == "N5PKMg")
fitK <- lm(rec_K ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
print(summary(fitK))
print(anova(fitK))
jj <- which(sortRotationPred[,"K.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_K"] <- predict(fitK,sortRotationPred[jj,])

resultDF_NPK <- data.frame(Recovery = c("N", "P", "K"),
                           Comparison = "NPK-omission",
                           R2 = c(summary(fitN)$r.squared, summary(fitP)$r.squared, summary(fitK)$r.squared),
                           RMSE = c( sqrt(mean(fitN$residuals^2)), sqrt(mean(fitP$residuals^2)),sqrt(mean(fitK$residuals^2)))          )


ii <- which(sortRotationPred[,"Trt"] == "FYM")
fitN <- lm(rec_FYMN ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
summary(fitN)
anova(fitN)
jj <- which(sortRotationPred[,"FYMN.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_FYMN"] <- predict(fitN,sortRotationPred[jj,])

ii <- which(sortRotationPred[,"Trt"] == "FYM")
fitP <- lm(rec_FYMP ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
summary(fitP)
anova(fitP)
jj <- which(sortRotationPred[,"FYMP.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_FYMP"] <- predict(fitP,sortRotationPred[jj,])

ii <- which(sortRotationPred[,"Trt"] == "FYM")
fitK <- lm(rec_FYMK ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
summary(fitK)
anova(fitK)
jj <- which(sortRotationPred[,"FYMK.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_FYMK"] <- predict(fitK,sortRotationPred[jj,])

resultDF_FYM <- data.frame(Recovery = c("FYMN", "FYMP", "FYMK"),
                           Comparison = "FYM-omission",
                           R2 = c(summary(fitN)$r.squared, summary(fitP)$r.squared, summary(fitK)$r.squared),
                           RMSE = c( sqrt(mean(fitN$residuals^2)), sqrt(mean(fitP$residuals^2)),sqrt(mean(fitK$residuals^2)))          )


ii <- which(sortRotationPred[,"Trt"] == "FYM+NPK")
fitN <- lm(rec_N_FYM ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
summary(fitN)
anova(fitN)
jj <- which(sortRotationPred[,"FYMN.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_N_FYM"] <- predict(fitN,sortRotationPred[jj,])

ii <- which(sortRotationPred[,"Trt"] == "FYM+NPK")
fitP <- lm(rec_P_FYM ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
summary(fitP)
anova(fitP)
jj <- which(sortRotationPred[,"FYMP.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_P_FYM"] <- predict(fitP,sortRotationPred[jj,])

ii <- which(sortRotationPred[,"Trt"] == "FYM+NPK")
fitK <- lm(rec_K_FYM ~ FertRegime + Crop * Year + section, sortRotationPred[ii, ])
summary(fitK)
anova(fitK)
jj <- which(sortRotationPred[,"FYMK.rate.kg.ha"] > 0)
sortRotationPred[jj,"LMpred.rec_K_FYM"] <- predict(fitK,sortRotationPred[jj,])

resultDF_FYMNPK <- data.frame(Recovery = c("N-FYM", "P-FYM", "K-FYM"),
                              Comparison = "FYMNPK-FYM + ommision",
                              R2 = c(summary(fitN)$r.squared, summary(fitP)$r.squared, summary(fitK)$r.squared),
                              RMSE = c( sqrt(mean(fitN$residuals^2)), sqrt(mean(fitP$residuals^2)),sqrt(mean(fitK$residuals^2)))          )
result_df<-rbind(resultDF_NPK, resultDF_FYM, resultDF_FYMNPK)
result_df[,"R2"]<- round(result_df[,"R2"],2)
result_df[,"RMSE"]<- round(result_df[,"RMSE"],2)
print(result_df)

#write dataset with LM predictions added 
write.csv(sortRotationPred,RotationFileOutLMpred)

#Cumulative applications and predicted uptakes
for(sec in unique(sortRotationPred[,"section"])){
  for(treat in unique(sortRotationPred[,"Trt"])){
    ii<-which(sortRotation[,"section"] == sec & sortRotation[,"Trt"] == treat)
    sortRotationPred[ii,"cum.N.rate.kg.ha"] <- cumsum(sortRotationPred[ii,"N.rate.kg.ha"]+sortRotationPred[ii,"FYMN.rate.kg.ha"])
    sortRotationPred[ii,"cum.P.rate.kg.ha"] <- cumsum(sortRotationPred[ii,"P.rate.kg.ha"]+sortRotationPred[ii,"FYMP.rate.kg.ha"])
    sortRotationPred[ii,"cum.K.rate.kg.ha"] <- cumsum(sortRotationPred[ii,"K.rate.kg.ha"]+sortRotationPred[ii,"FYMK.rate.kg.ha"])
    sortRotationPred[ii,"cum.LMpred.N.offtake.kg.ha"] <- cumsum(sortRotationPred[ii,"LMpred.N.offtake.kg.ha"])
    sortRotationPred[ii,"cum.LMpred.P.offtake.kg.ha"] <- cumsum(sortRotationPred[ii,"LMpred.P.offtake.kg.ha"])
    sortRotationPred[ii,"cum.LMpred.K.offtake.kg.ha"] <- cumsum(sortRotationPred[ii,"LMpred.K.offtake.kg.ha"])
  }
}

MDF <- NULL
for(crop in c("potato", "wheat", "oats","beans","maize")){
  ii <- which(sortRotation[,"Crop"] == crop & sortRotation[,"Trt"] == "N2PKMg")
  MDF <- rbind(MDF, data.frame(Crop = crop, Treatment ="N2PKMg",
                               Note = "Rec. from fertilizer NPK",
                               m_rec_N = mean(sortRotation[ii,"rec_N"], na.rm= TRUE),
                               m_rec_P = mean(sortRotation[ii,"rec_P"], na.rm= TRUE),
                               m_rec_K = mean(sortRotation[ii,"rec_K"], na.rm= TRUE)))
  ii <- which(sortRotation[,"Crop"] == crop & sortRotation[,"Trt"] == "FYM")
  MDF <- rbind(MDF, data.frame(Crop = crop, Treatment ="FYM",
                               Note = "Rec. from FYM",
                               m_rec_N = mean(sortRotation[ii,"rec_FYMN"], na.rm= TRUE),
                               m_rec_P = mean(sortRotation[ii,"rec_FYMP"], na.rm= TRUE),
                               m_rec_K = mean(sortRotation[ii,"rec_FYMK"], na.rm= TRUE))) 
  ii <- which(sortRotation[,"Crop"] == crop & sortRotation[,"Trt"] == "FYM+NPK")
  MDF <- rbind(MDF, data.frame(Crop = crop, Treatment ="FYM+NPK",
                               Note = "Rec. from fertilizer NPK when applied with FYM",
                               m_rec_N = mean(sortRotation[ii,"rec_N_FYM"], na.rm= TRUE),
                               m_rec_P = mean(sortRotation[ii,"rec_P_FYM"], na.rm= TRUE),
                               m_rec_K = mean(sortRotation[ii,"rec_K_FYM"], na.rm= TRUE))) 
}
print(MDF)

#measured recoveries
pdf(MeasuredRecoveriesPerYear, width=12, height=12)
  plotNPKRecovery(df = sortRotation, prefix = "rec_", x_axis=c("N","P","K"))
  plotNPKRecovery(df = sortRotation, prefix = "rec_", x_axis=c("FYMN","FYMP","FYMK"))
dev.off()
#predicted recoveries
pdf(PredictedRecoveriesPerYear, width=12, height=12)
  plotNPKRecovery(df = sortRotationPred, prefix = "LMpred.rec_", x_axis=c("N","P","K"))
  plotNPKRecovery(df = sortRotationPred, prefix = "LMpred.rec_", x_axis=c("FYMN","FYMP","FYMK"))
dev.off()
  
#Replace values by predicted values for easy plotting
sortRotationPred[,"N.offtake.kg.ha"] <- sortRotationPred[,"LMpred.N.offtake.kg.ha"]
sortRotationPred[,"P.offtake.kg.ha"] <- sortRotationPred[,"LMpred.P.offtake.kg.ha"]
sortRotationPred[,"K.offtake.kg.ha"] <- sortRotationPred[,"LMpred.K.offtake.kg.ha"]


#Only measured and predicted uptakes after 1968
pdf(CumPredictedOfftakes, width=12, height=12)
  for(sec in unique(sortRotationPred[, "section"])){
    plotCumApplication_vs_CumLMpredOfftake(sRP = subset(sortRotationPred, section == sec),
                                           title=paste0("Section: ",sec))
  }
dev.off()


