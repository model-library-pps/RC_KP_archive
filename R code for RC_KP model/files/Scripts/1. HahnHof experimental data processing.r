require(openxlsx)
require(lme4)
require(lmerTest)
source("RC_KP_model.R")
source("0. Hahnhof figures.r")

#INPUT
filename <- "..//Data//Hanninghof NPK input & output and crop yield including Mg treatments.xlsx"
Potato = read.xlsx(filename,sheet = "Potato")
Oats = read.xlsx(filename,sheet = "Oat")
Rye = read.xlsx(filename,sheet = "Rye")
Maize = read.xlsx(filename,sheet = "Maize")

SoilP = read.xlsx("..//Data//Hanninghof soil PK content.xlsx",sheet = "P-CAL (0-30)")
SoilK = read.xlsx("..//Data//Hanninghof soil PK content.xlsx",sheet = "K-CAL (0-30)")

#OUTPUT FILENAMES
potatoFileOut = "..//Data//Processed//1. HanningHof_potato_NPK_offtakes_kgha.csv"
OatsFileOut = "..//Data//Processed//1. HanningHof_oats_NPK_offtakes_kgha.csv"
RyeFileOut = "..//Data//Processed//1. HanningHof_rye_NPK_offtakes_kgha.csv"
MaizeFileOut = "..//Data//Processed//1. HanningHof_maize_NPK_offtakes_kgha.csv"
RotationFileOut = "..//Data//Processed//1. HanningHof_rotation_NPK_offtakes_kgha.csv"
RotationFileOutLMpred = "..//Data//Processed//1. HanningHof_rotation_NPK_offtakes_kgha_MLpredicted.csv"

MeasuredOfftakesPerYearPotato = "..//Results//1. HanningHof_measured_offtakes_potato_kgha.pdf"
MeasuredOfftakesPerYearRye = "..//Results//1. HanningHof_measured_offtakes_rye_kgha.pdf"
MeasuredOfftakesPerYearOats = "..//Results//1. HanningHof_measured_offtakes_oats_kgha.pdf"
CumMeasuredOfftakesPerYear = "..//Results//1. HanningHof_cumulative_measured_offtakes_rotation_kgha.pdf"
CumMeasuredOfftakesPerYearFYM = "..//Results//1. HanningHof_cumulative_measured_offtakes_FYMrotation_kgha.pdf"
PredictedOfftakesPerYear = "..//Results//1. HanningHof_LMpredicted_offtakes_rotation_kgha.pdf"
CumPredictedOfftakesPerYear = "..//Results//1. HanningHof_cumulative_LMpredicted_offtakes_rotation_kgha.pdf"
CumPredictedOfftakesPerYearFYM = "..//Results//1. HanningHof_cumulative_LMpredicted_offtakes_FYMrotation_kgha.pdf"
MeasuredRecoveriesPerYear = "..//Results//1. HanningHof_measured_recoveries.pdf"
PredictedRecoveriesPerYear = "..//Results//1. HanningHof_LMpredicted_recoveries.pdf"
SoilPK_vs_MeasuredOfftakePK = "..//Results//1. HanningHof_measured_offtakes_vs_soil PK-Cal.pdf"


#Process raw data
cnP <- colnames(Potato)
cnO <- colnames(Oats)
cnR <- colnames(Rye)
cnM <- colnames(Maize)

sPotato <- subset(Potato, select=c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                                   "N.rate.(kg/ha)","P.rate.(kg/ha)","K.rate.(kg/ha)",
                                   "*N.removal.by.tuber.(kg/ha),.Mean","*P.removal.by.tuber.(kg/ha),.Mean","*K.removal.by.tuber.(kg/ha),.Mean","Tuber.(t/ha)"))
sOats <- subset(Oats, select=c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                                   "N.rate.(kg/ha)","P.rate.(kg/ha)","K.rate.(kg/ha)",
                                   "*N.removal.by.grain.+.straw.(kg/ha),.Mean","*P.removal.by.grain.+.straw.(kg/ha),.Mean","*K.removal.by.grain.+.straw.(kg/ha),.Mean","Grain.(t/ha)"))
sRye <- subset(Rye, select=c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                               "N.rate.(kg/ha)","P.rate.(kg/ha)","K.rate.(kg/ha)",
                               "*N.removal.by.grain.+.straw.(kg/ha),.Mean","*P.removal.by.grain.+.straw.(kg/ha),.Mean","*K.removal.by.grain.+.straw.(kg/ha),.Mean","Grain.(t/ha)"))
sMaize <- subset(Maize, select=c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                                   "N.rate.(kg/ha)","P.rate.(kg/ha)","K.rate.(kg/ha)",
                                   "*N.removal.by.silage.(kg/ha),.Mean","*P.removal.by.silage.(kg/ha),.Mean","*K.removal.by.silage.(kg/ha),.Mean","Silage.(t/ha)"))
colnames(sPotato) <- c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                        "N.rate.kg.ha","P.rate.kg.ha","K.rate.kg.ha",
                        "N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")
colnames(sOats) <- c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                       "N.rate.kg.ha","P.rate.kg.ha","K.rate.kg.ha",
                       "N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")
colnames(sRye) <- c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                       "N.rate.kg.ha","P.rate.kg.ha","K.rate.kg.ha",
                       "N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")
colnames(sMaize) <- c("Year","Field_Code","Crop","Treatment","Description","Replication","Date_Code", 
                       "N.rate.kg.ha","P.rate.kg.ha","K.rate.kg.ha",
                       "N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha","Yield.t.ha")
Rotation <- rbind(sPotato, sOats, sRye, sMaize)
ssR <- sort(Rotation[,"Year"], index.return = TRUE)
sortRotation <- Rotation[ssR$ix,]

#Add indicator for the two fertilizer regime periods
sortRotation[, "FertRegime"] <- 1
ii<-which(sortRotation[,"Year"] < 1979)
sortRotation[ii, "FertRegime"] <- 0


sortRotation[,"FYMN.rate.kg.ha"] <- 0
sortRotation[,"FYMP.rate.kg.ha"] <- 0
sortRotation[,"FYMK.rate.kg.ha"] <- 0

#Mineral fertilizer rates were the same for all treatments with and without FYM
#FYM as pig manure was applied before potatoes, with 25t/ha.
#FYM N, P, K applications:  175 N/ha, 73.9 kg P/ha, 149.4 kg K/ha

for(year in unique(sortRotation[,"Year"])){
    #Determine how much N, P and K was added with FYM
    jj <- which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM")
    FYMN <- sortRotation[jj,"N.rate.kg.ha"]
    FYMP <- sortRotation[jj,"P.rate.kg.ha"]
    FYMK <- sortRotation[jj,"K.rate.kg.ha"]
    #Add to separate column and subtract from N, P and K rate
    for(trt in c("FYM","FYM+NPK","FYM+NP","FYM+NK","FYM+PK","FYM+NPKMg")){
      ii <- which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == trt)
      sortRotation[ii,"N.rate.kg.ha"] <- sortRotation[ii,"N.rate.kg.ha"] - FYMN
      sortRotation[ii,"P.rate.kg.ha"] <- sortRotation[ii,"P.rate.kg.ha"] - FYMP
      sortRotation[ii,"K.rate.kg.ha"] <- sortRotation[ii,"K.rate.kg.ha"] - FYMK
      sortRotation[ii,"FYMN.rate.kg.ha"] <- FYMN
      sortRotation[ii,"FYMP.rate.kg.ha"] <- FYMP
      sortRotation[ii,"FYMK.rate.kg.ha"] <- FYMK
    }  
}

#Apparent recovery per year (one crop per year only) and treatment
sortRotation[,"rec_N"] <- NA
sortRotation[,"rec_P"] <- NA
sortRotation[,"rec_K"] <- NA
sortRotation[,"rec_FYMN"] <- NA
sortRotation[,"rec_FYMP"] <- NA
sortRotation[,"rec_FYMK"] <- NA
for(year in unique(sortRotation[,"Year"])){
  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "PK")
  jj<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "NPK")
  sortRotation[jj,"rec_N"] <- ifelse(sortRotation[jj,"N.rate.kg.ha"] == 0, NA,
                                     (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"N.rate.kg.ha"])
  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "NK")
  sortRotation[jj,"rec_P"] <- ifelse(sortRotation[jj,"N.rate.kg.ha"] == 0, NA,
                                     (sortRotation[jj,"P.offtake.kg.ha"] - sortRotation[ii,"P.offtake.kg.ha"]) / sortRotation[jj,"P.rate.kg.ha"])
  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "NP")
  sortRotation[jj,"rec_K"] <- ifelse(sortRotation[jj,"N.rate.kg.ha"] == 0, NA,
                                     (sortRotation[jj,"K.offtake.kg.ha"] - sortRotation[ii,"K.offtake.kg.ha"]) / sortRotation[jj,"K.rate.kg.ha"])

  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM+PK")
  jj<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM+NPK")
  sortRotation[jj,"rec_N_FYM"] <- ifelse(sortRotation[jj,"N.rate.kg.ha"] == 0, NA, 
                                   (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"N.rate.kg.ha"])
  ii<-which(sortRotation[,"Year"] == year &sortRotation[,"Description"] == "FYM+NK")
  sortRotation[jj,"rec_P_FYM"] <- ifelse(sortRotation[jj,"P.rate.kg.ha"] == 0, NA, 
                                   (sortRotation[jj,"P.offtake.kg.ha"] - sortRotation[ii,"P.offtake.kg.ha"]) / sortRotation[jj,"P.rate.kg.ha"])
  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM+NP")
  sortRotation[jj,"rec_K_FYM"] <- ifelse(sortRotation[jj,"K.rate.kg.ha"] == 0, NA, 
                                   (sortRotation[jj,"K.offtake.kg.ha"] - sortRotation[ii,"K.offtake.kg.ha"]) / sortRotation[jj,"K.rate.kg.ha"])

  kk <- which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM+NPK")
  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "PK")
  jj<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM+PK")
  sortRotation[jj,"rec_FYMN"] <- ifelse(sortRotation[jj,"FYMN.rate.kg.ha"] == 0, 0, 
                                       (sortRotation[jj,"N.offtake.kg.ha"] - sortRotation[ii,"N.offtake.kg.ha"]) / sortRotation[jj,"FYMN.rate.kg.ha"])
  sortRotation[kk,"rec_FYMN"] <- sortRotation[jj,"rec_FYMN"]
  
  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "NK")
  jj<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM+NK")
  sortRotation[jj,"rec_FYMP"] <- ifelse(sortRotation[jj,"FYMP.rate.kg.ha"] == 0, 0, 
                                       (sortRotation[jj,"P.offtake.kg.ha"] - sortRotation[ii,"P.offtake.kg.ha"]) / sortRotation[jj,"FYMP.rate.kg.ha"])
  sortRotation[kk,"rec_FYMP"] <- sortRotation[jj,"rec_FYMP"]

  ii<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "NP")
  jj<-which(sortRotation[,"Year"] == year & sortRotation[,"Description"] == "FYM+NP")
  sortRotation[jj,"rec_FYMK"] <- ifelse(sortRotation[jj,"FYMK.rate.kg.ha"] == 0, 0, 
                                      (sortRotation[jj,"K.offtake.kg.ha"] - sortRotation[ii,"K.offtake.kg.ha"]) / sortRotation[jj,"FYMK.rate.kg.ha"])
  sortRotation[kk,"rec_FYMK"] <- sortRotation[jj,"rec_FYMK"]
}

#Write to file
write.csv(sPotato,potatoFileOut)
write.csv(sOats,OatsFileOut)
write.csv(sRye,RyeFileOut)
write.csv(sMaize,MaizeFileOut)
write.csv(sortRotation,RotationFileOut)


###Do some plotting
sortRotation[,"Trt"] <- sortRotation[,"Description"]
plotCropOfftakes(df = sortRotation, crop = "potato")

pdf(MeasuredOfftakesPerYearPotato, width=12, height=12)
  plotCropOfftakes(df = sortRotation, crop = "potato")
dev.off()
pdf(MeasuredOfftakesPerYearRye, width=12, height=12)
  plotCropOfftakes(df = sortRotation, crop = "rye")
dev.off()
pdf(MeasuredOfftakesPerYearOats, width=12, height=12)
  plotCropOfftakes(df = sortRotation, crop = "oats")
dev.off()


plotCumOfftakes_no_FYM(df = sortRotation)
pdf(CumMeasuredOfftakesPerYear, width=12, height=12)
  plotCumOfftakes_no_FYM(df = sortRotation)
dev.off()

plotCumOfftakes_FYM(df = sortRotation)

pdf(CumMeasuredOfftakesPerYearFYM, width=12, height=12)
  plotCumOfftakes_FYM(df = sortRotation)
dev.off()

plotCropYieldOfftakes(df = sortRotation, CropQUEFTS_param = RCKP_param(type="TSP", UseMeasuredNUptake = USE_MEASURED_UPTAKES))

####SOME ANALYSIS ON THIS DATASET
sortRotationPred <- sortRotation


fitN <- lm(N.offtake.kg.ha ~ FertRegime + Crop * ( Year  + Description) , sortRotationPred)
summary(fitN)
anova(fitN)


fitP <- lm(P.offtake.kg.ha ~ FertRegime + Crop * (Year  + Description) , sortRotationPred)
summary(fitP)
anova(fitP)
fitK <- lm(K.offtake.kg.ha ~ FertRegime + Crop * (Year  + Description), sortRotationPred)
summary(fitK)
anova(fitK)

sortRotationPred[,"LMpred.N.offtake.kg.ha"] <- predict(fitN,sortRotationPred)
sortRotationPred[,"LMpred.P.offtake.kg.ha"] <- predict(fitP,sortRotationPred)
sortRotationPred[,"LMpred.K.offtake.kg.ha"] <- predict(fitK,sortRotationPred)


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

#Filter out first-year negative values (mainly for P and K, that may affect models too much)
ii<-which(sortRotation[,"Description"] == "NPK" & sortRotation[,"rec_N"] >= 0)
fitN <- lm(rec_N ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
summary(fitN)
anova(fitN)
ii <- which(sortRotation[,"N.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_N"] <- predict(fitN,sortRotationPred[ii, ])

ii<-which(sortRotation[,"Description"] == "NPK" & sortRotation[,"rec_P"] >= 0)
fitP <- lm(rec_P ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
summary(fitP)
anova(fitP)
ii <- which(sortRotation[,"P.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_P"] <- predict(fitP,sortRotationPred[ii, ])

ii<-which(sortRotation[,"Description"] == "NPK" & sortRotation[,"rec_K"] >= 0)
fitK <- lm(rec_K ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
summary(fitK)
anova(fitK)
ii <- which(sortRotation[,"K.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_K"] <- predict(fitK,sortRotationPred[ii, ])  


#Filter out first-year negative values (mainly for P and K, that may affect models too much)
ii<-which(sortRotation[,"Description"] == "FYM+NPK" & sortRotation[,"rec_N_FYM"] >= 0)
fitN <- lm(rec_N_FYM ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
summary(fitN)
anova(fitN)
ii <- which(sortRotation[,"FYMN.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_N_FYM"] <- predict(fitN,sortRotationPred[ii, ])

ii<-which(sortRotation[,"Description"] == "FYM+NPK" & sortRotation[,"rec_P_FYM"] >= 0)
fitP <- lm(rec_P_FYM ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
summary(fitP)
anova(fitP)
ii <- which(sortRotation[,"FYMP.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_P_FYM"] <- predict(fitN,sortRotationPred[ii, ])

ii<-which(sortRotation[,"Description"] == "FYM+NPK" & sortRotation[,"rec_K_FYM"] >= 0)
fitK <- lm(rec_K_FYM ~ FertRegime + Crop * Year, sortRotationPred[ii, ])
summary(fitK)
anova(fitK)
ii <- which(sortRotation[,"FYMK.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_K_FYM"] <- predict(fitN,sortRotationPred[ii, ])


#FYM was only applied on potatoes
#FertRegime is not significant, so left out
jj<-which(sortRotation[,"Description"] == "FYM+NPK" & sortRotation[,"Crop"] == "potato")
ii<-which(sortRotation[,"Description"] == "FYM+PK" & sortRotation[,"Crop"] == "potato")
fitFYMN <- lm(rec_FYMN ~ 1 + Year, sortRotationPred[ii, ])
summary(fitFYMN)
anova(fitFYMN)
ii <- which(sortRotation[,"FYMN.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_FYMN"] <- predict(fitFYMN,sortRotationPred[ii, ])

ii<-which(sortRotation[,"Description"] == "FYM+NK" & sortRotation[,"Crop"] == "potato")
fitFYMP <- lm(rec_FYMP ~ 1 + Year, sortRotationPred[ii, ])
summary(fitFYMP)
anova(fitFYMP)
ii <- which(sortRotation[,"FYMP.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_FYMP"] <- predict(fitFYMP,sortRotationPred[ii, ])

ii<-which(sortRotation[,"Description"] == "FYM+NP" & sortRotation[,"Crop"] == "potato")
fitFYMK <- lm(rec_FYMK ~ 1 + Year, sortRotationPred[ii, ])
summary(fitFYMK)
anova(fitFYMK)
ii <- which(sortRotation[,"FYMK.rate.kg.ha"] > 0)
sortRotationPred[ii,"LMpred.rec_FYMK"] <- predict(fitFYMK,sortRotationPred[ii, ])



#write dataset with LM predictions added 
write.csv(sortRotationPred,RotationFileOutLMpred)

#Cumulative applications and predicted uptakes
for(treat in unique(sortRotationPred[,"Description"])){
  ii<-which(sortRotation[,"Description"] == treat)
  sortRotationPred[ii,"cum.N.rate.kg.ha"] <- cumsum(sortRotationPred[ii,"N.rate.kg.ha"])
  sortRotationPred[ii,"cum.P.rate.kg.ha"] <- cumsum(sortRotationPred[ii,"P.rate.kg.ha"])
  sortRotationPred[ii,"cum.K.rate.kg.ha"] <- cumsum(sortRotationPred[ii,"K.rate.kg.ha"])
  sortRotationPred[ii,"cum.LMpred.N.offtake.kg.ha"] <- cumsum(sortRotationPred[ii,"LMpred.N.offtake.kg.ha"])
  sortRotationPred[ii,"cum.LMpred.P.offtake.kg.ha"] <- cumsum(sortRotationPred[ii,"LMpred.P.offtake.kg.ha"])
  sortRotationPred[ii,"cum.LMpred.K.offtake.kg.ha"] <- cumsum(sortRotationPred[ii,"LMpred.K.offtake.kg.ha"])
}

MDF <- NULL
for(crop in c("potato", "rye", "oats")) {
  ii <- which(sortRotation[,"Crop"] == crop & sortRotation[,"Description"] == "NPK")
  MDF <- rbind(MDF, data.frame(Crop = crop, Treatment = "NPK",
                               Note = "Rec. from fertilizer NPK",
                               m_rec_N = mean(sortRotation[ii,"rec_N"], na.rm = TRUE),
                               m_rec_P = mean(sortRotation[ii,"rec_P"], na.rm = TRUE),
                               m_rec_K = mean(sortRotation[ii,"rec_K"], na.rm = TRUE)))
  ii <- which(sortRotation[,"Crop"] == crop & sortRotation[,"Description"] == "FYM+NPK")
  MDF <- rbind(MDF, data.frame(Crop = crop, Treatment = "FYM+NPK",
                               Note = "Rec. from fertilizer NPK when applied with FYM",
                               m_rec_N = mean(sortRotation[ii,"rec_N_FYM"], na.rm = TRUE),
                               m_rec_P = mean(sortRotation[ii,"rec_P_FYM"], na.rm = TRUE),
                               m_rec_K = mean(sortRotation[ii,"rec_K_FYM"], na.rm = TRUE)))
  ii <- which(sortRotation[,"Crop"] == crop & sortRotation[,"Description"] == "FYM")
  MDF <- rbind(MDF, data.frame(Crop = crop, Treatment = "FYM",
                               Note = "Rec. from FYM",
                               m_rec_N = mean(sortRotation[ii,"rec_FYMN"], na.rm = TRUE),
                               m_rec_P = mean(sortRotation[ii,"rec_FYMP"], na.rm = TRUE),
                               m_rec_K = mean(sortRotation[ii,"rec_FYMK"], na.rm = TRUE))) 
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

pdf(PredictedOfftakesPerYear, width=12, height=12)
  plotCropOfftakes(df = sortRotationPred)
dev.off()

pdf(CumPredictedOfftakesPerYear, width=12, height=12)
  plotCumOfftakes_no_FYM(df = sortRotationPred)
dev.off()

pdf(CumPredictedOfftakesPerYearFYM, width=12, height=12)
  plotCumOfftakes_FYM(df = sortRotationPred)
dev.off()

plotCumApplication_vs_CumLMpredOfftake(sortRotationPred)

pdf(SoilPK_vs_MeasuredOfftakePK, width=12, height=12)
  plotSoilP_vs_Pofftake(sortRotation = merge(sortRotation, 
                                             subset(SoilP, select=c("Year","Crop","Description", "P-CAL_0-30.(mg.P2O5/100g)")),
                                             by=c("Year","Crop","Description")))
  plotSoilK_vs_Kofftake(sortRotation = merge(sortRotation, 
                                             subset(SoilK, select=c("Year","Crop","Description", "K-CAL.(mg.K2O/100g)")),
                                             by=c("Year","Crop","Description")))
dev.off()