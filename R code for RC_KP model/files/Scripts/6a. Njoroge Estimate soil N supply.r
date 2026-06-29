#Estimate soil N supply in phase 2 of the experiment

outfile1 <- "..//Data//Processed//6a. Njoroge estimated yields and N uptake PHASE1.csv"
outfile2 <- "..//Data//Processed//6a. Njoroge estimated yields and N uptake PHASE2.csv"

param <- RCKP_param(Crops = "maize", type="TSP")
rN = param$CropQUEFTS_Fresh.rN
dN = param$CropQUEFTS_Fresh.dN
aN = param$CropQUEFTS_Fresh.aN
facumm_dilution <- c(Control = 0.65, PK =0.8, NPK =0.7)
Nfert <- 150


#Read in yield data for all years
Ydata <- read.csv("..//Data//Njoroge yield data.csv")
#Add the SEASON number to yield data
nr = 0
for(useas in unique(Ydata[,"SEASON"]) ){
  nr = nr + 1
  ii <- which(Ydata[,"SEASON"] == useas)
  Ydata[ii,"SEASON_NR"] <- nr
}
#Fit linear regression to control and PK yields over all farms in NOT phase.
#This to estimate overall year effect, not strongly influenced by noise
d_Control <- subset(Ydata, TRIALTYPE == "NOT" & TREATMENT == "Control")
fitC <- lm(yield.t.AIRDRY.ha.1 ~ SEASON_NR+FARMCODE, d_Control)

d_PK <- subset(Ydata, TRIALTYPE == "NOT" & TREATMENT == "PK")
fit_PK <- lm(yield.t.AIRDRY.ha.1 ~ SEASON_NR+FARMCODE, d_PK)

d_NPK <- subset(Ydata, TRIALTYPE == "NOT" & TREATMENT == "NPK")
fit_NPK <- lm(yield.t.AIRDRY.ha.1 ~ SEASON_NR+FARMCODE, d_NPK)

#PHASE1
dNS1=NULL
for(uFC in  unique(d_PK[,"FARMCODE"])){
  df1 <- data.frame(FARMCODE=uFC, SEASON_NR = seq(0, 8, by = 1))
  Control_Y_FM =  predict(fitC, df1)
  PK_Y_FM =  predict(fit_PK, df1)
  NPK_Y_FM =  predict(fit_NPK, df1)
  dNS1 <- rbind(dNS1, data.frame(FARMCODE = uFC,
                               SEASON_NR = df1[,"SEASON_NR"],
                               Control_Y_FM = Control_Y_FM,
                               PK_Y_FM = PK_Y_FM,
                               NPK_Y_FM = NPK_Y_FM,
                               Control_NS = rN + 1000 * Control_Y_FM/(aN + facumm_dilution["Control"] * (dN-aN)),
                               PK_NS = rN + 1000 * PK_Y_FM/(aN + facumm_dilution["PK"] * (dN-aN)),
                               NPK_NS = rN + 1000 * NPK_Y_FM/(aN + facumm_dilution["NPK"] * (dN-aN)) ))
}
dNS1[,"Nrec"] <- (dNS1[,"NPK_NS"] - dNS1[,"PK_NS"])/Nfert

write.csv(dNS1, outfile1)

#For subplots in Phase 2, soil N supply differs between farms and treatments in phase 1.
#Season nr is negative effect (-0.79 t/ha/y) as expected.
d_PK_Phase2 <- subset(Ydata, TRIALTYPE == "SUBPLOTS" & NEWTREATMENT == "PK")
fit_PK_Phase2 <- lm(yield.t.AIRDRY.ha.1 ~ SEASON_NR+TREATMENT+FARMCODE, d_PK_Phase2)
sfit_PK_Phase2 <- summary(fit_PK_Phase2)
sfit_PK_Phase2_coeff <- sfit_PK_Phase2$coefficients

d_NPK_Phase2 <- subset(Ydata, TRIALTYPE == "SUBPLOTS" & NEWTREATMENT == "NPK")
fit_NPK_Phase2 <- lm(yield.t.AIRDRY.ha.1 ~ SEASON_NR+TREATMENT+FARMCODE, d_NPK_Phase2)
sfit_NPK_Phase2 <- summary(fit_NPK_Phase2)
sfit_NPK_Phase2_coeff <- sfit_NPK_Phase2$coefficients

#Exclude F11: cattle damaged plots and no yields were recorded in 2018.
d_REST_NPK_Phase2 <- subset(Ydata, TRIALTYPE == "RESTORATION" & NEWTREATMENT == "NPK" & FARMCODE != "F11")
fit_REST_NPK_Phase2 <- lm(yield.t.AIRDRY.ha.1 ~ SEASON_NR+FARMCODE, d_REST_NPK_Phase2)
sfit_REST_NPK_Phase2 <- summary(fit_REST_NPK_Phase2)
afit_REST_NPK_Phase2 <- anova(fit_REST_NPK_Phase2)
sfit_REST_NPK_Phase2_coeff <- sfit_REST_NPK_Phase2$coefficients

#Estimate yield and SN for PHASE2

dNS2=NULL

#PHASE2
for(trt in c("Control", "PK", "NK", "NP", "NPK")){
  #SUBPLOTS
  for(uFC in  unique(d_NPK_Phase2[,"FARMCODE"])){
    df2 <- data.frame(FARMCODE=uFC, TREATMENT=trt, SEASON_NR = seq(9, 11, by = 1))
    PK_Y_FM =  predict(fit_PK_Phase2, df2)
    NPK_Y_FM =  predict(fit_NPK_Phase2, df2)
    dNS2 <- rbind(dNS2, data.frame(FARMCODE = uFC,
                                 TRT=trt,
                                 TRIALTYPE = "NOT-SUBPLOTS",
                                 SEASON_NR = df2[,"SEASON_NR"],
                                 PK_Y_FM = PK_Y_FM,
                                 NPK_Y_FM = NPK_Y_FM,
                                 PK_NS = rN + 1000 * PK_Y_FM / (aN + facumm_dilution["PK"] * (dN-aN)),
                                 NPK_NS = rN + 1000 * NPK_Y_FM / (aN + facumm_dilution["NPK"] * (dN-aN)) ))
  }
  #Restauration
  for(uFC in  unique(d_REST_NPK_Phase2[,"FARMCODE"])){
    #Use reference farm F5: this effect of farm is cancelled.
    df1 <- data.frame(FARMCODE="F5", TREATMENT=trt, SEASON_NR = seq(8, 11, by = 1))
    df2 <- data.frame(FARMCODE=uFC, TREATMENT=trt, SEASON_NR = seq(8, 11, by = 1))
    NPK_Y_FM =  predict(fit_REST_NPK_Phase2, df2)
    #Subtract NPK treatment effect and add PK treatment effect as estimated in subplot trials
    PK_Y_FM =  (NPK_Y_FM 
                - predict(fit_NPK_Phase2, df1) 
                + predict(fit_PK_Phase2, df1))
    dNS2 <- rbind(dNS2, data.frame(FARMCODE = uFC,
                                 TRT=trt,
                                 TRIALTYPE = "RESTORATION",
                                 SEASON_NR = df2[,"SEASON_NR"],
                                 PK_Y_FM = PK_Y_FM,
                                 NPK_Y_FM = NPK_Y_FM,
                                 PK_NS = rN + 1000 * PK_Y_FM/ (aN + facumm_dilution["PK"] * (dN-aN)),
                                 NPK_NS =rN + 1000 * NPK_Y_FM/ (aN + facumm_dilution["NPK"] * (dN-aN))))
  }
}
dNS2[,"Nrec"] <- (dNS2[,"NPK_NS"] - dNS2[,"PK_NS"])/Nfert

write.csv(dNS2, outfile2)