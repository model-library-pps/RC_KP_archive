source('RC_KP_model.R')
source("00. General figures.r")

dfS <- read.csv("..//Results//9. Siaya_measured_predicted_NPKuptake.csv")
dfHH <- read.csv("..//Results//4. Hanninghof_LMpred_FYM_Hanninghof__measured_predicted_NPKuptake.csv")
dfBB <- read.csv("..//Results//14. Broadbalk_LMpred_BBalk__measured_predicted_NPKuptake.csv")

plot_meas_pred_com <- function(colx = "Measured_N_Uptake.kg.ha" , coly= "Predicted_N_Uptake.kg.ha"){
  xymin <- min(c(dfHH[,colx], dfHH[,coly],
                 dfBB[,colx], dfBB[,coly],
                 dfS[,colx], dfS[,coly]),
               na.rm=TRUE)  
  xymax <- max(c(dfHH[,colx], dfHH[,coly],
                 dfBB[,colx], dfBB[,coly],
                 dfS[,colx], dfS[,coly]),
               na.rm=TRUE)
  plot(dfHH[,colx], dfHH[,coly], xlim=c(xymin,xymax), ylim=c(xymin,xymax), col="black", pch=3)
  points(dfS[,colx], dfS[,coly], col="red", pch=17)
  points(dfBB[,colx], dfBB[,coly], col="gold", pch=16)
  
  xx<-c(dfS[,colx],dfHH[,colx],dfBB[,colx])
  yy<-c(dfS[,coly],dfHH[,coly],dfBB[,coly])
  abline(0, 1, col=" black", lwd=2)
  text(x=xymax, y=xymin+0.1*(xymax-xymin), adj=1, paste0("R2 = ", round(R2(model_est = yy, measured_val = xx),2)))
  text(x=xymax, y=xymin+0.05*(xymax-xymin),adj=1,  paste0("NSE = ", round(NSE(model_up = yy, meas_up=xx),2)))
  text(x=xymax, y=xymin, adj=1, paste0("RMSE = ",  round(RMSE(model_up = yy, meas_up=xx),2)))
}



UptakeRatios <- function(df = dfHH, Experiment = "hanninghof", Treatments = c("FYM", "FYMNPK", "NPK", "NP", "NK", "PK")){
  df[,"P:N"]<-df[,"Measured_P_Uptake.kg.ha"]/df[,"Measured_N_Uptake.kg.ha"]
  df[,"K:N"]<-df[,"Measured_K_Uptake.kg.ha"]/df[,"Measured_N_Uptake.kg.ha"]
  

  
  dfNPKur <- NULL
  #N:P:K uptakes 
  for(cr in unique(df[,"Crop"])){
    for(Trt in Treatments){
      valPN <- subset(df, Treatment == Trt & Crop == cr)[,"P:N"]
      valKN <- subset(df, Treatment == Trt & Crop == cr)[,"K:N"]
      ii <- which(!is.na(valPN))
      jj <- which(!is.na(valKN))
      dfnpkur <- data.frame(Exp. = Experiment,
                            Crop = cr,
                            Trt = Trt,
                            mean = paste0("1:",round(mean(valPN[ii]),2),":",round(mean(valKN[jj]),2)),
                            stderr = paste0("1:",round(std_err(valPN[ii]),2),":",round(std_err(valKN[jj]),2)),
                            std = paste0("1:",round(std(valPN[ii]),2),":",round(std(valKN[jj]),2)),
                            p0.05 = paste0("1:",round(quantile(valPN[ii],prob=0.05, type=5),2),":",
                                           round(quantile(valKN[jj],prob=0.05, type=5),2)),
                            p0.95 = paste0("1:",round(quantile(valPN[ii],prob=0.95, type=5),2),":",
                                           round(quantile(valKN[jj],prob=0.95, type=5),2)) )
      dfNPKur <- rbind(dfNPKur,dfnpkur)
    }
  }
  return(dfNPKur)
}
dfNPKur_HH <- UptakeRatios(df = dfHH, Experiment = "Hanninghof", Treatments = c("FYM", "FYMNPK", "NPK", "NP", "NK", "PK"))
dfNPKur_BB <- UptakeRatios(df = dfBB, Experiment = "Broadbalk", Treatments = c("FYM", "FYMNPK", "N1PKMg", "N5PKMg", "NP", "PKMg"))
dfNPKur <- rbind(dfNPKur_HH, dfNPKur_BB)

print("N:P:K uptake ratios")
print(dfNPKur)

write.csv(x = dfNPKur, file=  "..//Results//18. Measured uptake ratios NtoPtoK.csv", row.names = FALSE)

#NK treatments#############################################################
plot_rur <- function(TreatmentHH = "NK", 
                     TreatmentBB = "N", 
                     ColNameUptake = "Predicted_P_Uptake.kg.ha",
                     ColNamePool = "P_labile",
                     ColNameQuefts ="CropQUEFTS_Dry.rur_P",
                     textlabel){
  
  dfHH_NK <- subset(dfHH, Treatment == TreatmentHH)
  dfBB_NK <- subset(dfBB, Treatment == TreatmentBB)
  show_plot_rur(dfHH_NK = dfHH_NK, 
                dfBB_NK = dfBB_NK, 
                ColNameUptake = ColNameUptake,
                ColNamePool = ColNamePool,
                ColNameQuefts =ColNameQuefts,
                textlabel = textlabel)
}  
#NK treatments#############################################################

#FYM treatments#############################################################
plot_rur_FYM <- function(TreatmentHH = "FYMNK", 
                         TreatmentBB = "FYM", 
                         ColNameUptake = "Predicted_P_Uptake.kg.ha",
                         ColNamePool = "P_labile",
                         ColNamePoolFYM = "P_labile_FYM",
                         ColNameQuefts ="CropQUEFTS_Dry.rur_P",
                         textlabel){
  
  dfHH_NK <- subset(dfHH, Treatment == TreatmentHH)
  dfBB_NK <- subset(dfBB, Treatment == TreatmentBB)
  dfBB_NK[,ColNamePoolFYM] <- dfBB_NK[,ColNamePool] + dfBB_NK[,ColNamePoolFYM]
  dfHH_NK[,ColNamePoolFYM] <- dfHH_NK[,ColNamePool] + dfHH_NK[,ColNamePoolFYM]
 
  show_plot_rur(dfHH_NK = dfHH_NK, 
                dfBB_NK = dfBB_NK, 
                ColNameUptake = ColNameUptake,
                ColNamePool = ColNamePoolFYM,
                ColNameQuefts =ColNameQuefts,
                textlabel = textlabel)
}
#FYM treatments#############################################################

show_plot_rur <- function(dfHH_NK = dfHH_NK, 
                          dfBB_NK = dfBB_NK, 
                          ColNameUptake = "Predicted_P_Uptake.kg.ha",
                          ColNamePool = "P_labile",
                          ColNameQuefts ="CropQUEFTS_Dry.rur_P",
                          textlabel){
  paramHH <- RCKP_param(type="TSP", CalSetName = "LMpred_FYM_Hanninghof",Crops = c("potato","oats","rye"), UseMeasuredNUptake = FALSE)
  paramBB <- RCKP_param(type="TSP", CalSetName = "LMpred_BBalk", Crops = c("potato","oats","wheat", "beans", "silage maize"), UseMeasuredNUptake = FALSE)
  xymin <- min(c(dfHH_NK[,ColNameUptake] / dfHH_NK[,ColNamePool],
                dfBB_NK[,ColNameUptake] / dfBB_NK[,ColNamePool],
                paramHH[ColNameQuefts][[1]],
                paramBB[ColNameQuefts][[1]]),
              na.rm=TRUE)  
  xymax <- max(c(dfHH_NK[,ColNameUptake] / dfHH_NK[,ColNamePool],
                dfBB_NK[,ColNameUptake] / dfBB_NK[,ColNamePool],
                paramHH[ColNameQuefts][[1]],
                paramBB[ColNameQuefts][[1]]),
                na.rm=TRUE)  

  plot(-100,-100, xlim=c(xymin,xymax), ylim=c(xymin,xymax), xlab="Uptake / Labile pool, kg kg-1 y-1", ylab="rur, kg kg-1 y-1")
  
  dfXY <- NULL
  for(cr in unique(dfHH_NK[,"Crop"])){
    param <- RCKP_param(Crops = cr, type="TSP", CalSetName = "LMpred_FYM_Hanninghof", UseMeasuredNUptake = FALSE)
    ii<-which(dfHH_NK[,"Crop"] == cr)
    dfxy <- data.frame(x=mean(dfHH_NK[ii,ColNameUptake] / dfHH_NK[ii,ColNamePool],na.rm=TRUE),
                       y=as.numeric(param[ColNameQuefts]))
    points(dfxy[,"x"],dfxy[,"y"], col="red", pch=1)
    dfXY<-rbind(dfXY, dfxy)
  }
  for(cr in unique(dfBB_NK[,"Crop"])){
    param <- RCKP_param(Crops = cr, type="TSP", CalSetName = "LMpred_BBalk", UseMeasuredNUptake = FALSE)
    ii<-which(dfBB_NK[,"Crop"] == cr)
    dfxy <- data.frame(x=mean(dfBB_NK[ii,ColNameUptake] / dfBB_NK[ii,ColNamePool],na.rm=TRUE),
                       y=as.numeric(param[ColNameQuefts]))
    points(dfxy[,"x"],dfxy[,"y"], col="black", pch=16)
    dfXY<-rbind(dfXY, dfxy)
  }
  text(x=xymin + 0.1 * (xymax - xymin), y=xymax, labels=textlabel,cex=1.0)
  
  fit<-lm(y~1+x,dfXY)
  sfit <- summary(fit)
  xx=c(min(dfXY[,"x"]),max(dfXY[,"x"]))
  lines(x=xx,y=sfit$coefficients[1]+xx*sfit$coefficients[2], col="red", lwd=2)
  text(x=xymin + 0.5 * (xymax - xymin), y=xymin,
       labels=paste0("y = ",round(sfit$coefficients[1],3) ,
                     " + ",round(sfit$coefficients[2],3),
                     " x. R2 = ", round(sfit$r.squared,2)),
       cex=0.8)
}

pdf("..//Results//18. Measured vs predicted uptakes for combined datasets.pdf")
  par(mfrow=c(2,3),mar=c(2,2,0.5,0.5), oma=c(2,3,0.5,0.5))
 
  plot_meas_pred_com(colx = "Measured_N_Uptake.kg.ha" , coly= "Predicted_N_Uptake.kg.ha")
  legend("topleft",legend=c("Hanninghof", "Siaya", "Broadbalk"),
         col=c("black","red","gold"),
         pch=c(3,17,16), bty="n")
 
  plot_meas_pred_com(colx = "Measured_P_Uptake.kg.ha" , coly= "Predicted_P_Uptake.kg.ha")
  plot_meas_pred_com(colx = "Measured_K_Uptake.kg.ha" , coly= "Predicted_K_Uptake.kg.ha")
 
   
  mtext(expression("Measured uptake, kg ha"^"-1"~y^"-1"), side=1, line=-24, cex=1.25, outer=TRUE)
  mtext(expression("Predicted uptake, kg ha"^"-1"~y^"-1"), side=2, line=0.5, cex=1.25, adj=0.95,outer=TRUE)
dev.off()

#NK treatments#############################################################
pdf("..//Results//18. Estimated vs calibrated rur for P and K.pdf")
  par(mfrow=c(2,2),mar=c(2,2,0.5,0.5), oma=c(2,3,0.5,0.5))
  plot_rur(TreatmentHH = "NK", TreatmentBB = "N", 
           ColNameUptake = "Predicted_P_Uptake.kg.ha",
           ColNamePool = "P_labile",
           ColNameQuefts ="CropQUEFTS_Dry.rur_P",
           textlabel = "a, P")
  
  
  plot_rur(TreatmentHH = "NP", TreatmentBB = "NP", 
           ColNameUptake = "Predicted_K_Uptake.kg.ha",
           ColNamePool = "K_labile",
           ColNameQuefts ="CropQUEFTS_Dry.rur_K",
           textlabel = "b, K")
  
  legend("right",legend=c("Hanninghof", "Broadbalk"),col=c("red","black"),pch=c(1,16), bty="n")
  mtext(expression("Uptake / labile pool, kg kg"^"-1"~y^"-1"), side=1, line=-19, cex=1.25, outer=TRUE)
  mtext(expression("rur, kg kg"^"-1"~y^"-1"), side=2, line=0.5, cex=1.25, adj=0.85,outer=TRUE)
  
  
  par(mfrow=c(2,2),mar=c(2,2,0.5,0.5), oma=c(2,3,0.5,0.5))
  plot_rur_FYM(TreatmentHH = "FYMNK", 
           TreatmentBB = "FYM", 
           ColNameUptake = "Predicted_P_Uptake.kg.ha",
           ColNamePool = "P_labile",
           ColNamePoolFYM = "P_labile_FYM",
           ColNameQuefts ="CropQUEFTS_Dry.rur_P",
           textlabel = "c, FYM P")

  plot_rur_FYM(TreatmentHH = "FYMNP", 
               TreatmentBB = "FYM", 
               ColNameUptake = "Predicted_K_Uptake.kg.ha",
               ColNamePool = "K_labile",
               ColNamePoolFYM = "K_labile_FYM",
               ColNameQuefts ="CropQUEFTS_Dry.rur_K",
               textlabel = "d, FYM K")
  legend("right",legend=c("Hanninghof", "Broadbalk"),col=c("red","black"),pch=c(1,16), bty="n")
  mtext(expression("Uptake labile pool, kg kg"^"-1"~y^"-1"), side=1, line=-19, cex=1.25, outer=TRUE)
  mtext(expression("rur, kg kg"^"-1"~y^"-1"), side=2, line=0.5, cex=1.25, adj=0.85,outer=TRUE)
dev.off()


pdf("..//Results//18. Effect of Mg on P and K Uptake.pdf")
    par(mfrow=c(2,2),mar=c(2,2,0.5,0.5), oma=c(2,3,0.5,0.5))
    UptP <- data.frame(NPK = unname(subset(dfHH, Treatment == "NPK", select="Measured_P_Uptake.kg.ha")),
                       NPKMg = unname(subset(dfHH, Treatment == "NPKMg", select="Measured_P_Uptake.kg.ha")) )
    plotCS(Upt = UptP,trt = "NPK", trtMg = "NPKMg", title = "NPK, P")
    UptP <- data.frame(NPK = unname(subset(dfBB, Treatment == "NPK", select="Measured_P_Uptake.kg.ha")),
                       NPKMg = unname(subset(dfBB, Treatment == "N4PKMg", select="Measured_P_Uptake.kg.ha")) )
    plotCS(Upt = UptP,trt = "NPK", trtMg = "NPKMg")
  
    UptK <- data.frame(NPK = unname(subset(dfHH, Treatment == "NPK", select="Measured_K_Uptake.kg.ha")),
                       NPKMg = unname(subset(dfHH, Treatment == "NPKMg", select="Measured_K_Uptake.kg.ha")) )
    plotCS(Upt = UptK,trt = "NPK", trtMg = "NPKMg", title = "NPK, K")
  
    UptK <- data.frame(NPK = unname(subset(dfBB, Treatment == "NPK", select="Measured_K_Uptake.kg.ha")),
                       NPKMg = unname(subset(dfBB, Treatment == "N4PKMg", select="Measured_K_Uptake.kg.ha")) )
    plotCS(Upt = UptK,trt = "NPK", trtMg = "NPKMg")
  
    UptP <- data.frame(FYMNPK = unname(subset(dfHH, Treatment == "FYMNPK", select="Measured_P_Uptake.kg.ha")),
                       FYMNPKMg = unname(subset(dfHH, Treatment == "FYMNPKMg", select="Measured_P_Uptake.kg.ha")) )
    plotCS(Upt = UptP,trt = "FYMNPK", trtMg = "FYMNPKMg", title = "FYM+NPK, P")
    UptK <- data.frame(FYMNPK = unname(subset(dfHH, Treatment == "FYMNPK", select="Measured_K_Uptake.kg.ha")),
                       FYMNPKMg = unname(subset(dfHH, Treatment == "FYMNPKMg", select="Measured_K_Uptake.kg.ha")) )
    plotCS(Upt = UptK,trt = "FYMNPK", trtMg = "FYMNPKMg", title = "FYM+NPK, K")
   
    legend("bottomright",legend=c("Hanninghof", "Broadbalk"),col=c("black","red"),pch=c(1,1), bty="n")
    mtext(expression("Cumulative uptake without Mg, kg ha"^"-1"), side=1, line=0.5, cex=1.25, outer=TRUE)
    mtext(expression("Cumulative uptake with Mg, kg ha"^"-1"), side=2, line=0.5, cex=1.25, outer=TRUE)
dev.off() 






Ydata <- read.csv("..//Data//Processed//11. Broadbalk_rotation_NPK_offtakes_kgha_MLpredicted.csv", as.is = TRUE)

plot(x=Ydata[,"Year"], y=rep(-100, nrow(Ydata)),xlab = "year", ylab="Rel. effect of Mg omission", 
     ylim=c(-50,100), main="Broadbalk")

cols=rainbow(15)
nr=0
legname = NULL
for(sec in seq(0,9,1)){
  nr=nr+1
  legname[nr] = paste0("section ", sec)
  dfMg <- subset(Ydata, section == sec & Trt == "N4PKMg", select = c("Year", "Yield.tDM.ha", "LMpred.N.offtake.kg.ha", "LMpred.P.offtake.kg.ha", "LMpred.K.offtake.kg.ha"))
  df <- subset(Ydata, section == sec & Trt == "NPK", select = c("Year", "Yield.tDM.ha", "LMpred.N.offtake.kg.ha", "LMpred.P.offtake.kg.ha", "LMpred.K.offtake.kg.ha"))
  lines(df[,"Year"], cumsum(dfMg[,"Yield.tDM.ha"] - df[,"Yield.tDM.ha"]), col=cols[nr], lwd=2)
}
legend("topleft", legend=legname,col=cols, lwd=2, bty="n")
mean(dfMg[,"Yield.tDM.ha"] - df[,"Yield.tDM.ha"],na.rm=TRUE)



##As TIFF files
tiff("..//Results//18. Schut et al. Figure 2.tif",
    width = 15, height = 10, units = "cm",
    compression = c("lzw"),
    bg = "white", res = 600, pointsize = 8,
    restoreConsole = TRUE, family= "serif",
    type = "windows",
    symbolfamily="default")

  par(mfrow=c(2,2),mar=c(2,2,0.5,0.5), oma=c(2,3,0.5,0.5))
  plot_rur(TreatmentHH = "NK", TreatmentBB = "N", 
           ColNameUptake = "Predicted_P_Uptake.kg.ha",
           ColNamePool = "P_labile",
           ColNameQuefts ="CropQUEFTS_Dry.rur_P",
           textlabel = "a, P")
  
  
  plot_rur(TreatmentHH = "NP", TreatmentBB = "NP", 
           ColNameUptake = "Predicted_K_Uptake.kg.ha",
           ColNamePool = "K_labile",
           ColNameQuefts ="CropQUEFTS_Dry.rur_K",
           textlabel = "b, K")
  
  legend("right",legend=c("Hanninghof", "Broadbalk"),col=c("red","black"),pch=c(1,16), bty="n")
  mtext(expression("Uptake / labile pool, kg kg"^"-1"~y^"-1"), side=1, line=-16, cex=1.25, outer=TRUE)
  mtext(expression("rur, kg kg"^"-1"~y^"-1"), side=2, line=0.5, cex=1.25, adj=0.85,outer=TRUE)
dev.off()
  
tiff("..//Results//18. Schut et al. Figure 2a.tif",
       width = 15, height = 10, units = "cm",
       compression = c("lzw"),
       bg = "white", res = 600, pointsize = 8,
       restoreConsole = TRUE, family= "serif",
       type = "windows",
       symbolfamily="default")  
  par(mfrow=c(2,2),mar=c(2,2,0.5,0.5), oma=c(2,3,0.5,0.5))
  plot_rur_FYM(TreatmentHH = "FYMNK", 
               TreatmentBB = "FYM", 
               ColNameUptake = "Predicted_P_Uptake.kg.ha",
               ColNamePool = "P_labile",
               ColNamePoolFYM = "P_labile_FYM",
               ColNameQuefts ="CropQUEFTS_Dry.rur_P",
               textlabel = "c, FYM P")
  
  plot_rur_FYM(TreatmentHH = "FYMNP", 
               TreatmentBB = "FYM", 
               ColNameUptake = "Predicted_K_Uptake.kg.ha",
               ColNamePool = "K_labile",
               ColNamePoolFYM = "K_labile_FYM",
               ColNameQuefts ="CropQUEFTS_Dry.rur_K",
               textlabel = "d, FYM K")
  legend("right",legend=c("Hanninghof", "Broadbalk"),col=c("red","black"),pch=c(1,16), bty="n")
  mtext(expression("Uptake labile pool, kg kg"^"-1"~y^"-1"), side=1, line=-16, cex=1.25, outer=TRUE)
  mtext(expression("rur, kg kg"^"-1"~y^"-1"), side=2, line=0.5, cex=1.25, adj=0.85,outer=TRUE)
dev.off()

tiff("..//Results//18. Schut et al. Figure 3.tif",
     width = 15, height = 10, units = "cm",
     compression = c("lzw"),
     bg = "white", res = 600, pointsize = 8,
     restoreConsole = TRUE, family= "serif",
     type = "windows",
     symbolfamily="default")  

    par(mfrow=c(2,3),mar=c(2,2,0.5,0.5), oma=c(2,3,0.5,0.5))
    
    plot_meas_pred_com(colx = "Measured_N_Uptake.kg.ha" , coly= "Predicted_N_Uptake.kg.ha")
    legend("topleft",legend=c("Hanninghof", "Siaya", "Broadbalk"),
           col=c("black","red","gold"),
           pch=c(3,17,16), bty="n")
    
    plot_meas_pred_com(colx = "Measured_P_Uptake.kg.ha" , coly= "Predicted_P_Uptake.kg.ha")
    plot_meas_pred_com(colx = "Measured_K_Uptake.kg.ha" , coly= "Predicted_K_Uptake.kg.ha")
    
    
    mtext(expression("Measured uptake, kg ha"^"-1"~y^"-1"), side=1, line=-20, cex=1.25, outer=TRUE)
    mtext(expression("Predicted uptake, kg ha"^"-1"~y^"-1"), side=2, line=0.5, cex=1.25, adj=0.95,outer=TRUE)
dev.off()



