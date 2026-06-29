
#INPUTS==================================================================================
dataBB <- read.csv("..//data//Processed//11. Broadbalk_rotation_NPK_offtakes_kgha.csv")
dataHH <- read.csv("..//data//Processed//1. HanningHof_rotation_NPK_offtakes_kgha.csv")
#OUTPUTS=================================================================================
Accumulation_Dilution_pdf <- "..//Results//17. Fitted accumulation and dilution curves.pdf"


#rename maize to silage maize
ii <- which(dataBB[,"Crop"] == "maize")
dataBB[ii,"Crop"] <- "silage maize"

#Change column name for Year to Year
cn <- colnames(dataBB)
cn[cn == "year"] <- "Year"
colnames(dataBB) <- cn


PKparam <- RCKP_param(type = "ASP", Crops = c("potato", "silage maize", "beans", "wheat", "oats", "rye"),UseMeasuredNUptake=FALSE)
parQUEFTS_FM <- data.frame(Crop = PKparam$CropQUEFTS_Fresh.crop,
                        rN = PKparam$CropQUEFTS_Fresh.rN,
                        aN = PKparam$CropQUEFTS_Fresh.aN,
                        dN = PKparam$CropQUEFTS_Fresh.dN,
                        rP = PKparam$CropQUEFTS_Fresh.rP,
                        aP = PKparam$CropQUEFTS_Fresh.aP,
                        dP = PKparam$CropQUEFTS_Fresh.dP,
                        rK = PKparam$CropQUEFTS_Fresh.rK,
                        aK = PKparam$CropQUEFTS_Fresh.aK,
                        dK = PKparam$CropQUEFTS_Fresh.dK,
                        DMc = PKparam$CropQUEFTS_Fresh.DMc)
parQUEFTS_DM <- data.frame(Crop = PKparam$CropQUEFTS_Dry.crop,
                           rN = PKparam$CropQUEFTS_Dry.rN,
                           aN = PKparam$CropQUEFTS_Dry.aN,
                           dN = PKparam$CropQUEFTS_Dry.dN,
                           rP = PKparam$CropQUEFTS_Dry.rP,
                           aP = PKparam$CropQUEFTS_Dry.aP,
                           dP = PKparam$CropQUEFTS_Dry.dP,
                           rK = PKparam$CropQUEFTS_Dry.rK,
                           aK = PKparam$CropQUEFTS_Dry.aK,
                           dK = PKparam$CropQUEFTS_Dry.dK,
                           PNratio_max = PKparam$CropQUEFTS_Dry.PNratio_max,
                           KNratio_max = PKparam$CropQUEFTS_Dry.KNratio_max,
                           DMc = PKparam$CropQUEFTS_Dry.DMc)


#Check accumulation and dilution
pdf(Accumulation_Dilution_pdf)
  par(mfcol=c(3,6), mar=c(1.5,1.5,1,0), oma=c(3,3,0.5,0.5))
  for(cr in c("potato", "silage maize", "beans", "wheat", "oats", "rye")){
    iQ <- which(parQUEFTS_FM[,"Crop"] == cr)
    ii <- which(dataBB[,"Crop"] == cr)
    jj <- which(dataHH[,"Crop"] == cr)
    
    plot(dataBB[ii, "N.offtake.kg.ha"], dataBB[ii, "Yield.t.ha"], 
         xlim=c(0,max(c(dataBB[ii, "N.offtake.kg.ha"], dataHH[jj, "N.offtake.kg.ha"]),na.rm=TRUE)),
         ylim=c(0,max(c(dataBB[ii, "Yield.t.ha"],      dataHH[jj, "Yield.t.ha"]), na.rm=TRUE)),
         main= cr, xlab="Uptake, kg N/ha", ylab="Yield, t FM/ha", pch=1, col="black")
    points(x=dataHH[jj, "N.offtake.kg.ha"], y=dataHH[jj, "Yield.t.ha"], pch=16, col="red")
    abline(a = -parQUEFTS_FM[iQ,"rN"] * parQUEFTS_FM[iQ,"aN"]/1000, b = parQUEFTS_FM[iQ,"aN"]/1000, col = "red", lty=1, lwd=1)
    abline(a = -parQUEFTS_FM[iQ,"rN"] * parQUEFTS_FM[iQ,"dN"]/1000, b = parQUEFTS_FM[iQ,"dN"]/1000, col = "red", lty=3, lwd=2)
    
    plot(dataBB[ii, "P.offtake.kg.ha"],dataBB[ii, "Yield.t.ha"],main="",
         xlim=c(0,max(c(dataBB[ii, "P.offtake.kg.ha"], dataHH[jj, "P.offtake.kg.ha"]),na.rm=TRUE)),
         ylim=c(0,max(c(dataBB[ii, "Yield.t.ha"],      dataHH[jj, "Yield.t.ha"]), na.rm=TRUE)),
         xlab="Uptake, kg P/ha", ylab= "Yield, t/ha")
    points(dataHH[jj, "P.offtake.kg.ha"], dataHH[jj, "Yield.t.ha"], pch=16, col="red")
    abline(a = -parQUEFTS_FM[iQ,"rP"] * parQUEFTS_FM[iQ,"aP"]/1000, b = parQUEFTS_FM[iQ,"aP"]/1000, col = "red", lty=1, lwd=1)
    abline(a = -parQUEFTS_FM[iQ,"rP"] * parQUEFTS_FM[iQ,"dP"]/1000, b = parQUEFTS_FM[iQ,"dP"]/1000, col = "red", lty=3, lwd=2)
    
    plot(dataBB[ii, "K.offtake.kg.ha"],dataBB[ii, "Yield.t.ha"],main="",
         xlim=c(0,max(c(dataBB[ii, "K.offtake.kg.ha"], dataHH[jj, "K.offtake.kg.ha"]),na.rm=TRUE)),
         ylim=c(0,max(c(dataBB[ii, "Yield.t.ha"],      dataHH[jj, "Yield.t.ha"]), na.rm=TRUE)),
         xlab="Uptake, kg K/ha", ylab = "Yield, t/ha")
    points(dataHH[jj, "K.offtake.kg.ha"], dataHH[jj, "Yield.t.ha"], pch=16, col="red")
    abline(a = -parQUEFTS_FM[iQ,"rK"] * parQUEFTS_FM[iQ,"aK"]/1000, b = parQUEFTS_FM[iQ,"aK"]/1000, col = "red", lty=1, lwd=1)
    abline(a = -parQUEFTS_FM[iQ,"rK"] * parQUEFTS_FM[iQ,"dK"]/1000, b = parQUEFTS_FM[iQ,"dK"]/1000, col = "red", lty=3, lwd=2)
  }
  mtext("N (top row), P, K (bottom) uptake, kg/ha",side=1, outer=TRUE, line=1)
  mtext("Yield, t/ha",side=2, outer=TRUE, line=1)
  
  
  #DRY
  par(mfcol=c(3,6), mar=c(2,2,1,0), oma=c(3,3,0,0))
  for(cr in c("potato", "silage maize", "beans", "wheat", "oats", "rye")){
    iQ <- which(parQUEFTS_DM[,"Crop"] == cr)
    ii <- which(dataBB[,"Crop"] == cr)
    jj <- which(dataHH[,"Crop"] == cr)
    
    plot(dataBB[ii, "N.offtake.kg.ha"], dataBB[ii, "Yield.tDM.ha"], 
         xlim=c(0,max(c(dataBB[ii, "N.offtake.kg.ha"], dataHH[jj, "N.offtake.kg.ha"]),na.rm=TRUE)),
         ylim=c(0,max(c(dataBB[ii, "Yield.tDM.ha"],      dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"]), na.rm=TRUE)),
         main= cr, xlab="Uptake, kg N/ha", ylab="Yield, t DM/ha", pch=1, col="black")
    points(x=dataHH[jj, "N.offtake.kg.ha"], y=dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"], pch=16, col="red")
    abline(a = -parQUEFTS_DM[iQ,"rN"] * parQUEFTS_DM[iQ,"aN"]/1000, b = parQUEFTS_DM[iQ,"aN"]/1000, col = "red", lty=1, lwd=1)
    abline(a = -parQUEFTS_DM[iQ,"rN"] * parQUEFTS_DM[iQ,"dN"]/1000, b = parQUEFTS_DM[iQ,"dN"]/1000, col = "red", lty=3, lwd=2)
    
    plot(dataBB[ii, "P.offtake.kg.ha"],dataBB[ii, "Yield.tDM.ha"],main="",
         xlim=c(0,max(c(dataBB[ii, "P.offtake.kg.ha"], dataHH[jj, "P.offtake.kg.ha"]),na.rm=TRUE)),
         ylim=c(0,max(c(dataBB[ii, "Yield.tDM.ha"],      dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"]), na.rm=TRUE)),
         xlab="Uptake, kg P/ha", ylab= "Yield, t/ha")
    points(dataHH[jj, "P.offtake.kg.ha"], dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"], pch=16, col="red")
    abline(a = -parQUEFTS_DM[iQ,"rP"] * parQUEFTS_DM[iQ,"aP"]/1000, b = parQUEFTS_DM[iQ,"aP"]/1000, col = "red", lty=1, lwd=1)
    abline(a = -parQUEFTS_DM[iQ,"rP"] * parQUEFTS_DM[iQ,"dP"]/1000, b = parQUEFTS_DM[iQ,"dP"]/1000, col = "red", lty=3, lwd=2)
    
    plot(dataBB[ii, "K.offtake.kg.ha"],dataBB[ii, "Yield.tDM.ha"],main="",
         xlim=c(0,max(c(dataBB[ii, "K.offtake.kg.ha"], dataHH[jj, "K.offtake.kg.ha"]),na.rm=TRUE)),
         ylim=c(0,max(c(dataBB[ii, "Yield.tDM.ha"],      dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"]), na.rm=TRUE)),
         xlab="Uptake, kg K/ha", ylab = "Yield, t/ha")
    points(dataHH[jj, "K.offtake.kg.ha"], dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"], pch=16, col="red")
    abline(a = -parQUEFTS_DM[iQ,"rK"] * parQUEFTS_DM[iQ,"aK"]/1000, b = parQUEFTS_DM[iQ,"aK"]/1000, col = "red", lty=1, lwd=1)
    abline(a = -parQUEFTS_DM[iQ,"rK"] * parQUEFTS_DM[iQ,"dK"]/1000, b = parQUEFTS_DM[iQ,"dK"]/1000, col = "red", lty=3, lwd=2)
  }
  mtext("N (top row), P, K (bottom) uptake, kg/ha",side=1, outer=TRUE, line=1)
  mtext("Yield, t/ha",side=2, outer=TRUE, line=1)  
  
dev.off()



#DRY
par(mfrow=c(2,3), mar=c(2,2,1,0), oma=c(3,3,0,0))
for(cr in c("wheat","potato")){
  iQ <- which(parQUEFTS_DM[,"Crop"] == cr)
  ii <- which(dataBB[,"Crop"] == cr)
  jj <- which(dataHH[,"Crop"] == cr)
  
  plot(dataBB[ii, "N.offtake.kg.ha"], dataBB[ii, "Yield.tDM.ha"], 
       xlim=c(0,max(c(dataBB[ii, "N.offtake.kg.ha"], dataHH[jj, "N.offtake.kg.ha"]),na.rm=TRUE)),
       ylim=c(0,max(c(dataBB[ii, "Yield.tDM.ha"],      dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"]), na.rm=TRUE)),
       main= cr, xlab="Uptake, kg N/ha", ylab="Yield, t DM/ha", pch=1, col="black")
  points(x=dataHH[jj, "N.offtake.kg.ha"], y=dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"], pch=16, col="red")
  abline(a = -parQUEFTS_DM[iQ,"rN"] * parQUEFTS_DM[iQ,"aN"]/1000, b = parQUEFTS_DM[iQ,"aN"]/1000, col = "red", lty=1, lwd=1)
  abline(a = -parQUEFTS_DM[iQ,"rN"] * parQUEFTS_DM[iQ,"dN"]/1000, b = parQUEFTS_DM[iQ,"dN"]/1000, col = "red", lty=3, lwd=2)
  
  plot(dataBB[ii, "P.offtake.kg.ha"],dataBB[ii, "Yield.tDM.ha"],main="",
       xlim=c(0,max(c(dataBB[ii, "P.offtake.kg.ha"], dataHH[jj, "P.offtake.kg.ha"]),na.rm=TRUE)),
       ylim=c(0,max(c(dataBB[ii, "Yield.tDM.ha"],      dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"]), na.rm=TRUE)),
       xlab="Uptake, kg P/ha", ylab= "Yield, t/ha")
  points(dataHH[jj, "P.offtake.kg.ha"], dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"], pch=16, col="red")
  abline(a = -parQUEFTS_DM[iQ,"rP"] * parQUEFTS_DM[iQ,"aP"]/1000, b = parQUEFTS_DM[iQ,"aP"]/1000, col = "red", lty=1, lwd=1)
  abline(a = -parQUEFTS_DM[iQ,"rP"] * parQUEFTS_DM[iQ,"dP"]/1000, b = parQUEFTS_DM[iQ,"dP"]/1000, col = "red", lty=3, lwd=2)

  aP <- parQUEFTS_DM[iQ,"dN"]/parQUEFTS_DM[iQ,"PNratio_max"]
  abline(a = -parQUEFTS_DM[iQ,"rP"] * aP/1000, b = aP/1000, col = "red", lty=5, lwd=3)
  
    
  plot(dataBB[ii, "K.offtake.kg.ha"],dataBB[ii, "Yield.tDM.ha"],main="",
       xlim=c(0,max(c(dataBB[ii, "K.offtake.kg.ha"], dataHH[jj, "K.offtake.kg.ha"]),na.rm=TRUE)),
       ylim=c(0,max(c(dataBB[ii, "Yield.tDM.ha"],      dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"]), na.rm=TRUE)),
       xlab="Uptake, kg K/ha", ylab = "Yield, t/ha")
  points(dataHH[jj, "K.offtake.kg.ha"], dataHH[jj, "Yield.t.ha"] * parQUEFTS_DM[iQ,"DMc"], pch=16, col="red")
  abline(a = -parQUEFTS_DM[iQ,"rK"] * parQUEFTS_DM[iQ,"aK"]/1000, b = parQUEFTS_DM[iQ,"aK"]/1000, col = "red", lty=1, lwd=1)
  abline(a = -parQUEFTS_DM[iQ,"rK"] * parQUEFTS_DM[iQ,"dK"]/1000, b = parQUEFTS_DM[iQ,"dK"]/1000, col = "red", lty=3, lwd=2)
  aK <- parQUEFTS_DM[iQ,"dN"]/parQUEFTS_DM[iQ,"KNratio_max"]
  abline(a = -parQUEFTS_DM[iQ,"rK"] * aK/1000, b = aK/1000, col = "red", lty=5, lwd=3)
}
mtext("N (top row), P, K (bottom) uptake, kg/ha",side=1, outer=TRUE, line=1)
mtext("Yield, t/ha",side=2, outer=TRUE, line=1)  

