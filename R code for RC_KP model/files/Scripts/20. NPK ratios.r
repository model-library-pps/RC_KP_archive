datafileN <- "..//Data//Processed//1. HanningHof_rotation_NPK_offtakes_kgha.csv"
resultfileN <- "..//Results//3. Estimated N uptake for parameter set  LMpred_FYM_Hanninghof .csv"
resultfileP <- "..//Results//3. Estimated P uptake for parameter set  LMpred_FYM_Hanninghof .csv"
resultfileK <- "..//Results//3. Estimated K uptake for parameter set  LMpred_FYM_Hanninghof .csv"

ResultPDF <- "..//Results//20. Dynamics of NPK ratios per treatment LMpred_FYM_Hanninghof.pdf"

df<-read.csv(datafileN)
cn <- colnames(df)
cn[cn=="Description"]<-"Trt"
colnames(df)<-cn
utrt <- unique(df[,"Trt"])

dfN<-read.csv(resultfileN)
dfP<-read.csv(resultfileP)
dfK<-read.csv(resultfileK)

plotRatios <- function(SelTrt, crops = c("potato","rye","oats"),ratio="K:N", addlegend=FALSE){
  MeasRatios<-subset(df, Trt == SelTrt, select=c("Year", "Crop", "Trt", "N.offtake.kg.ha", "P.offtake.kg.ha", "K.offtake.kg.ha"))
  MeasRatios["meas.K:N"] <- MeasRatios[,"K.offtake.kg.ha"] / MeasRatios[,"N.offtake.kg.ha"]
  MeasRatios["meas.P:N"] <- MeasRatios[,"P.offtake.kg.ha"] / MeasRatios[,"N.offtake.kg.ha"]
  MeasRatios["meas.K:P"] <- MeasRatios[,"K.offtake.kg.ha"] / MeasRatios[,"P.offtake.kg.ha"]
  
  
  PredRatios <- subset(dfN, select=c("Year", "Crop"))
  PredRatios[,"Trt"] <- SelTrt
  PredRatios["pred.K:N"] <- dfK[,paste0("pred_K_UP_",SelTrt)] / dfN[,paste0("pred_N_UP_",SelTrt)]
  PredRatios["pred.P:N"] <- dfP[,paste0("pred_P_UP_",SelTrt)] / dfN[,paste0("pred_N_UP_",SelTrt)]
  PredRatios["pred.K:P"] <- dfK[,paste0("pred_K_UP_",SelTrt)] / dfP[,paste0("pred_P_UP_",SelTrt)]
  PredRatios <- merge(PredRatios,MeasRatios, by=c("Year", "Crop", "Trt"))
  
  ucr <- c("potato","rye","oats")
  cols<-c("black","red","orange")
  pchs=c(0,15,16)
  nr<-0
  maxv <- max(c(PredRatios[,paste0("meas.",ratio)], PredRatios[,paste0("pred.",ratio)]), na.rm=TRUE)
  
  plot(-5,-5,xlab=paste0("measured", ratio),ylab=paste0("predicted", ratio), xlim=c(0,maxv),ylim=c(0,maxv))
  for( cr in ucr){
    nr=nr+1
    ii <- which(PredRatios[,"Crop"] ==ucr[nr])
    points(PredRatios[ii,paste0("meas.",ratio)],PredRatios[ii,paste0("pred.",ratio)],col=cols[nr], pch=pchs[nr])
  } 
  abline(0,1,col="black",lwd=2)
  if(addlegend){
    legend("bottomright",legend=ucr, col=c(cols),pch=pchs, bty="n")
  }

  plot(x=c(1961,2021),y=c(-5,-5),ylim=c(0,maxv),xlab="Year", ylab=ratio)
  nr=0
  for( cr in ucr){
    nr=nr+1
    i<-which(PredRatios[,"Crop"]==cr)
    points(PredRatios[i,"Year"],PredRatios[i,paste0("meas.",ratio)],col=cols[nr], pch=1)
    points(PredRatios[i,"Year"],PredRatios[i,paste0("pred.",ratio)],col=cols[nr], pch=15)
  }
  if(addlegend){
    legend("bottomright",legend=ucr, col=c(cols),pch=15, bty="n")
    legend("topright",legend=c("M.","P."), col="black",pch=c(1,15),bty="n")
  }
}
AddAxisText <- function(SelTrt){
  mtext(text=SelTrt,side=3,outer=TRUE,line=0,adj=0.5)
  mtext(text="K:N",side=2,outer=TRUE,line=0,adj=0.85)
  mtext(text="P:N",side=2,outer=TRUE,line=0,adj=0.5)
  mtext(text="K:P",side=2,outer=TRUE,line=0,adj=0.15)
  mtext(text="Predicted",side=2,outer=TRUE,line=1.5,adj=0.5)
  mtext(text="Measured",side=1,outer=TRUE,line=0.5,adj=0.25)
}

pdf(ResultPDF)
for(seltrt in c("Control", "NP", "NK", "PK", "N", "NPK", "NPKMg")){
  par(mfrow=c(3,2),mar=c(2,2,0.25,0.25), oma=c(2,3,1,0.5))
  plotRatios(SelTrt = seltrt, crops = c("potato","rye","oats"),ratio="K:N", addlegend=TRUE)
  plotRatios(SelTrt = seltrt, crops = c("potato","rye","oats"),ratio="P:N")
  plotRatios(SelTrt = seltrt, crops = c("potato","rye","oats"),ratio="K:P")
  AddAxisText(SelTrt = seltrt)
}
dev.off()




plotmeasuredNPK <- function(SelCr, SelTrt = c("NP","NK","PK","FYM+NPKMg"),ratio="K:N", addlegend=FALSE){
  iQ <- which(parQUEFTS_DM[,"Crop"] == SelCr)
  
  dfS <- subset(df, Crop == SelCr)
  cols<-c("blue","orange","red","black")
  pchs=c(15,16, 17, 1)

  nr<-0
  maxxv <- max(dfS[,"N.offtake.kg.ha"], na.rm=TRUE)
  maxyv <- max(dfS[,"K.offtake.kg.ha"], na.rm=TRUE)
  
  plot(-5,-5,xlab="N uptake",ylab="K uptake", xlim=c(0,maxxv),ylim=c(0,maxyv))
  for( trt in SelTrt){
    nr=nr+1
    ii <- which(dfS[,"Trt"] == trt)
    points(dfS[ii,"N.offtake.kg.ha"],dfS[ii,"K.offtake.kg.ha"],col=cols[nr], pch=pchs[nr])
  } 
  abline(a = 0, b = parQUEFTS_DM[iQ,"KNratio_max"], col = "black", lty=5, lwd=3)
  

  
  nr=0
  maxyv <- max(dfS[,"P.offtake.kg.ha"], na.rm=TRUE)
  plot(-5,-5,xlab="N uptake",ylab="P uptake", xlim=c(0,maxxv),ylim=c(0,maxyv))
  for( trt in SelTrt){
    nr=nr+1
    ii <- which(dfS[,"Trt"] == trt)
    points(dfS[ii,"N.offtake.kg.ha"],dfS[ii,"P.offtake.kg.ha"],col=cols[nr], pch=pchs[nr])
  }
  abline(a = 0, b = parQUEFTS_DM[iQ,"PNratio_max"], col = "black", lty=5, lwd=3)
  
  if(addlegend){
    legend("topleft",legend=SelTrt, col=c(cols),pch=pchs, bty="n")
  }  
}


PKparam <- RCKP_param(type = "ASP", Crops = c("potato", "silage maize", "beans", "wheat", "oats", "rye"),UseMeasuredNUptake=FALSE)

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

par(mfrow=c(3,2),mar=c(2,2,0.25,0.25), oma=c(2,3,1,0.5))
  plotmeasuredNPK(SelCr = "potato", SelTrt = c("NP","NK","PK","FYM+NPKMg"), addlegend=FALSE)
  plotmeasuredNPK(SelCr = "oats", SelTrt = c("NP","NK","PK","FYM+NPKMg"), addlegend=FALSE)
  plotmeasuredNPK(SelCr = "rye", SelTrt = c("NP","NK","PK","FYM+NPKMg"), addlegend=TRUE)
  mtext(text=SelTrt,side=3,outer=TRUE,line=0,adj=0.5)
  mtext(text="K or P uptake, kg/ha",side=2,outer=TRUE,line=1.25,adj=0.5)
  mtext(text="N uptake, kg/ha",side=1,outer=TRUE,line=0.5,adj=0.5)
dev.off()
