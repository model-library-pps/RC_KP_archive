source("00. General figures.r")


##HELP FUNCTION FOR PLOTTING EXPERIMENTAL DATA PROCESSING###############################################
plotCumOfftakes_LMpred <- function(df){
  iNPKMg <- which(df[,"Trt"] == "N5PKMg")  #green
  iNPK <- which(df[,"Trt"] == "NPK")  #green
  iNP <- which(df[,"Trt"] == "NP")   #blue
  iN <- which(df[,"Trt"] == "N")    #orange
  iPKMg <- which(df[,"Trt"] == "PKMg")    #cyan
  iC <- which(df[,"Trt"] == "Control") #red
  iFYMNPK <- which(df[,"Trt"] == "FYM+NPK")  #brown
  iFYM <- which(df[,"Trt"] == "FYM")   #black

  par(mfrow=c(2,3),mar=c(1.5,1.5, 1, 0.5), oma=c(2, 2, 0.5, 0.5))
  
  y_axis_figures <- c("LMpred.N.offtake.kg.ha","LMpred.P.offtake.kg.ha","LMpred.K.offtake.kg.ha")
  PlotTitles <- c( "N", "P", "K")
  for (i in 1:length(y_axis_figures)){
    y_axes <- y_axis_figures[i]
    plot( df[iC,"Year"],cumsum(df[iC,y_axes]),col="red",
          xlab="Year", ylab=y_axes, type = "l" , 
          ylim = c(0,max(c(cumsum(df[iNPKMg,y_axes]),
                           cumsum(df[iFYM,y_axes]),
                           cumsum(df[iFYMNPK,y_axes])), na.rm= TRUE)),
          main= PlotTitles[i])
    lines(df[iPKMg,"Year"],cumsum(df[iPKMg,y_axes]),col="cyan")
    lines(df[iN,"Year"],cumsum(df[iN,y_axes]),col="orange")
    lines(df[iNPK,"Year"],cumsum(df[iNPK,y_axes]),col="lightgreen")
    lines(df[iNP,"Year"],cumsum(df[iNP,y_axes]),col="blue")
    lines(df[iNPKMg,"Year"],cumsum(df[iNPKMg,y_axes]),col="darkgreen")
    lines(df[iFYMNPK,"Year"],cumsum(df[iFYMNPK,y_axes]),col="brown", lty=3, lwd=2)
    lines(df[iFYM,"Year"],cumsum(df[iFYM,y_axes]),col="black", lty=3, lwd=2)
  }
  legend("topleft",
         legend= c("N5PKMg"   , "NPK"       , "NP"  , "N"     , "PKMg", "Control", "FYMNPK", "FYM"), 
         col   = c("darkgreen", "lightgreen", "blue", "orange", "cyan", "red"    , "brown" , "black"),
         lty   = c(1,1,1,1,1,1,3,3),
         lwd   = c(1,1,1,1,1,1,2,2),bty = "n")
  
  
  x_axis_figures <- c("N.rate.kg.ha","P.rate.kg.ha","K.rate.kg.ha")
  y_axis_text <- c("N recovery, cum. kg/kg","P recovery,  cum. kg/kg","K recovery,  cum. kg/kg")
  x_axis2_figures <- c("FYMN.rate.kg.ha","FYMP.rate.kg.ha","FYMK.rate.kg.ha")
  ymax <- c(1.2, 0.8, 1.4)
  
  #determine totals
   for (i in 1:length(x_axis_figures)){
     df[,x_axis_figures[i]] <- df[,x_axis_figures[i]] + df[,x_axis2_figures[i]]
   }

  for (i in 1:length(y_axis_figures)){
    x_axes <- x_axis_figures[i]
    y_axes <- y_axis_figures[i]
    plot( df[iC,"Year"],cumsum(df[iC,y_axes]),col="red",
          xlab="Year", ylab=y_axis_text[i], type = "l" , 
          ylim = c(0,ymax[i]))
    lines(df[iPKMg,"Year"],cumsum(df[iPKMg,y_axes]) / cumsum(df[iPKMg,x_axes]),col="cyan")
    lines(df[iN,"Year"],cumsum(df[iN,y_axes]) / cumsum(df[iN,x_axes]),col="orange")
    lines(df[iNPK,"Year"],cumsum(df[iNPK,y_axes]) / cumsum(df[iNPK,x_axes]),col="lightgreen")
    lines(df[iNP,"Year"],cumsum(df[iNP,y_axes]) / cumsum(df[iNP,x_axes]),col="blue")
    lines(df[iNPKMg,"Year"],cumsum(df[iNPKMg,y_axes]) / cumsum(df[iNPKMg,x_axes]),col="darkgreen")
    lines(df[iFYMNPK,"Year"],cumsum(df[iFYMNPK,y_axes]) / cumsum(df[iFYMNPK,x_axes]),col="brown", lty=3, lwd=2)
    lines(df[iFYM,"Year"],cumsum(df[iFYM,y_axes]) / cumsum(df[iFYM,x_axes]),col="black", lty=3, lwd=2)
  }
  mtext("Recovery, cum. kg/kg", adj=0.15, side = 2, outer=TRUE, line=1)
  mtext("Cum. uptake, kg/ha", adj=0.85, side = 2, outer=TRUE, line=1)
}



plotCropOfftakes <- function(df, crop = "potato"){
  iNPK <- which(df[,"Trt"] == "FYM+NPK" & df[,"Crop"] == crop)  #red
  iFYM <- which(df[,"Trt"] == "FYM" & df[,"Crop"] == crop)  #red

  par(mfrow=c(2,3),mar=c(4,4,1,1))
  y_axis_figures <- c("N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha")
  for (y_axes in y_axis_figures){
    maxY <- max(df[iNPK,y_axes],na.rm = TRUE)
    
    plot( df[iNPK,"Year"],df[iNPK,y_axes],col="red",lty=1,
          xlab="Year", ylab=y_axes, type = "l", ylim = c(0,maxY))
    lines(df[iFYM,"Year"],df[iFYM,y_axes],col="black", lty=4)
  }
  legend("bottomleft",legend=c("FYM+NPK", "FYM"), 
         col=c("red", "black"),
         lty =c(1,4), bty = "n")
  
  iNPK <- which(df[,"Trt"] == "N5PKMg" & df[,"Crop"] == crop)  #red
  iNP <- which(df[,"Trt"] == "NP" & df[,"Crop"] == crop)  #red
  iN <- which(df[,"Trt"] == "N" & df[,"Crop"] == crop)  #red
  iPK <- which(df[,"Trt"] == "PKMg" & df[,"Crop"] == crop)  #red
  
  y_axis_figures <- c("N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha")
  for (y_axes in y_axis_figures){
    maxY <- max(df[iNPK,y_axes],na.rm = TRUE)
    
    plot( df[iNPK,"Year"],df[iNPK,y_axes],col="red",lty=1,
          xlab="Year", ylab=y_axes, type = "l", ylim = c(0,maxY))
    lines(df[iNP,"Year"],df[iNP,y_axes],col="green", lty=2)
    lines(df[iN,"Year"],df[iN,y_axes],col="blue", lty=3)
    lines(df[iPK,"Year"],df[iPK,y_axes],col="black", lty=4)
  }
  legend("bottomleft",legend=c("N5PKMg", "NP", "N", "PKMg"), 
         col=c("red", "green", "blue", "black"),
         lty =c(1,2,3,4), bty = "n")
  
}

plotCropYieldOfftakes <- function(df, CropQUEFTS_param){
  
  potatoDMc = 0.22      #g(DM) g(FM)-1
  oatsDMc = 0.88      #g(DM) g(FM)-1
  ryeDMc = 0.88      #g(DM) g(FM)-1
  
  
  uyr <- unique(df[,"Year"])
  cols <- rainbow(length(uyr))
  xx <- seq(0,1000,1)
  
  par(mfcol=c(3,3), mar=c(1,1,0.2,0.2), oma=c(2,2,1,1))
  for (crop in c("potato","rye","oats")){
    icrop <- which(df[,"Crop"] == crop) 
    iQCrop <- which(as.character(CropQUEFTS_param$CropQUEFTS_Dry.crop) == crop)
    
    #Convert yields to kg matter
    df[icrop,"Yield.kg.DM.ha"] <- df[icrop,"Yield.t.ha"] * CropQUEFTS_param$CropQUEFTS_Dry.DMc[iQCrop] * 1000
    
    
    aN <- -CropQUEFTS_param$CropQUEFTS_Dry.rN[iQCrop] + xx * CropQUEFTS_param$CropQUEFTS_Dry.aN[iQCrop]
    dN <- -CropQUEFTS_param$CropQUEFTS_Dry.rN[iQCrop] + xx * CropQUEFTS_param$CropQUEFTS_Dry.dN[iQCrop]
    
    plot( c(-10,-5),c(-10,-5),col="green",
          xlim=c(0,max(df[icrop, "N.offtake.kg.ha"], na.rm = TRUE)), ylim = c(0,max(df[icrop,"Yield.kg.DM.ha"], na.rm = TRUE)),
          xlab="N.offtake.kg.ha", ylab="DM yield, kg/ha", type = "p", main=crop)
    nr <- 0
    for(y in uyr){
      nr <- nr +1
      icropy <- which(df[,"Crop"] == crop & df[,"Year"] == y) 
      points( df[icropy, "N.offtake.kg.ha" ],df[icropy,"Yield.kg.DM.ha"],col=cols[nr])
    }
    lines(xx,aN,lty=3, col="black",lwd=2)
    lines(xx,dN,lty=3, col="red",lwd=2)
  }
  for (crop in c("potato","rye","oats")){
    icrop <- which(df[,"Crop"] == crop) 
    iQCrop <- which(as.character(CropQUEFTS_param$CropQUEFTS_Dry.crop) == crop)
    aP <- -CropQUEFTS_param$CropQUEFTS_Dry.rP[iQCrop] + xx * CropQUEFTS_param$CropQUEFTS_Dry.aP[iQCrop]
    dP <- -CropQUEFTS_param$CropQUEFTS_Dry.rP[iQCrop] + xx * CropQUEFTS_param$CropQUEFTS_Dry.dP[iQCrop]
    plot( c(-10,-5),c(-10,-5),col="green",
          xlim=c(0,max(df[icrop, "P.offtake.kg.ha"], na.rm = TRUE)), ylim = c(0,max(df[icrop,"Yield.kg.DM.ha"], na.rm = TRUE)),
          xlab="P.offtake.kg.ha", ylab="DM yield, kg/ha", type = "p", main=crop)
    nr <- 0
    for(y in uyr){
      nr <- nr +1
      icropy <- which(df[,"Crop"] == crop & df[,"Year"] == y) 
      points( df[icropy, "P.offtake.kg.ha" ],df[icropy,"Yield.kg.DM.ha"],col=cols[nr])
    }
    lines(xx,aP,lty=3, col="black",lwd=2)
    lines(xx,dP,lty=3, col="red",lwd=2)
  }
  for (crop in c("potato","rye","oats")){
    icrop <- which(df[,"Crop"] == crop) 
    iQCrop <- which(as.character(CropQUEFTS_param$CropQUEFTS_Dry.crop) == crop)
    aK <- -CropQUEFTS_param$CropQUEFTS_Dry.rK[iQCrop] + xx * CropQUEFTS_param$CropQUEFTS_Dry.aK[iQCrop]
    dK <- -CropQUEFTS_param$CropQUEFTS_Dry.rK[iQCrop] + xx * CropQUEFTS_param$CropQUEFTS_Dry.dK[iQCrop]
    plot( c(-10,-5),c(-10,-5),col="green",
          xlim=c(0,max(df[icrop, "K.offtake.kg.ha"], na.rm = TRUE)), ylim = c(0,max(df[icrop,"Yield.kg.DM.ha"], na.rm = TRUE)),
          xlab="K.offtake.kg.ha", ylab="DM yield, kg/ha", type = "p", main=crop)
    nr <- 0
    for(y in uyr){
      nr <- nr +1
      icropy <- which(df[,"Crop"] == crop & df[,"Year"] == y) 
      points( df[icropy, "K.offtake.kg.ha" ],df[icropy,"Yield.kg.DM.ha"],col=cols[nr])
    }
    lines(xx,aK,lty=3, col="black",lwd=2)
    lines(xx,dK,lty=3, col="red",lwd=2)
  }
  mtext("N, P and K offtake, kg/ha", 1, outer=TRUE, line=1)
  mtext("Yield.kg.DM.ha", 2, outer=TRUE, line=1)
  legend("bottomright",legend=uyr[c(1,floor(c(0.2,0.4,0.6, 0.8, 1)*length(uyr)))], 
         col = cols[c(1,floor(c(0.2,0.4,0.6, 0.8, 1)*length(uyr)))], pch=1,bty="n")
}

plotNPKRecovery <- function(df, prefix ="rec_", x_axis=c("N","P","K")){
  par(mfrow=c(3,3),mar=c(2,2,1,1))
  
  for(cr in c("potato","wheat", "maize")){
    ii <- which(df[,"Crop"] == cr & df[,"Trt"] == "N2PKMg")
    jj <- which(df[,"Crop"] == cr & df[,"Trt"] == "FYM+NPK")
    for (i_ax in 1:length(x_axis)){
      x_ax <- paste0(prefix, x_axis[i_ax])
      allval <- c(df[ii, x_ax],df[jj, x_ax])
      if( length( which(!is.na(allval)) ) > 0 ){
        ymin=min(c(0,allval), na.rm = TRUE)
        ymax=max(allval, na.rm = TRUE)
        plot(df[ii,"Year"],df[ii,x_ax],type="b", col="red", lty=1, pch=1,
             ylim=c(ymin, ymax), 
             main=paste0(x_axis[i_ax], " ", cr))
        lines( df[jj,"Year"],df[jj,x_ax], col="black", lty =2)
        points(df[jj,"Year"],df[jj,x_ax], col="black", pch =2)
      }
    }
  }
  legend("bottomright", legend=c("Synt.","Org + synt"), 
         col = c("red","black"), lty=c(1, 2), pch=c(1,2), bty="n")
}

plotCumApplication_vs_CumLMpredOfftake <- function(sRP = sortRotationPred, title = ""){
  par(mfcol=c(2,2),mar=c(4,4,1,1))
  seltreat = c("FYM+NPK", "N2PKMg", "N4PKMg", "N5PKMg",    "NP", "PKMg",  "FYM",    "NPK","N","Control")
  cols=      c("gold","lightgreen", "green", "darkgreen","blue", "orange","brown","magenta","cyan","red")
  ltys=c(1,1,1,1,1, 5,5,5,5,5)
  
  
  xymax <- max(c(sRP[,"cum.N.rate.kg.ha"],sRP[,"cum.LMpred.N.offtake.kg.ha"]), na.rm=TRUE)
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,xymax),ylim=c(0,xymax),main=title,
       xlab="Cumulative N application, kg / ha",
       ylab="Cumulative pred. N uptake, kg / ha")
  abline(0,1,col="black", lty=1, lwd=1)
  
  nr = 0
  for(treat in seltreat){
    nr=nr+1
    ii<-which(sRP[,"Trt"] == treat)
    lines(sRP[ii,"cum.N.rate.kg.ha"],sRP[ii,"cum.LMpred.N.offtake.kg.ha"], 
          col = cols[nr], lty = ltys[nr], lwd=2)
  }
  
  xymax <- max(c(sRP[,"cum.P.rate.kg.ha"],sRP[,"cum.LMpred.P.offtake.kg.ha"]), na.rm=TRUE)
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,xymax),ylim=c(0,xymax),main=title,
       xlab="Cumulative P application, kg / ha",
       ylab="Cumulative pred. P uptake, kg / ha")
  abline(0,1,col="black", lty=1, lwd=1)
  nr=0
  for(treat in seltreat){
    nr=nr+1
    ii<-which(sRP[,"Trt"] == treat)
    lines(sRP[ii,"cum.P.rate.kg.ha"],sRP[ii,"cum.LMpred.P.offtake.kg.ha"], 
          col = cols[nr], lty = ltys[nr], lwd=2)
  }
  
  xymax <- max(c(sRP[,"cum.K.rate.kg.ha"],sRP[,"cum.LMpred.K.offtake.kg.ha"]), na.rm=TRUE)
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,xymax),ylim=c(0,xymax),main=title,
       xlab="Cumulative K application, kg / ha",
       ylab="Cumulative pred. K uptake, kg / ha")
  abline(0,1,col="black", lty=1, lwd=1)
  nr=0
  for(treat in seltreat){
    nr=nr+1
    ii<-which(sRP[,"Trt"] == treat)
    lines(sRP[ii,"cum.K.rate.kg.ha"],sRP[ii,"cum.LMpred.K.offtake.kg.ha"], 
          col=cols[nr], lty =ltys[nr], lwd=2)
  }
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,max(sRP[,"cum.K.rate.kg.ha"], na.rm=TRUE)), 
       ylim=c(0,max(sRP[,"cum.LMpred.K.offtake.kg.ha"], na.rm=TRUE)),
       xlab=" ",
       ylab=" ")
  nr=0
  legend("topleft",legend=seltreat, col=cols, lty=ltys, lwd=2, bty="n")
}

plotSoilP_vs_Pofftake <- function(sortRotation = sortRotation){
  par(mfrow=c(2,2),mar=c(2,2,1,0),oma=c(2,2,0,1))
  ii <- which(sortRotation[,"Trt"] == "NPK" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "NPK" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "NPK" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ii,"P.offtake.kg.ha"],
       main="NPK",xlim=c(0,40), ylim=c(0,40),type="p",col="black")
  points(sortRotation[ij,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ij,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="red")
  points(sortRotation[jj,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[jj,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="orange")
  legend("topleft",legend=c("Potato","Oats","Rye"),col=c("black","red","orange"), pch=1, bty="n")
  
  ii <- which(sortRotation[,"Trt"] == "Control" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "Control" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "Control" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ii,"P.offtake.kg.ha"],
       main="Control", xlim=c(0,40), ylim=c(0,40),type="p",col="black")
  points(sortRotation[ij,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ij,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="red")
  points(sortRotation[jj,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[jj,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="orange")
  
  ii <- which(sortRotation[,"Trt"] == "FYM" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "FYM" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "FYM" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ii,"P.offtake.kg.ha"],
       main="FYM", xlim=c(0,40), ylim=c(0,40),type="p",col="black")
  points(sortRotation[ij,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ij,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="red")
  points(sortRotation[jj,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[jj,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="orange")
  
  ii <- which(sortRotation[,"Trt"] == "NK" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "NK" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "NK" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ii,"P.offtake.kg.ha"],
       main="NK", xlim=c(0,40), ylim=c(0,40),type="p",col="black")
  points(sortRotation[ij,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[ij,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="red")
  points(sortRotation[jj,"P-CAL_0-30.(mg.P2O5/100g)"],sortRotation[jj,"P.offtake.kg.ha"],
         xlab="P-CAL, mg(P2O5)/100g" ,ylab="P offtake, kg/ha", type="p",col="orange")
  mtext("P-CAL, mg(P2O5)/100g", side=1, outer=TRUE, line=0.5)
  mtext("P offtake, kg/ha", side=2, outer=TRUE, line=0.5)
  
}

plotSoilK_vs_Kofftake <- function(sortRotation = sortRotation){
  par(mfrow=c(2,2),mar=c(2,2,1,0),oma=c(2,2,0,1))
  ii <- which(sortRotation[,"Trt"] == "NPK" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "NPK" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "NPK" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"K-CAL.(mg.K2O/100g)"],sortRotation[ii,"K.offtake.kg.ha"],
       main="NPK", xlim=c(0,30), ylim=c(0,200),type="p",col="black")
  points(sortRotation[ij,"K-CAL.(mg.K2O/100g)"],sortRotation[ij,"K.offtake.kg.ha"],col="red")
  points(sortRotation[jj,"K-CAL.(mg.K2O/100g)"],sortRotation[jj,"K.offtake.kg.ha"],col="orange")
  legend("topleft",legend=c("Potato","Oats","Rye"),col=c("black","red","orange"), pch=1, bty="n")
  
  ii <- which(sortRotation[,"Trt"] == "Control" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "Control" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "Control" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"K-CAL.(mg.K2O/100g)"],sortRotation[ii,"K.offtake.kg.ha"],
       main="Control", xlim=c(0,30), ylim=c(0,200),type="p",col="black")
  points(sortRotation[ij,"K-CAL.(mg.K2O/100g)"],sortRotation[ij,"K.offtake.kg.ha"],col="red")
  points(sortRotation[jj,"K-CAL.(mg.K2O/100g)"],sortRotation[jj,"K.offtake.kg.ha"],col="orange")
  
  ii <- which(sortRotation[,"Trt"] == "FYM" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "FYM" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "FYM" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"K-CAL.(mg.K2O/100g)"],sortRotation[ii,"K.offtake.kg.ha"],
       main="FYM", xlim=c(0,30), ylim=c(0,200),type="p",col="black")
  points(sortRotation[ij,"K-CAL.(mg.K2O/100g)"],sortRotation[ij,"K.offtake.kg.ha"],col="red")
  points(sortRotation[jj,"K-CAL.(mg.K2O/100g)"],sortRotation[jj,"K.offtake.kg.ha"],col="orange")
  
  ii <- which(sortRotation[,"Trt"] == "NP" & sortRotation[,"Crop"] == "potato")
  ij <- which(sortRotation[,"Trt"] == "NP" & sortRotation[,"Crop"] == "rye")
  jj <- which(sortRotation[,"Trt"] == "NP" & sortRotation[,"Crop"] == "oats")
  plot(sortRotation[ii,"K-CAL.(mg.K2O/100g)"],sortRotation[ii,"K.offtake.kg.ha"],
       main="NP", xlim=c(0,30), ylim=c(0,200), type="p", col="black")
  points(sortRotation[ij,"K-CAL.(mg.K2O/100g)"],sortRotation[ij,"K.offtake.kg.ha"],col="red")
  points(sortRotation[jj,"K-CAL.(mg.K2O/100g)"],sortRotation[jj,"K.offtake.kg.ha"],col="orange")
  mtext("K-CAL, mg(K2O)/100g", side=1, outer=TRUE, line=0.5)
  mtext("K offtake, kg/ha", side=2, outer=TRUE, line=0.5)
}
Visualise_effect_Mg_on_cumulative_uptake <- function(UptP, UptK){
  par(mfrow=c(2,2),mar=c(4,4,1,1))
  plotCS(Upt = UptP,trt = "NPK", trtMg = "N4PKMg", title = "P")
  plotCS(Upt = UptK,trt = "NPK", trtMg = "N4PKMg", title = "K")
}

plot_cumsum_PK_per_crop <- function(){
  par(mfcol=c(4,2),mar=c(2,2,1,0),oma = c(2,2,0.5,0.5))
  cumsum_plot(Upt = out$UptP, which(out$UptP[,"Crop"] == "wheat"), parListNames, cols, ltys, lwds, plottitle = "Winter wheat, P uptake")
  cumsum_plot(Upt = out$UptP, which(out$UptP[,"Crop"] == "potato"), parListNames, cols, ltys, lwds, plottitle = "Potato, P uptake")  
  cumsum_plot(Upt = out$UptP, which(out$UptP[,"Crop"] == "beans"), parListNames, cols, ltys, lwds, plottitle = "Spring beans, P uptake")
  legend("topleft",legend=parListNames,col=cols,lwd=lwds, lty=ltys,bty="n")
  cumsum_plot(Upt = out$UptP, which(out$UptP[,"Crop"] == "silage maize"), parListNames, cols, ltys, lwds, plottitle = "Silage maize, P uptake")
  
  cumsum_plot(Upt = out$UptK, which(out$UptK[,"Crop"] == "wheat"), parListNames, cols, ltys, lwds, plottitle = "Winter wheat, K uptake")
  cumsum_plot(Upt = out$UptK, which(out$UptK[,"Crop"] == "potato"), parListNames, cols, ltys, lwds, plottitle = "Potato, K uptake")  
  cumsum_plot(Upt = out$UptK, which(out$UptK[,"Crop"] == "beans"), parListNames, cols, ltys, lwds, plottitle = "Spring beans, K uptake")
  cumsum_plot(Upt = out$UptK, which(out$UptK[,"Crop"] == "silage maize"), parListNames, cols, ltys, lwds, plottitle = "Silage maize, K uptake")
  mtext("Measured cumulative uptake, kg/ha", side = 1, outer = TRUE, line = 0.5, cex=1.25)
  mtext("Predicted cumulative uptake, kg/ha", side = 2, outer = TRUE, line = 0.5, cex=1.25)
}

##HELP FUNCTION FOR PLOTTING EXPERIMENTAL DATA PROCESSING###############################################




