source("00. General figures.r")

##HELP FUNCTION FOR PLOTTING EXPERIMENTAL DATA PROCESSING###############################################
plotCumOfftakes_no_FYM <- function(df){
  iNPK <- which(df[,"Trt"] == "NPK")  #green
  iNK <- which(df[,"Trt"] == "NK")   #red
  iNP <- which(df[,"Trt"] == "NP")   #blue
  iN <- which(df[,"Trt"] == "N")    #orange
  iPK <- which(df[,"Trt"] == "PK")    #cyan
  iC <- which(df[,"Trt"] == "Control") #red
  
  par(mfrow=c(2,3),mar=c(4,4,1,1))
  y_axis_figures <- c("N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha")
  for (y_axes in y_axis_figures){
    plot( df[iC,"Year"],cumsum(df[iC,y_axes]),col="black",
          xlab="Year", ylab=y_axes, type = "l" , ylim = c(0,max(cumsum(df[iNPK,y_axes]), na.rm= TRUE)))
    lines(df[iPK,"Year"],cumsum(df[iPK,y_axes]),col="cyan")
    lines(df[iN,"Year"],cumsum(df[iN,y_axes]),col="orange")
    lines(df[iNK,"Year"],cumsum(df[iNK,y_axes]),col="red")
    lines(df[iNP,"Year"],cumsum(df[iNP,y_axes]),col="blue")
    lines(df[iNPK,"Year"],cumsum(df[iNPK,y_axes]),col="green")
  }
  legend("topleft",legend=c("NPK", "NK", "NP", "N", "PK", "Control"), 
         col=c("green" ," red", "blue", "orange", "cyan" ,"black"),
         lty = 1,bty = "n")
  
  
  x_axis_figures <- c("N.rate.kg.ha","P.rate.kg.ha","K.rate.kg.ha")
  
  for (i in 1:length(y_axis_figures)){
    x_axes <- x_axis_figures[i]
    y_axes <- y_axis_figures[i]
    xylim = max(c(cumsum(df[iNPK,x_axes]),cumsum(df[iNPK,y_axes])), na.rm= TRUE)
    plot(  cumsum(df[iC,x_axes]),cumsum(df[iC,y_axes]),col="black",
           xlab=x_axes, ylab=y_axes, type = "l", 
           xlim = c(0,xylim), ylim = c(0,xylim))
    lines(cumsum(df[iPK,x_axes]),cumsum(df[iPK,y_axes]),col="cyan")
    lines(cumsum(df[iN,x_axes]),cumsum(df[iN,y_axes]),col="orange")
    lines(cumsum(df[iNK,x_axes]),cumsum(df[iNK,y_axes]),col="red")
    lines(cumsum(df[iNP,x_axes]),cumsum(df[iNP,y_axes]),col="blue")
    lines(cumsum(df[iNPK,x_axes]),cumsum(df[iNPK,y_axes]),col="green")
    lines(c(-10000,10000),c(-10000,10000),col="black",lty=3, lwd=3)
    
  }
  legend("topleft",legend=c("NPK", "NK", "NP", "N", "PK", "Control"), 
         col=c("green" ," red", "blue", "orange", "cyan" ,"black"),
         lty = 1,bty = "n")
}

plotCumOfftakes_FYM <- function(df){
  iNPK <- which(df[,"Trt"] == "FYM+NPK")  #green
  iNK <- which(df[,"Trt"] == "FYM+NK")   #red
  iNP <- which(df[,"Trt"] == "FYM+NP")   #blue
  iN <- which(df[,"Trt"] == "FYM+N")    #orange
  iPK <- which(df[,"Trt"] == "FYM+PK")    #cyan
  iC <- which(df[,"Trt"] == "Control") #red
  
  par(mfrow=c(2,3),mar=c(4,4,1,1))
  y_axis_figures <- c("N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha")
  for (y_axes in y_axis_figures){
    plot( df[iC,"Year"],cumsum(df[iC,y_axes]),col="black",
          xlab="Year", ylab=y_axes, type = "l" , ylim = c(0,max(cumsum(df[iNPK,y_axes]), na.rm= TRUE)))
    lines(df[iPK,"Year"],cumsum(df[iPK,y_axes]),col="cyan")
    lines(df[iN,"Year"],cumsum(df[iN,y_axes]),col="orange")
    lines(df[iNK,"Year"],cumsum(df[iNK,y_axes]),col="red")
    lines(df[iNP,"Year"],cumsum(df[iNP,y_axes]),col="blue")
    lines(df[iNPK,"Year"],cumsum(df[iNPK,y_axes]),col="green")
  }
  legend("topleft",legend=c("FYM+NPK", "FYM+NK", "FYM+NP", "FYM+N", "FYM+PK", "Control"), 
         col=c("green" ," red", "blue", "orange", "cyan" ,"black"),
         lty = 1,bty = "n")
  
  
  x_axis_figures <- c("N.rate.kg.ha","P.rate.kg.ha","K.rate.kg.ha")
  
  for (i in 1:length(y_axis_figures)){
    x_axes <- x_axis_figures[i]
    y_axes <- y_axis_figures[i]
    xylim = max(c(cumsum(df[iNPK,x_axes]),cumsum(df[iNPK,y_axes])), na.rm= TRUE)
    plot(  cumsum(df[iC,x_axes]),cumsum(df[iC,y_axes]),col="black",
           xlab=x_axes, ylab=y_axes, type = "l", 
           xlim = c(0,xylim), ylim = c(0,xylim))
    lines(cumsum(df[iPK,x_axes]),cumsum(df[iPK,y_axes]),col="cyan")
    lines(cumsum(df[iN,x_axes]),cumsum(df[iN,y_axes]),col="orange")
    lines(cumsum(df[iNK,x_axes]),cumsum(df[iNK,y_axes]),col="red")
    lines(cumsum(df[iNP,x_axes]),cumsum(df[iNP,y_axes]),col="blue")
    lines(cumsum(df[iNPK,x_axes]),cumsum(df[iNPK,y_axes]),col="green")
    lines(c(-10000,10000),c(-10000,10000),col="black",lty=3, lwd=3)
    
  }
  legend("topleft",legend=c("FYM+NPK", "FYM+NK", "FYM+NP", "FYM+N", "FYM+PK", "Control"), 
         col=c("green" ," red", "blue", "orange", "cyan" ,"black"),
         lty = 1,bty = "n")
}

#Compare treatments with and without Mg
plotCS <- function(Upt,trt,trtMg, title){
  CSUpt <- cumsum(Upt[,c(trt,trtMg)])
  colnames(CSUpt)<-c("x","y")
  plot(CSUpt[,"x"],CSUpt[,"y"],
       xlab = paste0("Cum. uptake ",trt," kg/ha"),
       ylab = paste0("Cum. uptake ",trtMg, " kg/ha"), 
       main = title)
  fitCS = lm(y ~ 1 + x, CSUpt)
  abline(a=0, b=1, col="red")
  abline(a=fitCS$coefficients[1], b=fitCS$coefficients[2])
  text(0.5*max(CSUpt[,"x"]),0.05*max(CSUpt[,"y"]),
       paste0("R2 = ",round(summary(fitCS)$r.squared,2),
              " NPKMg= ",round(fitCS$coefficients[1],3),
              " + ", round(fitCS$coefficients[2],3), "NPK"))
}

Visualise_effect_Mg_on_cumulative_uptake <- function(UptP, UptK){
  par(mfrow=c(2,2),mar=c(4,4,1,1))
  plotCS(Upt = UptP,trt = "NPK", trtMg = "NPKMg", title = "P")
  plotCS(Upt = UptP,trt = "FYMNPK", trtMg = "FYMNPKMg", title = "P")
  plotCS(Upt = UptK,trt = "NPK", trtMg = "NPKMg", title = "K")
  plotCS(Upt = UptK,trt = "FYMNPK", trtMg = "FYMNPKMg", title = "K")
}

plotCropOfftakes <- function(df, crop = "potato"){
  iNPK <- which(df[,"Trt"] == "FYM+NPK" & df[,"Crop"] == crop)  #red
  iNP <- which(df[,"Trt"] == "FYM+NP" & df[,"Crop"] == crop)  #red
  iNK <- which(df[,"Trt"] == "FYM+NK" & df[,"Crop"] == crop)  #red
  iPK <- which(df[,"Trt"] == "FYM+PK" & df[,"Crop"] == crop)  #red
  
  par(mfrow=c(2,3),mar=c(4,4,1,1))
  y_axis_figures <- c("N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha")
  for (y_axes in y_axis_figures){
    maxY <- max(df[iNPK,y_axes],na.rm = TRUE)
    
    plot( df[iNPK,"Year"],df[iNPK,y_axes],col="red",lty=1,
          xlab="Year", ylab=y_axes, type = "l", ylim = c(0,maxY))
    lines(df[iNP,"Year"],df[iNP,y_axes],col="green", lty=2)
    lines(df[iNK,"Year"],df[iNK,y_axes],col="blue", lty=3)
    lines(df[iPK,"Year"],df[iPK,y_axes],col="black", lty=4)
  }
  legend("bottomleft",legend=c("FYM+NPK", "FYM+NP", "FYM+NK", "FYM+PK"), 
         col=c("red", "green", "blue", "black"),
         lty =c(1,2,3,4), bty = "n")
  
  iNPK <- which(df[,"Trt"] == "NPK" & df[,"Crop"] == crop)  #red
  iNP <- which(df[,"Trt"] == "NP" & df[,"Crop"] == crop)  #red
  iNK <- which(df[,"Trt"] == "NK" & df[,"Crop"] == crop)  #red
  iPK <- which(df[,"Trt"] == "PK" & df[,"Crop"] == crop)  #red
  
  y_axis_figures <- c("N.offtake.kg.ha","P.offtake.kg.ha","K.offtake.kg.ha")
  for (y_axes in y_axis_figures){
    maxY <- max(df[iNPK,y_axes],na.rm = TRUE)
    
    plot( df[iNPK,"Year"],df[iNPK,y_axes],col="red",lty=1,
          xlab="Year", ylab=y_axes, type = "l", ylim = c(0,maxY))
    lines(df[iNP,"Year"],df[iNP,y_axes],col="green", lty=2)
    lines(df[iNK,"Year"],df[iNK,y_axes],col="blue", lty=3)
    lines(df[iPK,"Year"],df[iPK,y_axes],col="black", lty=4)
  }
  legend("bottomleft",legend=c("NPK", "NP", "NK", "PK"), 
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
  
  for(cr in c("potato","rye", "oats")){
    ii <- which(df[,"Crop"] == cr & df[,"Trt"] == "NPK")
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

plotCumApplication_vs_CumLMpredOfftake <- function(sortRotationPred){
  par(mfcol=c(2,2),mar=c(4,4,1,1))
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,max(sortRotationPred[,"cum.N.rate.kg.ha"])), 
       ylim=c(0,max(sortRotationPred[,"cum.LMpred.N.offtake.kg.ha"])),
       xlab="Cumulative N application, kg / ha",
       ylab="Cumulative pred. N uptake, kg / ha")
  seltreat = c("FYM+NPK", "FYM+NP","FYM+NK","FYM+PK", "NPK", "NP", "NK", "PK")
  cols=c(rainbow(4),rainbow(4))
  ltys=c(1,1,1,1, 3,3,3,3)
  nr = 0
  for(treat in seltreat){
    nr=nr+1
    ii<-which(sortRotation[,"Trt"] == treat)
    lines(sortRotationPred[ii,"cum.N.rate.kg.ha"],sortRotationPred[ii,"cum.LMpred.N.offtake.kg.ha"], 
          col = cols[nr], lty = ltys[nr])
  }
  
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,max(sortRotationPred[,"cum.P.rate.kg.ha"])), 
       ylim=c(0,max(sortRotationPred[,"cum.LMpred.P.offtake.kg.ha"])),
       xlab="Cumulative P application, kg / ha",
       ylab="Cumulative pred. P uptake, kg / ha")
  nr=0
  for(treat in seltreat){
    nr=nr+1
    ii<-which(sortRotation[,"Trt"] == treat)
    lines(sortRotationPred[ii,"cum.P.rate.kg.ha"],sortRotationPred[ii,"cum.LMpred.P.offtake.kg.ha"], 
          col = cols[nr], lty = ltys[nr])
  }
  
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,max(sortRotationPred[,"cum.K.rate.kg.ha"])), 
       ylim=c(0,max(sortRotationPred[,"cum.LMpred.K.offtake.kg.ha"])),
       xlab="Cumulative K application, kg / ha",
       ylab="Cumulative pred. K uptake, kg / ha")
  nr=0
  for(treat in seltreat){
    nr=nr+1
    ii<-which(sortRotation[,"Trt"] == treat)
    lines(sortRotationPred[ii,"cum.K.rate.kg.ha"],sortRotationPred[ii,"cum.LMpred.K.offtake.kg.ha"], 
          col=cols[nr], lty =ltys[nr])
  }
  plot(c(-1000,-1000),c(-1000,-1000),
       xlim=c(0,max(sortRotationPred[,"cum.K.rate.kg.ha"])), 
       ylim=c(0,max(sortRotationPred[,"cum.LMpred.K.offtake.kg.ha"])),
       xlab=" ",
       ylab=" ")
  nr=0
  legend("topleft",legend=seltreat, col=cols, lty=ltys, bty="n")
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
##HELP FUNCTION FOR PLOTTING EXPERIMENTAL DATA PROCESSING###############################################

###Hanninghof model results############################################################################################################
cumsum_plot <- function(Upt, index, parListNames, cols, ltys, lwds, plottitle){
  Upt <- Upt[index,]
  
  if( length( strfind(names(Upt), "pred_K_UP") ) >= 1  ){
    NameCol = "pred_K_UP_"
  }else{
    NameCol = "pred_P_UP_"
  }
  ymin = 0; ymax = 0; 
  for(ln in parListNames){
    index <- !is.na(Upt[,ln])
    ymax = max( c(ymax, cumsum(Upt[index,ln]),  cumsum(Upt[index, paste0( NameCol, ln)])), na.rm = TRUE)
  }
  plot(-100, -100, xlim=c(ymin,ymax),  ylim=c(ymin,ymax), 
       ylab = "Predicted uptake, kg /ha", xlab="Measured uptake, kg /ha", main=plottitle)
  
  nr = 0
  for(ln in parListNames){
    index <- !is.na(Upt[,ln])
    nr = nr + 1
    lines(cumsum(Upt[index,ln]), cumsum(Upt[index,paste0(NameCol,ln)]), col = cols[nr], lty=ltys[nr], lwd=lwds[nr])
  }
  abline(0,1,col="black", lty=1, lwd=2)
}

plot_state_results <- function(){  
  par(mfcol=c(3,2),mar=c(2,4,1,1))
  
  statenames = c("P_labile", "P_stable", "P_labile_FYM", "K_labile", "K_stable", "K_labile_FYM")
  xlim=c(TESTYEARS[1],TESTYEARS[length(TESTYEARS)])
  for(sn in statenames){
    
    ymin = 1E9; ymax = 0; 
    for(i in 1:length(listStates)){
      ymin = min(ymin, listStates[[i]][,sn], na.rm = TRUE)
      ymax = max(ymax, listStates[[i]][,sn], na.rm = TRUE)
    }
    plot(-100, -100, ylab = paste0(sn, ", kg/ha"), xlab= "Year", xlim = xlim, ylim=c(ymin,ymax))
    for(i in 1:length(listStates)){
      lines(listStates[[i]][,"time"], listStates[[i]][,sn], col = cols[i], lty=ltys[i], lwd=lwds[i])  
    }
  }
  legend("topleft",legend = parListNames, bty="n", col=cols, lwd=lwds)
  
}
plot_meas_pred_uptake_time <- function(){
  par(mfrow=c(2,1),mar=c(2,4,1,1))
  
  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, UptK[,ln],  out$UptK[,paste0("pred_K_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, UptK[,ln],  out$UptK[,paste0("pred_K_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "K uptake, kg/ha", xlab= "Year", xlim=c(min(TESTYEARS),max(TESTYEARS)), ylim=c(ymin,ymax))
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    lines(out$UptK[,"Year"], out$UptK[,ln], col = cols[nr], lty=2, lwd=1)  
    lines(out$UptK[,"Year"], out$UptK[,paste0("pred_K_UP_", ln)], col = cols[nr], lty=ltys[nr], lwd=lwds[nr])  
  }
  
  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, out$UptP[,ln],  out$UptP[,paste0("pred_P_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, out$UptP[,ln],  out$UptP[,paste0("pred_P_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "P uptake, kg/ha", xlab= "Year", xlim=c(min(TESTYEARS),max(TESTYEARS)), ylim=c(ymin,ymax))
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    lines(out$UptP[,"Year"], out$UptP[,ln], col = cols[nr], lty=2, lwd=1)  
    lines(out$UptP[,"Year"], out$UptP[,paste0("pred_P_UP_", ln)], col = cols[nr], lty=ltys[nr], lwd=lwds[nr])  
  }
}
plot_meas_vs_pred_PKuptake <- function(ShowLegend = TRUE){
  par(mfrow=c(2,2),mar=c(2,2,1,0),oma=c(2,2,0.5,0.5))
  
  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, out$UptP[,ln],  out$UptP[,paste0("pred_P_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, out$UptP[,ln],  out$UptP[,paste0("pred_P_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "Predicted P uptake, kg/ha", xlab= "Measured P uptake, kg/ha", xlim = c(ymin,ymax), ylim=c(ymin,ymax))
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    points(out$UptP[,ln], out$UptP[,paste0("pred_P_UP_", ln)], col = cols[nr], pch=pchs[nr])  
  }
  abline(0,1,col="black",lwd=3,lty=1)
  text(x=12.5, y=ymin, paste0("RMSE = ", round(out$RMSE_P,2), " kg P/ha")  )  
  mtext('c', side = 3, line = -2, adj = 0.99, cex = 1, col= "black")
  
  if(ShowLegend){
    legend("topleft",legend = parListNames, col=cols, bty="n", pch = pchs)
  }
  
  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, out$UptK[,ln],  out$UptK[,paste0("pred_K_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, out$UptK[,ln],  out$UptK[,paste0("pred_K_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "Predicted K uptake, kg/ha", xlab= "Measured K uptake, kg/ha", xlim = c(ymin,ymax), ylim=c(ymin,ymax))
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    points(out$UptK[,ln], out$UptK[,paste0("pred_K_UP_", ln)], col = cols[nr], pch=pchs[nr])  
  }
  abline(0,1,col="black",lwd=3,lty=1)
  text(x=90, y=ymin, paste0("RMSE = ", round(out$RMSE_K,2), " kg K/ha")  )  
  mtext('d', side = 3, line = -2, adj = 0.99, cex = 1, col = "black")
  
  mtext("Measured uptake, kg/ha", side = 1, outer= TRUE, line=-19, cex=1.25)
  mtext("Predicted uptake, kg/ha", side = 2, outer= TRUE, line=0.5, adj=0.9, cex=1.25)
}
plot_cumsum_PK <- function(ShowLegend = TRUE){
  par(mfrow=c(2,2),mar=c(2,2,1,0),oma = c(2,2,0.5,0.5))
  cumsum_plot(Upt = out$UptP, 1:nrow(out$UptP), parListNames, cols, ltys, lwds, plottitle = "Rotation, P uptake")  
  if(ShowLegend){
    legend("topleft",legend=parListNames,col=cols,lwd=lwds, lty=ltys,bty="n", cex=0.8)
  }
  
  cumsum_plot(Upt = out$UptK, 1:nrow(out$UptK), parListNames, cols, ltys, lwds, plottitle = "Rotation, K uptake")  
  mtext("Measured cum. uptake, kg/ha", side = 1, outer = TRUE, line = -19, cex=1.25)
  mtext("Predicted cum. uptake, kg/ha", side = 2, outer = TRUE, line = 0.5, adj=0.9, cex=1.25)
}

plot_cumsum_PK_per_crop <- function(){
  par(mfcol=c(3,2),mar=c(2,2,1,0),oma = c(2,2,0.5,0.5))
  cumsum_plot(Upt = out$UptP, which(out$UptP[,"Crop"] == "potato"), parListNames, cols, ltys, lwds, plottitle = "Potato, P uptake")  
  cumsum_plot(Upt = out$UptP, which(out$UptP[,"Crop"] == "rye"), parListNames, cols, ltys, lwds, plottitle = "Rye, P uptake")
  cumsum_plot(Upt = out$UptP, which(out$UptP[,"Crop"] == "oats"), parListNames, cols, ltys, lwds, plottitle = "Oats, P uptake")
  legend("topleft",legend=parListNames,col=cols,lwd=lwds, lty=ltys,bty="n", cex=0.8)
  
  cumsum_plot(Upt = out$UptK, which(out$UptK[,"Crop"] == "potato"), parListNames, cols, ltys, lwds, plottitle = "Potato, K uptake")  
  cumsum_plot(Upt = out$UptK, which(out$UptK[,"Crop"] == "rye"), parListNames, cols, ltys, lwds, plottitle = "Rye, K uptake")
  cumsum_plot(Upt = out$UptK, which(out$UptK[,"Crop"] == "oats"), parListNames, cols, ltys, lwds, plottitle = "Oats, K uptake")
  mtext("Measured cumulative uptake, kg/ha", side = 1, outer = TRUE, line = 0.5, cex=1.25)
  mtext("Predicted cumulative uptake, kg/ha", side = 2, outer = TRUE, line = 0.5, cex=1.25)
}
###Hanninghof model results############################################################################################################