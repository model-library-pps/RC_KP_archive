

cumsum_plot <- function(Upt, index, parListNames, cols, ltys, lwds, plottitle){
  Upt <- Upt[index,]
  
  if( length( strfind(names(Upt), "pred_K_UP") ) >= 1 ){
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
       ylab = "Predicted uptake, kg /ha", xlab="Measured uptake, kg /ha")
  
  nr = 0
  for(ln in parListNames){
    index <- !is.na(Upt[,ln])
    nr = nr + 1
    lines(cumsum(Upt[index,ln]), cumsum(Upt[index,paste0(NameCol,ln)]), col = cols[nr], lty=ltys[nr], lwd=lwds[nr])
  }
  abline(0,1,col="black", lty=1, lwd=2)
  text(x=ymax, y=ymin, adj=1, labels = plottitle)
}

plot_state_results <- function(){  
  par(mfcol=c(3,2),mar=c(2,4,1,1))
  
  statenames = c("K_labile", "K_stable", "K_labile_FYM", "P_labile", "P_stable", "P_labile_FYM")
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
  legend("topleft",legend = parListNames, bty="n", col=cols, lwd=lwds, lty=ltys)
  
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

plot_meas_vs_pred_NPKuptake <- function(ShowLegend = TRUE){
  par(mfrow=c(2,3),mar=c(2,2,0.5,0),oma=c(2,3,0.5,0.5))

  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, out$UptN[,ln],  out$UptN[,paste0("pred_N_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, out$UptN[,ln],  out$UptN[,paste0("pred_N_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "Predicted N uptake, kg/ha", 
       xlab= "Measured N uptake, kg/ha", xlim = c(ymin,ymax), ylim=c(ymin,ymax))
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    points(out$UptN[,ln], out$UptN[,paste0("pred_N_UP_", ln)], col = cols[nr], pch=pchs[nr])  
  }
  abline(0,1,col="black",lwd=3,lty=1)
  mtext('N', side = 3, line = -2, cex = 1, col= "black")
  text(x=ymax, y=ymin, adj=1, paste0("RMSE = ", round(out$RMSE_N,2), " kg N/ha")  )  
  
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
  mtext('P', side = 3, line = -2, cex = 1, col= "black")
  
  text(x=ymax, y=ymin, adj=1, paste0("RMSE = ", round(out$RMSE_P,2), " kg P/ha")  )
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
  mtext('K', side = 3, line = -2, cex = 1, col = "black")
  
  text(x=ymax, y=ymin, adj=1, paste0("RMSE = ", round(out$RMSE_K,2), " kg K/ha")  )  
  mtext(expression("Measured uptake, kg ha"^"-1") , side = 1, outer= TRUE, line=-24, cex=1.25)
  mtext(expression("Predicted uptake, kg ha"^"-1"), side = 2, outer= TRUE, line=0.5, adj=0.9, cex=1.25)
}


plot_meas_vs_pred_PKuptake <- function(ShowLegend = TRUE){
  par(mfrow=c(2,2),mar=c(2,2,0.5,0),oma=c(2,3,0.5,0.5))
  
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
  mtext('e', side = 3, line = -2, adj = 0.99, cex = 1, col= "black")
  
  text(x=12.5, y=ymin, paste0("RMSE = ", round(out$RMSE_P,2), " kg P/ha")  ) 
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
  mtext('f', side = 3, line = -2, adj = 0.99, cex = 1, col = "black")
  
  text(x=90, y=ymin, paste0("RMSE = ", round(out$RMSE_K,2), " kg K/ha")  )  
  mtext(expression("Measured uptake, kg ha"^"-1"), side = 1, outer= TRUE, line=-19, cex=1.25)
  mtext(expression("Predicted uptake, kg ha"^"-1"), side = 2, outer= TRUE, line=0.5, adj=0.9, cex=1.25)
}

plot_firstyear_meas_vs_pred_PKuptake <- function(){
  par(mfrow=c(2,2),mar=c(2,2,1,0),oma=c(2,2,0.5,0.5))
  
  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, out$UptN[1,ln],  out$UptN[1,paste0("pred_N_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, out$UptN[1,ln],  out$UptN[1,paste0("pred_N_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "Predicted N uptake, kg/ha", xlab= "Measured N uptake, kg/ha", 
       xlim = c(ymin,ymax), ylim=c(ymin,ymax), main="Year one N uptake")
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    points(out$UptN[1,ln], out$UptN[1,paste0("pred_N_UP_", ln)], col = cols[nr], pch=pchs[nr])  
  }
  abline(0,1,col="black",lwd=3,lty=3)
  legend("topleft",legend = parListNames, col=cols, bty="n", pch = pchs)
  
  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, out$UptP[1,ln],  out$UptP[1,paste0("pred_P_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, out$UptP[1,ln],  out$UptP[1,paste0("pred_P_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "Predicted P uptake, kg/ha", xlab= "Measured P uptake, kg/ha", 
       xlim = c(ymin,ymax), ylim=c(ymin,ymax), main="Year one P uptake")
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    points(out$UptP[1,ln], out$UptP[1,paste0("pred_P_UP_", ln)], col = cols[nr], pch=pchs[nr])  
  }
  abline(0,1,col="black",lwd=3,lty=3)
  
  ymin = 1E9; ymax = 0; 
  for(ln in parListNames){
    ymin = min( c(ymin, out$UptK[1,ln],  out$UptK[1,paste0("pred_K_UP_", ln)]), na.rm = TRUE)
    ymax = max( c(ymax, out$UptK[1,ln],  out$UptK[1,paste0("pred_K_UP_", ln)]), na.rm = TRUE)
  }
  plot(-100, -100, ylab = "Predicted K uptake, kg/ha", xlab= "Measured K uptake, kg/ha", 
       xlim = c(ymin,ymax), ylim=c(ymin,ymax), main="Year one K uptake")
  nr = 0
  for(ln in parListNames){
    nr=nr+1
    points(out$UptK[1,ln], out$UptK[1,paste0("pred_K_UP_", ln)], col = cols[nr], pch=pchs[nr])  
  }
  abline(0,1,col="black",lwd=3,lty=3)
  mtext(expression("Measured uptake, kg ha"^"-1"), side = 1, outer= TRUE, line=1, cex=1.25)
  mtext(expression("Predicted uptake, kg ha"^"-1"), side = 2, outer= TRUE, line=1, cex=1.25)
}

plot_cumsum_PK <- function(ShowLegend = TRUE){
  par(mfrow=c(2,2),mar=c(2,2,0.5,0),oma = c(2,3,0.5,0.5))
  cumsum_plot(Upt = out$UptP, 1:nrow(out$UptP), parListNames, cols, ltys, lwds, plottitle = "a, P")  
  if(ShowLegend){
    legend("topleft",legend=parListNames,col=cols,lwd=lwds, lty=ltys, bty="n", cex=0.8, seg.len = 3)
  }
  
  cumsum_plot(Upt = out$UptK, 1:nrow(out$UptK), parListNames, cols, ltys, lwds, plottitle = "b, K")  
  mtext(expression("Meas. cumulative uptake, kg ha"^"-1"), side = 1, outer = TRUE, line = -19, cex=1.25)
  mtext(expression("Pred. cumulative uptake, kg ha"^"-1"), side = 2, outer = TRUE, line = 0.5, adj=1, cex=1.25)
}


#Compare treatments with and without Mg
plotCS <- function(Upt,trt,trtMg, title = NULL){
  #remove NA's
  ii <- which(!is.na(Upt[,trt]) & !is.na(Upt[,trtMg]))
  CSUpt <- cumsum(Upt[ii,c(trt,trtMg)])
  colnames(CSUpt)<-c("x","y")
  fitCS = lm(y ~ 1 + x, CSUpt)
  
  if(!is.null(title)){
    plot(CSUpt[,"x"],CSUpt[,"y"],
         xlab = paste0("Cum. uptake ",trt," kg/ha"),
         ylab = paste0("Cum. uptake ",trtMg, " kg/ha"), 
         col="black",pch=1,
         main = title)
    ypostxt <- min(CSUpt[,"y"]) + 0.95*(max(CSUpt[,"y"])-min(CSUpt[,"y"]))
    lines(x=CSUpt[c(1, nrow(CSUpt)),"x"], 
          y=fitCS$coefficients[1] + fitCS$coefficients[2] *CSUpt[c(1, nrow(CSUpt)),"x"],
          col="black")
  }else{
    points(CSUpt[,"x"],CSUpt[,"y"],col="red")
    ypostxt <- min(CSUpt[,"y"]) + 0.05*(max(CSUpt[,"y"])-min(CSUpt[,"y"]))
    lines(x=CSUpt[c(1, nrow(CSUpt)),"x"], 
          y=fitCS$coefficients[1] + fitCS$coefficients[2] *CSUpt[c(1, nrow(CSUpt)),"x"],
          col="red")
  }
  text(x=0.7*max(CSUpt[,"x"]),y=ypostxt,
       paste0("R2 = ",round(summary(fitCS)$r.squared,2),
              "; y= ",round(fitCS$coefficients[1],1),
              " + ", round(fitCS$coefficients[2],3), "x"),cex=0.7)
}

plot_ratio <- function(ratio="NK"){
  df<-data.frame(Crop=out$UptN[,"Crop"],
                 mNK_Control=out$UptN[,"Control"]/out$UptK[,"Control"])
  for (trt in parListNames){
    df[,paste0("mNK_",trt)] <- out$UptN[,trt]/out$UptK[,trt]
    df[,paste0("mKN_",trt)] <- out$UptK[,trt]/out$UptN[,trt]
    df[,paste0("mNP_",trt)] <- out$UptN[,trt]/out$UptP[,trt]
    df[,paste0("mPN_",trt)] <- out$UptP[,trt]/out$UptN[,trt]
    df[,paste0("mPK_",trt)] <- out$UptP[,trt]/out$UptK[,trt]
    df[,paste0("mKP_",trt)] <- out$UptK[,trt]/out$UptP[,trt]
    df[,paste0("pNK_",trt)] <- out$UptN[,paste0("pred_N_UP_",trt)]/out$UptK[,paste0("pred_K_UP_",trt)]
    df[,paste0("pKN_",trt)] <- out$UptK[,paste0("pred_K_UP_",trt)]/out$UptN[,paste0("pred_N_UP_",trt)]
    df[,paste0("pNP_",trt)] <- out$UptN[,paste0("pred_N_UP_",trt)]/out$UptP[,paste0("pred_P_UP_",trt)]
    df[,paste0("pPN_",trt)] <- out$UptP[,paste0("pred_P_UP_",trt)]/out$UptN[,paste0("pred_N_UP_",trt)]
    df[,paste0("pPK_",trt)] <- out$UptP[,paste0("pred_P_UP_",trt)]/out$UptK[,paste0("pred_K_UP_",trt)]
    df[,paste0("pKP_",trt)] <- out$UptK[,paste0("pred_K_UP_",trt)]/out$UptP[,paste0("pred_P_UP_",trt)]
  }
  
  par(mfcol=c(2,3), mar=c(2,2,1,0), oma=c(2,2,0.5,0.5))
  for(crop in unique(df[,"Crop"])){
    ii<-which(df[,"Crop"] == crop)
    mv=0
    for (i in 1:length(parListNames)){
      trt=parListNames[i]
      mv=max(c(mv,df[ii,paste0("m",ratio,"_",trt)], df[ii,paste0("p",ratio,"_",trt)]), na.rm=TRUE)
    }
    plot(-5,-5,xlim=c(0,mv),ylim=c(0,mv),xlab="Measured",ylab="Predicted", main=crop, col="red")
    for (i in 1:length(parListNames)){
      trt=parListNames[i]
      points(df[ii,paste0("m",ratio,"_",trt)],df[ii,paste0("p",ratio,"_",trt)],col=cols[i],pch=pchs[i])
    }
    abline(a=0,b=1,"black")
  }
  legend("topleft",legend=parListNames,col=cols,pch=pchs, bty="n")
  mtext(paste0("Measured ",ratio, " ratio"), side = 1, outer= TRUE, line=1, cex=1.25)
  mtext(paste0("Predicted ",ratio, " ratio"), side = 2, outer= TRUE, line=1, cex=1.25)
}

plot_meas_vs_pred_Nuptake <- function(UptN, col_meas_N_UP, col_pred_N_UP){
  cols=rainbow(length(unique(UptN[,"Crop"])))
  
  par(mfrow=c(2,1),mar=c(4,4,1,1))
  plot(-5, -5, xlab="Measured uptake, kg N/ha", ylab = "Predicted uptake, kg N/ha", 
       main= col_meas_N_UP, col="red", 
       xlim=c(0, max(UptN[,col_meas_N_UP], na.rm=TRUE)),
       ylim=c(0, max(UptN[,col_meas_N_UP], na.rm=TRUE)))
  nr <- 0
  for(uCrop in unique(UptN[,"Crop"])){
    nr <- nr + 1
    ii<-which(UptN[,"Crop"] == uCrop)
    points(UptN[ii,col_meas_N_UP],UptN[ii,col_pred_N_UP], col=cols[nr], pch=16)
  }
  abline(a=0, b=1, col="black", lwd=2)
  legend("topleft", legend=unique(UptN[,"Crop"]), col = cols, pch=16)
  
  plot(-5, -5, xlab="Year", ylab = "Uptake, kg N/ha", 
       xlim=c(min(UptN[,"Year"], na.rm=TRUE), max(UptN[,"Year"], na.rm=TRUE)),
       ylim=c(0, max(UptN[,col_meas_N_UP], na.rm=TRUE)))
  nr <- 0
  for(uCrop in unique(UptN[,"Crop"])){
    nr <- nr + 1
    ii<-which(UptN[,"Crop"] == uCrop)
    points(UptN[ii,"Year"],UptN[ii,col_pred_N_UP], col=cols[nr],pch = 1)
    points(UptN[ii,"Year"],UptN[ii,col_meas_N_UP], col=cols[nr],pch = 16)
  }
  legend("topright", legend=c("Predicted", "Measured"), col = "black", pch = c(1,16))
}

