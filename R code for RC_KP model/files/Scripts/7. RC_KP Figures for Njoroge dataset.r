#RCKP model: plotting functions

#PLOTTING FUNCTIONS==================================
plot_results_trials_SN_uptake_measurements <- function(UptP, UptK){
  iPRow2016 <- which(UptP[,"SEASON"] == "LR2016")
  iPRow2018 <- which(UptP[,"SEASON"] == "LR2018")
  iKRow2016 <- which(UptK[,"SEASON"] == "LR2016")
  iKRow2018 <- which(UptK[,"SEASON"] == "LR2018")
  
  Treatments = c("Control",    "PK",     "NK",  "NP",      "NPK")
  cols       = c(    "red","orange","magenta","blue","darkgreen")
  pchs       = c(        0,       1,       2,      3,          4)
  par(mfrow=c(3,2),mar=c(2,2,1,0.5), oma=c(2, 2, 1, 1))
  
  ymaxP=30
  ymaxK=150
  
  i <- which(Treatments == "NK")
  plot(  UptP[iPRow2016,"NK_NONE"],  UptP[iPRow2016,"pred_P_UP_NK"],type="p",col=cols[i],pch=16,
         xlab="Measured P uptake, kg/ha",ylab="Predicted uptake, kg/ha",xlim=c(0,ymaxP),ylim=c(0,ymaxP), main="P")
  abline(0,1,col="black")
  mtext('a', side = 3, line = -2, adj = 0.95, cex = 1,
        col = "black")
  err=UptP[iPRow2016,"NK_NONE"] - UptP[iPRow2016,"pred_P_UP_NK"]
  text(x=0.5*ymaxP,y=1,paste0("RMSE:", round(sqrt(mean(err^2,na.rm= TRUE)),1))) 
  
  i <- which(Treatments == "NP")
  plot(  UptK[iKRow2016,"NP_NONE"],  UptK[iKRow2016,"pred_K_UP_NP"],type="p",col=cols[i],pch=16,
         xlab="Measured K uptake, kg/ha",ylab="Predicted K uptake, kg/ha",xlim=c(0,ymaxK),ylim=c(0,ymaxK), main="K")
  lines(c(0,150),c(0,150),col="black")
  mtext('b', side = 3, line = -2, adj = 0.95, cex = 1,
        col = "black")  
  err=UptK[iKRow2016,"NP_NONE"] - UptK[iKRow2016,"pred_K_UP_NP"]
  text(x=0.5*ymaxK,y=1,paste0("RMSE:", round(sqrt(mean(err^2,na.rm= TRUE)),1))) 
  
    legend("topleft",legend=Treatments,col=cols,pch=16,bty="n",cex = 0.8)
  j <- which(Treatments != "Control")
  legend("bottomright",legend=Treatments[j],col="black",pch=pchs[j],bty="n",cex = 0.8)
  
  #Compare modelled P uptakes with measured uptakes for LR2016
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured P uptake, kg/ha",ylab="Predicted uptake, kg/ha",xlim=c(0,ymaxP),ylim=c(0,ymaxP), main="LR2016, P")
  abline(0,1,col="black")
  
  err = NULL
  for(i in 1:length(Treatments)){
    if(Treatments[i] != "NK"){
      x = UptP[iPRow2016,paste0(Treatments[i],"_NONE")] 
      y = UptP[iPRow2016,paste0("pred_P_UP_",Treatments[i])]
      points(x, y, col=cols[i],pch = 16)
      err=  c(err, x-y)
    }  
  }
  text(x=0.5*ymaxP,y=1,paste0("RMSE:", round(sqrt(mean(err^2,na.rm= TRUE)),1))) 
  mtext('c', side = 3, line = -2, adj = 0.95, cex = 1,
        col = "black")
  
  #Compare modelled K uptakes with measured uptakes for LR2016
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured K uptake, kg/ha",ylab="Predicted uptake, kg/ha",xlim=c(0,ymaxK),ylim=c(0,ymaxK), main="LR2016, P")
  abline(0,1,col="black")
  
  err = NULL
  for(i in 1:length(Treatments)){
    if(Treatments[i] != "NP"){
      x = UptK[iPRow2016,paste0(Treatments[i],"_NONE")]
      y = UptK[iPRow2016,paste0("pred_K_UP_",Treatments[i])]
      points(x, y, col=cols[i],pch=16)
      err=c(err, x-y)
    }
  }  
  text(x=0.5*ymaxK,y=1,paste0("RMSE:", round(sqrt(mean(err^2,na.rm= TRUE)),1))) 
  mtext('d', side = 3, line = -2, adj = 0.95, cex = 1,
        col = "black")
  
  
  #Compare modelled P uptakes with measured uptakes for LR2018
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured P uptake, kg/ha",ylab="Predicted uptake, kg/ha",xlim=c(0,ymaxP),ylim=c(0,ymaxP), main="LR2018, P")
  abline(0,1,col="black")
  
  err = NULL
  for(i in 1:length(Treatments)){
    for(j in 1:length(Treatments)){
      if(Treatments[j] != "Control"){
        x = UptP[iPRow2018,paste0(Treatments[i],"_",Treatments[j])]
        y = UptP[iPRow2018,paste0("pred_P_UP_",Treatments[i],"_",Treatments[j])]
        points(x, y, col=cols[i],pch=pchs[j])
        err=c(err, x-y)
      }
    }
  }  
  text(x=0.5*ymaxP,y=1,paste0("RMSE:", round(sqrt(mean(err^2,na.rm= TRUE)),1))) 
  mtext('e', side = 3, line = -2, adj = 0.95, cex = 1,
        col = "black")
  
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured K uptake, kg/ha",ylab="Predicted uptake, kg/ha",xlim=c(0,ymaxK),ylim=c(0,ymaxK), main="LR2018, K")
  abline(0,1,col="black")
    
  err = NULL
  for(i in 1:length(Treatments)){
    for(j in 1:length(Treatments)){
      if(Treatments[j] != "Control"){
        x = UptK[iPRow2018,paste0(Treatments[i],"_",Treatments[j])] 
        y = UptK[iPRow2018,paste0("pred_K_UP_",Treatments[i],"_",Treatments[j])]
        points(x, y, col=cols[i],pch=pchs[j])
        err=c(err, x-y)
      }
    }  
  }
  text(x=0.5*ymaxK,y=1,paste0("RMSE:", round(sqrt(mean(err^2,na.rm= TRUE)),1))) 
  mtext('f', side = 3, line = -2, adj = 0.95, cex = 1,
        col = "black")

  mtext("Measured uptake, kg/ha" , side = 1, outer= TRUE, line = 0.5, cex = 1.25, col = "black")
  mtext("Predicted uptake, kg/ha" , side = 2, outer= TRUE,  line = 0.5, cex = 1.25, col = "black")

}

plot_results_trials_meas_vs_pred <- function(UptP, UptK){
  iPRow2016 <- which(UptP[,"SEASON"] == "LR2016")
  iPRow2018 <- which(UptP[,"SEASON"] == "LR2018")
  iKRow2016 <- which(UptK[,"SEASON"] == "LR2016")
  iKRow2018 <- which(UptK[,"SEASON"] == "LR2018")
  
  Treatments = c("Control",    "PK",     "NK",  "NP",      "NPK")
  cols       = c(    "red","orange","magenta","blue","darkgreen")
  pchs       = c(        0,       1,       2,      3,          4)
  par(mfrow=c(2,2),mar=c(2,2,1,0), oma=c(2, 2, 0.5,0.5))

  ymaxP=30
  ymaxK=150
  
  #Combined 2016 + 2018 excluding NP 2016 for K uptake and NK 2016 for P uptake
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured P uptake, kg/ha",ylab="Predicted uptake, kg/ha",xlim=c(0,ymaxP),ylim=c(0,ymaxP))
  abline(0,1,col="black")
  
  err = NULL
  for(i in 1:length(Treatments)){
    if(Treatments[i] != "NK"){
      x = UptP[iPRow2016,paste0(Treatments[i],"_NONE")] 
      y = UptP[iPRow2016,paste0("pred_P_UP_",Treatments[i])]
      points(x, y, col=cols[i],pch = 16)
      err=  c(err, x-y)
    }  
    for(j in 1:length(Treatments)){ 
      if(Treatments[j] != "Control"){ 
        x = UptP[iPRow2018,paste0(Treatments[i],"_",Treatments[j])]
        y = UptP[iPRow2018,paste0("pred_P_UP_",Treatments[i],"_",Treatments[j])]
        points(x, y, col = cols[i],pch = pchs[j])
        err = c(err, x - y)
      }
    }
  }  
  text(x=0.5*ymaxP,y=0.2,paste0("RMSE = ", round(sqrt(mean(err^2,na.rm= TRUE)),1), " kg P/ha")) 
  mtext('a', side = 3, line = -2, adj = 0.99, cex = 1, col = "black")
  
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured K uptake, kg/ha",ylab="Predicted uptake, kg/ha",xlim=c(0,ymaxK),ylim=c(0,ymaxK))
  abline(0,1,col="black")
  
  err = NULL
  for(i in 1:length(Treatments)){
    if(Treatments[i] != "NP"){
      x = UptK[iPRow2016,paste0(Treatments[i],"_NONE")]
      y = UptK[iPRow2016,paste0("pred_K_UP_",Treatments[i])]
      points(x, y, col=cols[i],pch=16)
      err = c(err, x-y)
    }
    for(j in 1:length(Treatments)){
      if(Treatments[j] != "Control"){
        x = UptK[iPRow2018,paste0(Treatments[i],"_",Treatments[j])] 
        y = UptK[iPRow2018,paste0("pred_K_UP_",Treatments[i],"_",Treatments[j])]
        points(x, y, col="black",pch=pchs[j])
        err = c(err, x-y)
      }
    }  
  }
  text(x=0.5*ymaxK,y=1,paste0("RMSE = ", round(sqrt(mean(err^2,na.rm= TRUE)),1), " kg K/ha")) 
  mtext('b', side = 3, line = -2, adj = 0.99, cex = 1, col = "black")
  
  legend("topleft",legend=paste0(Treatments,"'16"),col=cols,pch=16,bty="n",cex = 0.8)
  j <- which(Treatments != "Control")
  legend("bottomright",legend=paste0(Treatments[j],"'18"),col="black",pch=pchs[j],bty="n",cex = 0.8)
  
  
  mtext("Measured uptake, kg/ha" , side = 1, outer = TRUE, line = -19, cex = 1.25, col = "black")
  mtext("Predicted uptake, kg/ha" , side = 2, outer = TRUE,  line = 0.5, cex = 1.25, col = "black", adj=0.9)
  
}

plot_results_trials_meas_vs_pred_Nuptake <- function(UptN, dNS1 = NULL, dNS2  = NULL){
  iRow2016 <- which(UptP[,"SEASON"] == "LR2016")
  iRow2018 <- which(UptP[,"SEASON"] == "LR2018")

  Treatments = c("Control",    "PK",     "NK",  "NP",      "NPK")
  cols       = c(    "red","orange","magenta","blue","darkgreen")
  pchs       = c(        0,       1,       2,      3,          4)
  par(mfrow=c(2,2),mar=c(2,2,1,0), oma=c(2, 2, 0.5,0.5))
  
  ymaxN=175

  #Combined 2016 + 2018 excluding NP 2016 for K uptake and NK 2016 for P uptake
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured N uptake, kg/ha",ylab="Predicted N uptake, kg/ha",xlim=c(0,ymaxN),ylim=c(0,ymaxN))
  abline(0,1,col="black")
  
  err = NULL
  for(i in 1:length(Treatments)){
      x = UptN[iRow2016,paste0(Treatments[i],"_NONE")] 
      y = UptN[iRow2016,paste0("pred_N_UP_",Treatments[i])]
      points(x, y, col=cols[i],pch = 16)
      err=  c(err, x-y)
  }
  text(x=0.5*ymaxN,y=0.2,paste0("RMSE = ", round(sqrt(mean(err^2,na.rm= TRUE)),1), " kg N/ha")) 
  mtext('a', side = 3, line = -2, adj = 0.99, cex = 1, col = "black")
  legend("topleft",legend=paste0(Treatments,"'16"),col=cols,pch=16,bty="n",cex = 0.8)
  
  plot(  -100,  -100 ,type="p",col="black",pch=16,
         xlab="Measured N uptake, kg/ha",ylab="Predicted N uptake, kg/ha",xlim=c(0,ymaxN),ylim=c(0,ymaxN))
  abline(0,1,col="black")
  err = NULL
  for(i in 1:length(Treatments)){
    for(j in 1:length(Treatments)){ 
      if(Treatments[j] != "Control"){ 
        x = UptN[iRow2018,paste0(Treatments[i],"_",Treatments[j])]
        y = UptN[iRow2018,paste0("pred_N_UP_",Treatments[i],"_",Treatments[j])]
        points(x, y, col = cols[i],pch = pchs[j])
        err = c(err, x - y)
      }
    }
  }  
  text(x=0.5*ymaxN,y=0.2,paste0("RMSE = ", round(sqrt(mean(err^2,na.rm= TRUE)),1), " kg N/ha")) 
  mtext('b', side = 3, line = -2, adj = 0.99, cex = 1, col = "black")
  
  j <- which(Treatments != "Control")
  legend("bottomright",legend=paste0(Treatments[j],"'18"),col=cols[j],pch=pchs[j],bty="n",cex = 0.8)
  
  
  mtext("Measured N uptake, kg/ha" , side = 1, outer = TRUE, line = -16, cex = 1.25, col = "black")
  mtext("Predicted N uptake, kg/ha" , side = 2, outer = TRUE,  line = 0.5, cex = 1.25, col = "black", adj=0.9)
  
}



plot_labile_pools_y1y7 <- function(DF_POOLS = out$DF_POOLS){
  df<-data.frame(K_labile_s1 = DF_POOLS[DF_POOLS[,"SEASON"]==1,"K_labile"],
                 P_labile_s1 = DF_POOLS[DF_POOLS[,"SEASON"]==1,"P_labile"],
                 K_labile_s7 = DF_POOLS[DF_POOLS[,"SEASON"]==7,"K_labile"],
                 P_labile_s7 = DF_POOLS[DF_POOLS[,"SEASON"]==7,"P_labile"],
                 K_stable_s1 = DF_POOLS[DF_POOLS[,"SEASON"]==1,"K_stable"],
                 P_stable_s1 = DF_POOLS[DF_POOLS[,"SEASON"]==1,"P_stable"],
                 K_stable_s7 = DF_POOLS[DF_POOLS[,"SEASON"]==7,"K_stable"],
                 P_stable_s7 = DF_POOLS[DF_POOLS[,"SEASON"]==7,"P_stable"])
  
  fit2 <- lm(K_labile_s7 ~ 1 + K_labile_s1, df)
  fit2s <- summary(fit2)
  fit3 <- lm(P_labile_s7 ~ 1 + P_labile_s1, df)
  fit3s <- summary(fit3)
  fit4 <- lm(K_stable_s7 ~ 1 + K_stable_s1, df)
  fit4s <- summary(fit4)
  fit5 <- lm(P_stable_s7 ~ 1 + P_stable_s1, df)
  fit5s <- summary(fit5)
  
  par(mfrow=c(2,2), mar=c(4,4,1,1))
  plot(df[,"K_labile_s1"],df[,"K_labile_s7"],
       xlab="Labile pool in season 1, kg/ha", ylab="Labile pool in season 7, kg/ha", main="K")
  lines(df[,"K_labile_s1"], predict(fit2, df), col="red")
  text(x=0.5*max(df[,"K_labile_s1"]),y=min(df[,"K_labile_s7"])+5,paste0("y = ", round(fit2s$coefficients[1],2), 
                                                 "+", round(fit2s$coefficients[2],2),
                                                 "x R2= ", round(fit2s$r.squared,2)),cex=0.7) 
  
  plot(df[,"P_labile_s1"],df[,"P_labile_s7"],
       xlab="Labile pool in season 1, kg/ha", ylab="Labile pool in season 7, kg/ha", main="P")
  lines(df[,"P_labile_s1"], predict(fit3, df), col="red")
  text(x=0.5*max(df[,"P_labile_s1"]),y=min(df[,"P_labile_s7"])+1,paste0("y = ", round(fit3s$coefficients[1],2), 
                                                 "+", round(fit3s$coefficients[2],2),
                                                 "x R2= ", round(fit3s$r.squared,2)),cex=0.7) 

  plot(df[,"K_stable_s1"],df[,"K_stable_s7"],
       xlab="Stable pool in season 1, kg/ha", ylab="Stable pool in season 7, kg/ha", main="K")
  lines(df[,"K_stable_s1"], predict(fit4, df), col="red")
  text(x=0.5*max(df[,"K_stable_s1"]),y=min(df[,"K_stable_s7"])+5,paste0("y = ", round(fit4s$coefficients[1],2), 
                                                 "+", round(fit4s$coefficients[2],2),
                                                 "x R2= ", round(fit4s$r.squared,2)),cex=0.7) 
  
  plot(df[,"P_stable_s1"],df[,"P_stable_s7"],
       xlab="Stable pool in season 1, kg/ha", ylab="Stable pool in season 7, kg/ha", main="P")
  lines(df[,"P_stable_s1"], predict(fit5, df), col="red")
  text(x=0.5*max(df[,"P_stable_s1"]),y=min(df[,"P_stable_s7"])+1,paste0("y = ", round(fit5s$coefficients[1],2), 
                                                 "+", round(fit5s$coefficients[2],2),
                                                 "x R2= ", round(fit5s$r.squared,2)),cex=0.7) 
  
  }


figFarmError <- function(out){
  #Determine error per farm
  uF <- unique(out$DF_POOLS[,"FARMCODE"])
  FarmErr <- NULL
  for (uf in uF) {
    for (season in c("LR2016","LR2018")){
      farmErrorP <- 0
      farmErrorK <- 0
      iFp <- which(out$UptP[,"FARMCODE"] == uf & out$UptP[,"SEASON"] == season)
      iFk <- which(out$UptK[,"FARMCODE"] == uf & out$UptK[,"SEASON"] == season)
      iFdf <- which(out$DF_POOLS[,"FARMCODE"] == uf & out$DF_POOLS[,"SEASON"] == 1)
      if(length(iFp) > 0){
        for(tr in c("Control","PK","NK","NP","NPK")){
          if(season == "LR2016"){
            farmErrorP <- farmErrorP + ifelse(is.na(out$UptP[iFp,paste0(tr,"_NONE")]), 0, 
                                              out$UptP[iFp,paste0("pred_P_UP_",tr)] - out$UptP[iFp,paste0(tr,"_NONE")])
            farmErrorK <- farmErrorK + ifelse(is.na(out$UptK[iFk,paste0(tr,"_NONE")]), 0, 
                                              out$UptK[iFk,paste0("pred_K_UP_",tr)] - out$UptK[iFp,paste0(tr,"_NONE")])
          }else{
            for(tr2 in c("PK","NK","NP","NPK")){
              farmErrorP <- farmErrorP + ifelse(is.na(out$UptP[iFp,paste0(tr,"_",tr2)]), 0, 
                                                out$UptP[iFp,paste0("pred_P_UP_",tr,"_",tr2)] - out$UptP[iFp,paste0(tr,"_",tr2)])
              farmErrorK <- farmErrorK + ifelse(is.na(out$UptK[iFk,paste0(tr,"_",tr2)]), 0, 
                                                out$UptK[iFk,paste0("pred_K_UP_",tr,"_",tr2)] - out$UptK[iFp,paste0(tr,"_",tr2)])
            }
          }
        }
        farmerr <- data.frame(FARMCODE = uf,
                              SEASON = season,
                              farmErrorP = farmErrorP,
                              farmErrorK = farmErrorK,
                              K_labile = out$DF_POOLS[iFdf,"K_labile"],
                              K_stable = out$DF_POOLS[iFdf,"K_stable"],
                              P_labile = out$DF_POOLS[iFdf,"P_labile"],
                              P_stable = out$DF_POOLS[iFdf,"K_stable"])
        FarmErr <- rbind(FarmErr, farmerr)
      }
    }  
  }
  par(mfcol=c(2,2))
  for (season in c("LR2016","LR2018")){
    iSeason <- which(FarmErr[,"SEASON"] == season)
    plot(FarmErr[iSeason,"P_labile"],FarmErr[iSeason,"farmErrorP"], type="p",col="white",
         xlab="Initial P labile kg/ha", ylab="Pred. - meas. uptake, kg P/ha", main=paste0("Cumulative farm error", season))
    text(x = FarmErr[iSeason,"P_labile"],
         y = FarmErr[iSeason,"farmErrorP"],
         labels = FarmErr[iSeason,"FARMCODE"],offset = 0.0)
    fit <- lm(farmErrorP ~ 1+ P_labile, FarmErr[iSeason,])
    sfit <- summary(fit)
    abline(sfit$coefficients[1],sfit$coefficients[2], col = "red")
    text(x = 0.5*max(FarmErr[iSeason,"P_labile"]),
         y = min(FarmErr[iSeason,"farmErrorP"]) + 2,
         paste0("y = ", round(sfit$coefficients[1],2), 
                "+", round(sfit$coefficients[2],2),
                "x R2= ", round(sfit$r.squared,2)),cex = 0.7) 
    
    plot(FarmErr[iSeason,"K_labile"],
         FarmErr[iSeason,"farmErrorK"], type = "p",col="white",
         xlab = "Initial K labile kg/ha", 
         ylab = "Pred. - meas. uptake, kg K/ha", main=paste0("Cumulative farm error", season))
    text(x = FarmErr[iSeason,"K_labile"],
         y = FarmErr[iSeason,"farmErrorK"],
         labels = FarmErr[iSeason,"FARMCODE"], offset = 0.0)
    fit <- lm(farmErrorK ~ 1+ K_labile, FarmErr[iSeason,])
    sfit <- summary(fit)
    abline(sfit$coefficients[1],sfit$coefficients[2], col = "red")
    text(x = 0.5*max(FarmErr[iSeason,"K_labile"]),
         y = min(FarmErr[iSeason,"farmErrorK"]) + 10,
         paste0("y = ", round(sfit$coefficients[1],2), 
                "+", round(sfit$coefficients[2],2),
                "x R2= ", round(sfit$r.squared,2)),cex=0.7) 
  }
  return(FarmErr)
}

#Compare states for all farms with selected treatments
compare_states_farms <- function(TREATMENT, NEW.TREATMENT, results_out){
  par(mfrow=c(2,3),mar=c(2,2,1,0), oma=c(2, 2, 0.5,0.5))
  
  uF<- unique(results_out[,"FARMCODE"])
  cols<-rainbow(length(uF))
  for(state in c("P_labile", "P_labile_placed", "P_stable", 
                 "K_labile", "K_stable")){
    plot(x=-100, y=-100, xlim=c(0,11), ylim=c(0,max(results_out[,state])),
         xlab="Time", ylab="Pool size, kg/ha", main=state)
    nr=0
    for(uf in uF){
      nr=nr+1
      #Phase 1
      ii<-which(results_out[,"FARMCODE"] == uf &
                  results_out[,"TREATMENT"] == TREATMENT &
                  results_out[,"NEW.TREATMENT"] == "NONE" )
      lines(results_out[ii,"time"],results_out[ii,state], col=cols[nr])
      #Phase 2
      ii<-which(results_out[,"FARMCODE"] == uf &
                  results_out[,"TREATMENT"] == TREATMENT &
                  results_out[,"NEW.TREATMENT"] == NEW.TREATMENT )
      lines(results_out[ii,"time"],results_out[ii,state], col=cols[nr])
    }
  }
  plot(x=-100, y=-100, xlim=c(0,11), ylim=c(0,100),
       xlab="Time", ylab="Pool size, kg/ha", main=state)
  legend("topleft", legend=uF, col=cols, lty=1, ncol = 2, bty="n")
  text(x=0, y=10, adj=c(0,0), labels=paste0("Phase 1: ", TREATMENT))
  text(x=0, y=5, adj=c(0,0), labels=paste0("Phase 2: ",NEW.TREATMENT))
}

#END PLOTTING FUNCTIONS==================================
