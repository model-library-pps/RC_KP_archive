
selyears <- seq(1961,2008,1)

SoilP <- read.csv("..//Data//Hanninghof soil P analysis.csv", stringsAsFactors = FALSE)
SoilP <- SoilP[which(SoilP[,"Year"] %in% selyears),]
SoilP[SoilP[,"P.CAL_0.30..mg.P2O5.100g."] == "Missing", "P.CAL_0.30..mg.P2O5.100g."] <- NA
SoilP[,"P.CAL_0.30..mg.P2O5.100g."]<- as.numeric(SoilP[,"P.CAL_0.30..mg.P2O5.100g."])

SoilK <- read.csv("..//Data//Hanninghof soil K analysis.csv", stringsAsFactors = FALSE)
SoilK <- SoilK[which(SoilK[,"Year"] %in% selyears),]
SoilK[SoilK[,"K.CAL..mg.K2O.100g."] == "Missing", "K.CAL..mg.K2O.100g."] <- NA
SoilK[,"K.CAL..mg.K2O.100g."]<- as.numeric(SoilK[,"K.CAL..mg.K2O.100g."])



#Change column names for FYM+ 
AdaptDescription <- function(SoilPK){
  SoilPK[,"Description"]<- as.character(SoilPK[,"Description"])
  SoilPK[SoilPK[,"Description"] == "FYM+N","Description"] <- "FYMN"
  SoilPK[SoilPK[,"Description"] == "FYM+PK","Description"] <- "FYMPK"
  SoilPK[SoilPK[,"Description"] == "FYM+NK","Description"] <- "FYMNK"
  SoilPK[SoilPK[,"Description"] == "FYM+NP","Description"] <- "FYMNP"
  SoilPK[SoilPK[,"Description"] == "FYM+NPK","Description"] <- "FYMNPK"
  SoilPK[SoilPK[,"Description"] == "FYM+NPKMg","Description"] <- "FYMNPKMg"
  return(SoilPK)
}
SoilP <- AdaptDescription(SoilPK = SoilP)
SoilK <- AdaptDescription(SoilPK = SoilK)

fit <- lm(P.CAL_0.30..mg.P2O5.100g.~1+Year+Description+Crop, SoilP)
summary(fit)
anova(fit)

fit <- lm(K.CAL..mg.K2O.100g.~1+Year*Description+Crop, SoilK)
summary(fit)
anova(fit)


parListNamesAll <- c("NPK"  ,  "PK",     "NK", "Control",    "N",   "NP",  "NPKMg",  "FYMNPK",   "FYM",   "FYMPK",     "FYMNK",       "FYMNP",    "FYMNPKMg")
cols            <- c("green", "blue","orange",     "red", "grey", "cyan","magenta",     "green", "black",     "blue",   "orange",      "cyan",     "magenta")
pchs            <- c(      1,     1,        1,       1,        1,      1,        1,           2,       2,          2,          2,           2,             2)
lwds            <- c(      2,     2,        2,       2,        2,      2,        2,           2,       2,          2,          2,           2,             2)
ltys            <- c(      1,     1,        1,       1,        1,      1,        1,           2,       2,          2,          2,           2,             2)

pdf(paste("..//Results//5. Hanninghof time vs PCAL and KCAL.pdf"))
  par(mfrow=c(2,1), mar=c(4,4,1,1))
  
  plot(-100,-100, xlab="year", ylab="P-CAL, mg P/kg",xlim=c(1960,2010),ylim=c(0,38) )
  for (trt in parListNamesAll) {
    ii <- which(trt == parListNamesAll)
    soilp <- subset(SoilP, Description == trt)
    lines(soilp[,"Year"],soilp[,"P.CAL_0.30..mg.P2O5.100g."]/2.29, col = cols[ii], pch = pchs[ii], lty=ltys[ii])
  }
  legend("topleft",legend=parListNamesAll, col=cols, lty = ltys, lwd = lwds, cex=0.5, bty="n")
  
  plot(-100,-100, xlab="year", ylab="K-CAL, mg K/kg",xlim=c(1960,2010),ylim=c(0,30) )
  for (trt in parListNamesAll) {
    ii <- which(trt == parListNamesAll)
    soilk <- subset(SoilK, Description == trt)
    lines(soilk[,"Year"],soilk[,"K.CAL..mg.K2O.100g."]/1.2195, col = cols[ii], pch = pchs[ii], lty=ltys[ii])
  }
  
dev.off()

ParSET <- "LMpred_FYM_Hanninghof"

pdf(paste("..//Results//5. Hanninghof legend ",ParSET, ".pdf"))
  plot(x=-5, y=-5, xlim=c(0,350), ylim=c(0,200), xlab ="K CAL, mg/kg", ylab ="Measured uptake, kg K/ha")
  legend("topleft",legend=parListNamesAll, col=cols, pch = pchs)
  plot(x=-5, y=-5, xlim=c(0,350), ylim=c(0,200), xlab ="K CAL, mg/kg", ylab ="Measured uptake, kg K/ha")
  legend("topleft",legend=parListNamesAll, col=cols, lty = ltys, lwd = lwds)
dev.off()  

  DFCOMP <- NULL
  for (trt in parListNamesAll) {
    soilp <- subset(SoilP, Description == trt)
    soilk <- subset(SoilK, Description == trt)
    
    df <- read.csv(paste("..//Results//3. Estimated pools for treatment ", trt, " and parameter set ",ParSET, ".csv"))
    df <- df[which(df[,"time"] %in% soilp[,"Year"]),]
    upP <- UptP[which(UptP[,"Year"] %in% soilp[,"Year"]),]
    upK <- UptK[which(UptK[,"Year"] %in% soilp[,"Year"]),]
    
    dfcomp<- data.frame(trt = trt,
                        year = df[,"time"],
                        #mgP/kg = mg P2O5/100g * 100g/kg * mg P20f/mgP 
                        PCAL = soilp[,"P.CAL_0.30..mg.P2O5.100g."] * 10 * 0.4365,
                        LabileP = df[,"P_labile"] + df[,"P_labile_FYM"]+ df[,"P_labile_placed"],
                        Pred_P_up = df[,"P_up"],
                        Meas_P_up = upP[,trt],
                        KCAL = soilk[,"K.CAL..mg.K2O.100g."] * 10 * 0.82,
                        LabileK = df[,"K_labile"] + df[,"K_labile_FYM"],
                        Pred_K_up = df[,"K_up"],
                        Meas_K_up = upK[,trt])
    DFCOMP <- rbind(DFCOMP, dfcomp)
  }
  fitP <- lm(LabileP ~PCAL, DFCOMP)
  afitP <-anova(fitP)
  sfitP <- summary(fitP)
  print(sfitP)
  fitK <- lm(LabileK ~KCAL, DFCOMP)
  afitK <-anova(fitK)
  sfitK <- summary(fitK)
  print(sfitK)
  
  pdf(paste("..//Results//5. Hanninghof CALPK vs Labile PK ",ParSET, ".pdf"))
    par(mfrow=c(2,2), mar=c(4,4,1,1))
    
    plot(x=-5, y=-5, xlim=c(0,350), ylim=c(200,2200), xlab ="P CAL, mg/kg", ylab ="Labile P, kg/ha")
    for (trt in parListNamesAll) {
      ii <- which(trt == parListNamesAll)
      jj <- which(trt == DFCOMP[,"trt"])
      points(DFCOMP[jj,"PCAL"],DFCOMP[jj,"LabileP"],col=cols[ii],pch=pchs[ii])
    }
    xx <- seq(min(DFCOMP[,"PCAL"]),max(DFCOMP[,"PCAL"]),1)
    lines(x=xx, y=sfitP$coefficients[1] + xx * sfitP$coefficients[2], col="red", lty=1, lwd=1.5)

    plot(x=-5, y=-5, xlim=c(0,350), ylim=c(200,3000), xlab ="K CAL, mg/kg", ylab ="Labile K, kg/ha")
    for (trt in parListNamesAll) {
      ii <- which(trt == parListNamesAll)
      jj <- which(trt == DFCOMP[,"trt"])
      points(DFCOMP[jj,"KCAL"],DFCOMP[jj,"LabileK"],col=cols[ii],pch=pchs[ii])
    }
    xx <- seq(min(DFCOMP[,"KCAL"]),max(DFCOMP[,"KCAL"]),1)
    lines(x=xx, y=sfitK$coefficients[1] + xx * sfitK$coefficients[2], col="red", lty=1, lwd=1.5)

    plot(x=-5, y=-5, xlim=c(0,350), ylim=c(0,35), xlab ="P CAL, mg/kg", ylab ="Measured uptake, kg P/ha")
    for (trt in parListNamesAll) {
      ii <- which(trt == parListNamesAll)
      jj <- which(trt == DFCOMP[,"trt"])
      points(DFCOMP[jj,"PCAL"],DFCOMP[jj,"Meas_P_up"],col=cols[ii],pch=pchs[ii])
    }
    
    plot(x=-5, y=-5, xlim=c(0,350), ylim=c(0,200), xlab ="K CAL, mg/kg", ylab ="Measured uptake, kg K/ha")
    for (trt in parListNamesAll) {
      ii <- which(trt == parListNamesAll)
      jj <- which(trt == DFCOMP[,"trt"])
      points(DFCOMP[jj,"KCAL"],DFCOMP[jj,"Meas_K_up"],col=cols[ii],pch=pchs[ii])
    }
    
          
dev.off()
