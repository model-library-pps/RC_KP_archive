require("pracma")

SoilP <- read.csv("..//Data//Broadbalk soil P total analysis mgP_kgSoil.csv")
selyears <- c(2000)

#BD:
#https://www.era.rothamsted.ac.uk/experiment/rbk1/soilphys#documents then scroll down to Broadbalk site and soil physical properties, keep scrolling and you get to soil weights for 0-23cm soil layers. 
#To convert to bulk density, g/cm3, divide by 23 and multiply by 10. So the inorganic plots (2.88E6 kg topsoil/ha) is 1.25 g/cm3 and the FYM plots (2.52E6 kg topsoil/ha) is 1.10 g/cm3. 
#BD data from 2000
#Soil depths of FYM treatments were increased to have same amounts of soil to compare treatments.
#1250 * 0.25 / 100 = 0.2614 cm. 
cn <- colnames(SoilP)

parListNamesAll <- c("N1PKMg","PKMg","Control",    "N",  "NKMg",   "NP",  "N5PKMg","FYMNPK",   "FYM")
cols            <- c("green", "blue",    "red", "grey", "orange", "cyan","magenta",  "gold", "black")
pchs            <- c(      1,    16,       1,        1,       16,      1,       16,      16,      17)
BD_kgm3         <- c(   1250,   1250,   1250,     1250,     1250,   1250,     1250,    1100,    1100)
SD_m            <- c(   0.23,   0.23,   0.23,     0.23,     0.23,   0.23,     0.23,  0.2614,  0.2614)

ParSET <- "LMpred_BBalk"

pdf(paste("..//Results//16a. Broadbalk P total vs Stable P ",ParSET, ".pdf"))
  
  
  DFCOMP <- NULL
  for (i in 2:length(cn)) {
    kk <- which(cn[i] == parListNamesAll)
    if(length(kk) > 0 ){
      df <- read.csv(paste("..//Results//14. Estimated pools for treatment ", cn[i], " and parameter set ",ParSET, ".csv"))
      for(y in selyears){
        ii <-which(df[,"time"] == y)
        df <- df[ii[1],]
        jj <-which(SoilP[,"Year"] == y)
        
        dfcomp<- data.frame(trt = cn[i],
                            year = df[,"time"],
                            #kg/ha =           mg/kg * kg/m3       * m        * m2/ha * kg/mg
                            Ptotal = SoilP[jj,cn[i]] * BD_kgm3[kk] * SD_m[kk] * 1E4   *  1E-6,
                            #kg/ha
                            Pmod = df[,"P_stable"] + df[,"P_labile"] + df[,"P_labile_FYM"]+ df[,"P_labile_placed"],
                            pch=pchs[kk],
                            col=cols[kk])
        DFCOMP <- rbind(DFCOMP, dfcomp)
      }
      
    }
  }
  plot(x=DFCOMP[,"Ptotal"], y=DFCOMP[,"Pmod"], 
       xlab ="P total, kg/ha", ylab ="Modelled labile + stable P, kg/ha",
       col=DFCOMP[,"col"], pch=DFCOMP[,"pch"])
  legend("topleft",legend = DFCOMP[,"trt"], col=DFCOMP[,"col"],pch=DFCOMP[,"pch"], bty="n")
  
  fit <- lm(Pmod ~Ptotal, DFCOMP)
  afit <-anova(fit)
  sfit <- summary(fit)
  txt <- paste0("R2 = ", round(sfit$r.squared,2),
                ", y = ",round(sfit$coefficients[1],1),
                " + ", round(sfit$coefficients[2],2)," x")
  text(x=2500,y=1500,labels=txt)
  
  xx <- seq(min(DFCOMP[,"Ptotal"]),max(DFCOMP[,"Ptotal"]),1)
  lines(x=xx, y=sfit$coefficients[1] + xx * sfit$coefficients[2], col="red", lty=1, lwd=1.5)
  
dev.off()
