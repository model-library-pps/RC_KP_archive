require("pracma")

SoilP <- read.csv("..//Data//Broadbalk soil Olsen P analysis mgP_kgSoil.csv")

cn <- colnames(SoilP)

parListNamesAll <- c("N1PKMg"  ,"PKMg",   "Control",    "N",   "NKMg",   "NP",  "N5PKMg",    "FYMNPK",   "FYM")
cols            <- c("green", "blue",     "red", "grey", "orange", "cyan","magenta", "gold", "black")
pchs            <- c(      1,    16,        1,        1,     16,      1,       16,          16,      17)

selyears <- c(1968, 1988, 1998)
ParSET <- "LMpred_BBalk"

pdf(paste("..//Results//16. Broadbalk OlsenP vs Labile P ",ParSET, ".pdf"))
  
  plot(x=-50, y=-50, xlim=c(0,110), ylim=c(0,200), xlab ="Olsen P, mg/kg", ylab ="Labile P, kg/ha")
  
  DFCOMP <- NULL
  for (i in 2:length(cn)) {
    ii <- which(cn[i] == parListNamesAll)
    
    df <- read.csv(paste("..//Results//14. Estimated pools for treatment ", cn[i], " and parameter set ",ParSET, ".csv"))
    df <- df[which(df[,"time"] %in% selyears),]
    
    dfcomp<- data.frame(trt = cn[i],
                        year = df[,"time"],
                        OlsenP = interp1(SoilP[,"Year"],SoilP[,cn[i]],selyears),
                        LabileP = df[,"P_labile"] + df[,"P_labile_FYM"]+ df[,"P_labile_placed"])
    DFCOMP <- rbind(DFCOMP, dfcomp)
    points(dfcomp[,"OlsenP"],dfcomp[,"LabileP"],col=cols[ii],pch=pchs[ii])
  }
  fit <- lm(LabileP ~OlsenP, DFCOMP)
  afit <-anova(fit)
  sfit <- summary(fit)
  
  xx <- seq(min(DFCOMP[,"OlsenP"]),max(DFCOMP[,"OlsenP"]),1)
  lines(x=xx, y=sfit$coefficients[1] + xx * sfit$coefficients[2], col="red", lty=1, lwd=1.5)
  
dev.off()
