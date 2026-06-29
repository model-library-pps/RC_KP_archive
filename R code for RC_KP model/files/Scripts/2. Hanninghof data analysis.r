#Figures for analysis of Hanninghof dataset
#
#Authors: AGT Schut and W. Reymann
#         Plant Production Systems group 
#         Wageningen University, 2021
source('RC_KP_model.r')
source('CNE_triangle_plot.r')


#INPUTS==============================================================================
data <- read.csv("..//Data//Processed//1. HanningHof_rotation_NPK_offtakes_kgha.csv")
#INPUTS==============================================================================


#OUTPUTS==============================================================================
#names of files produced
Accumulation_Dilution_pdf <- "..//Results//2. HanningHof. Fitted accumulation and dilution curves.pdf"
pCNE_no_FYM_pdf           <- "..//Results//2. pCNE vs time HanningHof_no FYM treatments.pdf"
pCNE_FYM_pdf              <- "..//Results//2. pCNE vs time HanningHof_FYM treatments.pdf"
sumCNE_no_FYM_pdf         <- "..//Results//2. sum CNE HanningHof_no FYM treatments.pdf"
sumCNE_FYM_pdf            <- "..//Results//2. sum CNE HanningHof_FYM treatments.pdf"
CNE_triange_FYM_pdf       <- "..//Results//2. CNE triangle_HanningHof_colors_per_year_per crop (rows) for Co NPK FYM+NPK(cols).pdf"
CNE_triangle_per_crop_pdf <- "..//Results//2. CNE triangle_HanningHof_colors_per_crop_type_for NPK, NP, NK PK.pdf"
CNE_triangle_per_year_pdf <- "..//Results//2. CNE triangle_HanningHof_colors_per_year_per crop (rows) for NP NK PK (cols).pdf"
#OUTPUTS==============================================================================

#FUNCTIONS===============================================================================================
##Plot sum CNE 
plot_sum_CNE <- function(data=data, Crop, treatments=c("NPK","NP","NK","PK","Control"),show_legend=FALSE){
  ii <- which(data[,"Crop"] == Crop)
  xmin=min(data[ii,"sumkCNE"], na.rm = TRUE)
  xmax=max(data[ii,"sumkCNE"], na.rm = TRUE)
  ymin=min(data[ii,"Yield.t.ha"], na.rm = TRUE)
  ymax=max(data[ii,"Yield.t.ha"], na.rm = TRUE)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[1])
  plot(data[ii,"sumkCNE"],data[ii,"Yield.t.ha"],col="green", pch=1, 
       xlim=c(xmin,xmax),ylim=c(ymin,ymax),
       xlab="CNE_N+CNE_P+CNE_K, kCNE", ylab="Yield, t/ha", main=Crop)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[2])
  points(data[ii,"sumkCNE"],data[ii,"Yield.t.ha"],col="blue", pch=2)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[3])
  points(data[ii,"sumkCNE"],data[ii,"Yield.t.ha"],col="orange", pch=3)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[4])
  points(data[ii,"sumkCNE"],data[ii,"Yield.t.ha"],col="red", pch=4)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[5])
  points(data[ii,"sumkCNE"],data[ii,"Yield.t.ha"],col="black", pch=5)
  
  
  #Quadratic model for all data
  ii <- which(data[,"Crop"] == Crop)
  data[,"sumkCNE2"]<-data[,"sumkCNE"]^2
  fit<-lm(Yield.t.ha ~ 1 + sumkCNE + sumkCNE2, data[ii,])
  sfit<-summary(fit)
  print(sfit)
  xx <- seq(0,1000,1)
  yy <- sfit$coefficients[1] + sfit$coefficients[2]*xx + sfit$coefficients[3]*xx^2
  lines(x=xx,y=yy,col="black")
  text(xmin+0.6*(xmax-xmin), y=ymin+0.025*(ymax-ymin),
       paste0("y = ",round(sfit$coefficients[1],3),"+", 
              round(sfit$coefficients[2],4),"x +",
              round(sfit$coefficients[3],5)," x^2"),cex=0.8)
  text(xmin+0.9*(xmax-xmin), y=ymin+0.1*(ymax-ymin),
       paste0("R2 = ", round(sfit$r.squared,2)),cex=0.8)
  
  
  if(show_legend){
    legend("topleft",legend=treatments,col=c("green", "blue","orange","red","black"),pch=c(1,2,3,4,5),cex=0.8,bty="n")
  }
  
}

plot_pCNE <- function(data, Crop, yvar = "pCNE_N", treatments=c("NPK","NP","NK","PK","Control"),show_legend=FALSE){
  ii <- which(data[,"Crop"] == Crop)
  xmin=min(data[ii,"Year"], na.rm = TRUE)
  xmax=max(data[ii,"Year"], na.rm = TRUE)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[1])
  plot(data[ii,"Year"],data[ii,yvar],col="green", pch=1, 
       xlim=c(xmin,xmax),ylim=c(15,60),
       xlab="Year", ylab=yvar, main=Crop)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[2])
  points(data[ii,"Year"],data[ii,yvar],col="blue", pch=2)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[3])
  points(data[ii,"Year"],data[ii,yvar],col="orange", pch=3)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[4])
  points(data[ii,"Year"],data[ii,yvar],col="red", pch=4)
  ii <- which(data[,"Crop"] == Crop &  data[,"Description"] == treatments[5])
  points(data[ii,"Year"],data[ii,yvar],col="black", pch=5)
  lines(c(0,5000),c(33.3,33.3),col="red",lty=3, lwd=2)
  ii <- which(data[,"Crop"] == Crop)
  if(show_legend){
    legend("topleft",legend=treatments,col=c("green", "blue","orange","red","black"),pch=c(1,2,3,4,5),cex=0.8,bty="n")
  }
  
}
#FUNCTIONS===============================================================================================


#get QUEFTS parameters, expressed per kg of fresh product
PKparam <- RCKP_param(type = "ASP", Crops = c("silage maize", "potato","oats","rye"))
parQUEFTS <- data.frame(Crop = PKparam$CropQUEFTS_Fresh.crop,
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

parQUEFTS[,"PhEmedN"] <-  (parQUEFTS[,"aN"] + parQUEFTS[,"dN"])/2
parQUEFTS[,"PhEmedP"] <-  (parQUEFTS[,"aP"] + parQUEFTS[,"dP"])/2
parQUEFTS[,"PhEmedK"] <-  (parQUEFTS[,"aK"] + parQUEFTS[,"dK"])/2

#Add CNE per nutrient and as percentage of the sum
for(i in 1:nrow(data)){
  iQ <- which(parQUEFTS[,"Crop"] == data[i,"Crop"])
  data[i,"rCNE_N"] <- 100 * ((data[i, "N.offtake.kg.ha"] - data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "dN"])
                           /(data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "aN"] - data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "dN"]))
  data[i,"rCNE_P"] <- 100 * ((data[i, "P.offtake.kg.ha"] - data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "dP"])
                           /(data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "aP"] - data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "dP"]))
  data[i,"rCNE_K"] <- 100 * ((data[i, "K.offtake.kg.ha"] - data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "dK"])
                           /(data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "aK"] - data[i,"Yield.t.ha"]*1000 / parQUEFTS[iQ, "dK"]))
  
  data[i,"kCNE_N"] <- data[i, "N.offtake.kg.ha"] * parQUEFTS[iQ, "PhEmedN"]/parQUEFTS[iQ, "PhEmedN"]
  #kg N CNE / ha               kg P/ha           *            kg Y/kg P *    kg N/ kg Y 
  data[i,"kCNE_P"] <- data[i, "P.offtake.kg.ha"] * parQUEFTS[iQ, "PhEmedP"]/parQUEFTS[iQ, "PhEmedN"]
  #kg N CNE / ha               kg K/ha           *            kg Y/kg K *    kg N/ kg Y 
  data[i,"kCNE_K"] <- data[i, "K.offtake.kg.ha"] * parQUEFTS[iQ, "PhEmedK"]/parQUEFTS[iQ, "PhEmedN"]
}
data[,"sumkCNE"] <- data[,"kCNE_N"] + data[,"kCNE_P"] + data[,"kCNE_K"]
data[,"pCNE_N"] <- 100 * data[,"kCNE_N"] / data[,"sumkCNE"]
data[,"pCNE_P"] <- 100 * data[,"kCNE_P"] / data[,"sumkCNE"]
data[,"pCNE_K"] <- 100 * data[,"kCNE_K"] / data[,"sumkCNE"]



#Check accumulation and dilution
pdf(Accumulation_Dilution_pdf)
  par(mfcol=c(3,4), mar=c(2,2,1,0), oma=c(3,3,0,0))
  for(cr in c("potato", "rye", "oats", "silage maize")){
    iQ <- which(parQUEFTS[,"Crop"] == cr)
    ii <- which(data[,"Crop"] == cr)
    plot(data[ii, "N.offtake.kg.ha"], data[ii, "Yield.t.ha"], main= cr, xlab="Uptake, kg N/ha", ylab="Yield, t/ha")
    abline(a = -parQUEFTS[iQ,"rN"] * parQUEFTS[iQ,"aN"]/1000, b = parQUEFTS[iQ,"aN"]/1000, col = "red")
    abline(a = -parQUEFTS[iQ,"rN"] * parQUEFTS[iQ,"dN"]/1000, b = parQUEFTS[iQ,"dN"]/1000, col = "red")
    plot(data[ii, "P.offtake.kg.ha"],data[ii, "Yield.t.ha"],main="",xlab="Uptake, kg P/ha", ylab= "Yield, t/ha")
    abline(a = -parQUEFTS[iQ,"rP"] * parQUEFTS[iQ,"aP"]/1000, b = parQUEFTS[iQ,"aP"]/1000, col = "red")
    abline(a = -parQUEFTS[iQ,"rP"] * parQUEFTS[iQ,"dP"]/1000, b = parQUEFTS[iQ,"dP"]/1000, col = "red")
    plot(data[ii, "K.offtake.kg.ha"],data[ii, "Yield.t.ha"],main="",xlab="Uptake, kg K/ha", ylab = "Yield, t/ha")
    abline(a = -parQUEFTS[iQ,"rK"] * parQUEFTS[iQ,"aK"]/1000, b = parQUEFTS[iQ,"aK"]/1000, col = "red")
    abline(a = -parQUEFTS[iQ,"rK"] * parQUEFTS[iQ,"dK"]/1000, b = parQUEFTS[iQ,"dK"]/1000, col = "red")
  }
  mtext("N (top row), P, K (bottom) uptake, kg/ha",side=1, outer=TRUE, line=1)
  mtext("Yield, t/ha",side=2, outer=TRUE, line=1)
dev.off()

pdf(pCNE_no_FYM_pdf)
  par(mfcol = c(3,3), mar=c(2,2,1,0), oma=c(2,2,1,1))
  plot_pCNE(data=data, Crop="potato",yvar = "pCNE_N", treatments=c("NPK","NP","NK","PK","Control"))
  mtext("pCNE N, %",side=2, outer=FALSE, line=2)
  plot_pCNE(data=data, Crop="potato",yvar = "pCNE_P", treatments=c("NPK","NP","NK","PK","Control"))
  mtext("pCNE P, %",side=2, outer=FALSE, line=2)
  plot_pCNE(data=data, Crop="potato",yvar = "pCNE_K", treatments=c("NPK","NP","NK","PK","Control"))
  mtext("pCNE K, %",side=2, outer=FALSE, line=2)
  plot_pCNE(data=data, Crop="rye",yvar = "pCNE_N", treatments=c("NPK","NP","NK","PK","Control"),show_legend=TRUE)
  plot_pCNE(data=data, Crop="rye",yvar = "pCNE_P", treatments=c("NPK","NP","NK","PK","Control"))
  plot_pCNE(data=data, Crop="rye",yvar = "pCNE_K", treatments=c("NPK","NP","NK","PK","Control"))
  plot_pCNE(data=data, Crop="oats",yvar = "pCNE_N", treatments=c("NPK","NP","NK","PK","Control"))
  plot_pCNE(data=data, Crop="oats",yvar = "pCNE_P", treatments=c("NPK","NP","NK","PK","Control"))
  plot_pCNE(data=data, Crop="oats",yvar = "pCNE_K", treatments=c("NPK","NP","NK","PK","Control"))
  mtext("Year",side=1, outer=TRUE, line=0)
dev.off()

pdf(pCNE_FYM_pdf)
  par(mfcol = c(3,3), mar=c(2,2,1,0), oma=c(2,2,1,1))
  plot_pCNE(data=data, Crop="potato",yvar = "pCNE_N", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  mtext("pCNE N, %",side=2, outer=FALSE, line=2)
  plot_pCNE(data=data, Crop="potato",yvar = "pCNE_P", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  mtext("pCNE P, %",side=2, outer=FALSE, line=2)
  plot_pCNE(data=data, Crop="potato",yvar = "pCNE_K", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  mtext("pCNE K, %",side=2, outer=FALSE, line=2)
  plot_pCNE(data=data, Crop="rye",yvar = "pCNE_N", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"),show_legend=TRUE)
  plot_pCNE(data=data, Crop="rye",yvar = "pCNE_P", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  plot_pCNE(data=data, Crop="rye",yvar = "pCNE_K", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  plot_pCNE(data=data, Crop="oats",yvar = "pCNE_N", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  plot_pCNE(data=data, Crop="oats",yvar = "pCNE_P", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  plot_pCNE(data=data, Crop="oats",yvar = "pCNE_K", treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK"))
  mtext("Year",side=1, outer=TRUE, line=0)
dev.off()

pdf(sumCNE_no_FYM_pdf)
  par(mfrow = c(2,2), mar=c(2,2,1,0), oma=c(2,2,1,1))
  plot_sum_CNE(data=data, Crop="potato",treatments=c("NPK","NP","NK","PK","Control"))
  plot_sum_CNE(data=data, Crop="rye",treatments=c("NPK","NP","NK","PK","Control"))
  plot_sum_CNE(data=data, Crop="oats",treatments=c("NPK","NP","NK","PK","Control"))
  plot_sum_CNE(data=data, Crop="silage maize",treatments=c("NPK","NP","NK","PK","Control"),show_legend = TRUE)
dev.off()

pdf(sumCNE_FYM_pdf)
  par(mfrow = c(2,2), mar=c(2,2,1,0), oma=c(2,2,1,1))
  plot_sum_CNE(data=data, Crop="potato",treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK","Control"))
  plot_sum_CNE(data=data, Crop="rye",treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK","Control"))
  plot_sum_CNE(data=data, Crop="oats",treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK","Control"))
  plot_sum_CNE(data=data, Crop="silage maize",treatments=c("FYM+NPK","FYM+NP","FYM+NK","FYM+PK","Control"),show_legend = TRUE)
dev.off()


#Add some colors and symbols per crop type
data[data[,"Crop"] == "potato", "leg_text"] <- "potato"
data[data[,"Crop"] == "potato", "colors"] <- "red"
data[data[,"Crop"] == "potato", "pch"] <- 21
data[data[,"Crop"] == "silage maize", "leg_text"] <- "sil.m"
data[data[,"Crop"] == "silage maize", "colors"] <- "blue"
data[data[,"Crop"] == "silage maize", "pch"] <- 22
data[data[,"Crop"] == "rye", "leg_text"] <- "rye"
data[data[,"Crop"] == "rye", "colors"] <- "black"
data[data[,"Crop"] == "rye", "pch"] <- 23
data[data[,"Crop"] == "oats", "leg_text"] <- "oats"
data[data[,"Crop"] == "oats", "colors"] <- "green"
data[data[,"Crop"] == "oats", "pch"] <- 24

xyoff = 15

pdf(CNE_triangle_per_crop_pdf)
  par(mfrow = c(2,2), mar=c(2,0,2,0))
  ii <- which(data[,"Description"] == "NPK")
  plot_triangle(data[ii,], defined_symbols = TRUE, show_legend = FALSE, main = "NPK", xyoff = xyoff, optimum_threshold = 28)
  ii <- which(data[,"Description"] == "NP")
  plot_triangle(data[ii,], defined_symbols = TRUE, show_legend = TRUE, main = "NP", xyoff = xyoff, optimum_threshold = 28)
  ii <- which(data[,"Description"] == "NK")
  plot_triangle(data[ii,], defined_symbols = TRUE, show_legend = FALSE, main = "NK", xyoff = xyoff, optimum_threshold = 28)
  ii <- which(data[,"Description"] == "PK")
  plot_triangle(data[ii,], defined_symbols = TRUE, show_legend = FALSE, main = "PK", xyoff = xyoff, optimum_threshold = 28)
dev.off()

#sort the data on year
is <- sort(data[,"Year"], index.return = TRUE)
sdata <- data[is$ix,]
sdata[,"colors"] <- rainbow(nrow(sdata))
sdata[, "leg_text"] <- sdata[,"Year"]
sdata[, "pch"] <- 21

#Plot as in rotation: potato, rye and oats 
pdf(CNE_triangle_per_year_pdf)
  
  par(mfrow = c(3,3), mar=c(2,1,1,1))
  
  treatment = "NP"
  ii <- which(sdata[,"Crop"] == "potato" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " potato"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "rye" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " rye"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "oats" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " oats"), xyoff = xyoff, optimum_threshold = 28)
  
  treatment = "NK"
  ii <- which(sdata[,"Crop"] == "potato" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " potato"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "rye" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " rye"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "oats" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " oats"), xyoff = xyoff, optimum_threshold = 28)
  
  treatment = "PK"
  ii <- which(sdata[,"Crop"] == "potato" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " potato"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "rye" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " rye"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "oats" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " oats"), xyoff = xyoff, optimum_threshold = 28)
  
dev.off()

#Plot as in rotation: potato, rye and oats 
pdf(CNE_triange_FYM_pdf)
  
  par(mfrow = c(3,3), mar=c(2,1,1,1))
  
  treatment = "Control"
  ii <- which(sdata[,"Crop"] == "potato" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " potato"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "rye" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " rye"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "oats" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " oats"), xyoff = xyoff, optimum_threshold = 28)
  
  treatment = "NPK"
  ii <- which(sdata[,"Crop"] == "potato" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " potato"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "rye" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " rye"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "oats" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " oats"), xyoff = xyoff, optimum_threshold = 28)
  
  treatment = "FYM+NPK"
  ii <- which(sdata[,"Crop"] == "potato" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " potato"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "rye" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " rye"), xyoff = xyoff, optimum_threshold = 28)
  ii <- which(sdata[,"Crop"] == "oats" &  sdata[,"Description"] == treatment)
  plot_triangle(sdata[ii, ], defined_symbols = TRUE, show_legend = FALSE, main = paste0(treatment, " oats"), xyoff = xyoff, optimum_threshold = 28)

dev.off()
