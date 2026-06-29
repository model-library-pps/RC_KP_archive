require(reshape2)
source('RC_KP_model.r')
source('7. RC_KP Figures for Njoroge dataset.r')
source('8. RC_PK_Njoroge dataset.r')


#INPUT######################
#Read in pre-processed data on uptake, including only data from LR2016 and LR2018
UptData <- read.csv("..//Data//Processed//6. Njoroge dataset. Yield stover nutrient contents and uptake.csv")

#INPUT######################

#OUTPUT######################
outfile_details <- "..//Results//9. Njoroge measured vs predicted uptake of P and K details.pdf"
outfile <- "..//Results//9. Njoroge measured vs predicted uptake of P and K.pdf"
outfile_pools <- "..//Results//9. Njoroge estimated labile pools in y1 and y7.pdf"
outfile_farmerr <- "..//Results//9. Njoroge cumulative errors per farm.pdf"
outfile_farm_pools <- "..//Results//9. Njoroge pool dynamics per farm.pdf"


#NPK or PK treatment needs to be the first in the list!!!
#Initial P pools are estimated from differences in measured uptakes in NK and NPK treatments
#Initial K pools are estimated from differences in measured uptakes in NP and NPK treatments
#The first treatment needs to include both K and P applications!!
#This to ensure that initial pools are derived from treatment with PK fertilization
parListNames <- c("NPK"  ,  "PK",     "NK", "Control",  "NP")
cols         <- c("green", "blue","orange",     "red", "cyan")
pchs         <- c(      1,     1,        1,       1,        1)
lwds         <- c(      1,     2,        2,       1,        1)
ltys         <- c(      1,     1,        1,       1,        1)
Nrate.kg.ha  <- c(    150,     0,      150,       0,      150)
pPrate.kg.ha  <- c(    40,    40,        0,       0,       40)
Krate.kg.ha  <- c(     60,    60,       60,       0,        0)

ParSET <- "Njoroge"
LMERpredicted = TRUE #Use LM predicted uptakes or measured uptakes
#INPUT=============================================================================================
#Season 1 = end of LR2013, TIME = 0 TO 1
#Season 2 = end of SR2013, TIME = 1 TO 2
#Season 3 = end of LR2014, TIME = 2 TO 3
#Season 4 = end of SR2014, TIME = 3 TO 4
#Season 5 = end of LR2015, TIME = 4 TO 5
#Season 6 = end of SR2015, TIME = 5 TO 6
#Season 7 = end of LR2016, TIME = 6 TO 7
#Season 8 = end of SR2016, TIME = 7 TO 8 #First season of NOT-RESTAURATION
#Season 9 = end of LR2017, TIME = 8 TO 9 #First season of NOT-SUBPLOTS
#Season 10 = end of SR2017, TIME = 9 TO 10
#Season 11 = end of LR2018, TIME = 10 TO 11


if(LMERpredicted){
  UptDataNPK <- subset(UptData, select = c("FARMCODE","TRIALTYPE","TREATMENT","NEW.TREATMENT","SEASON",
                                           "LMER.N_Uptake.kg.ha", "LMER.P_Uptake.kg.ha", "LMER.K_Uptake.kg.ha")) 
}else{
  UptDataNPK <- subset(UptData, select = c("FARMCODE","TRIALTYPE","TREATMENT","NEW.TREATMENT","SEASON",
                                           "N_Uptake.kg.ha", "P_Uptake.kg.ha", "K_Uptake.kg.ha")) 
}
colnames(UptDataNPK)<- c("FARMCODE","TRIALTYPE","TREATMENT","NEW.TREATMENT","SEASON",
                         "N_Uptake.kg.ha", "P_Uptake.kg.ha", "K_Uptake.kg.ha")

#Add uptake to columens per treatment, combining TREATMENT and NEW.TREATMENT in column names 
UptN <- dcast(UptDataNPK, FARMCODE + TRIALTYPE + SEASON ~ TREATMENT + NEW.TREATMENT,value.var=c("N_Uptake.kg.ha"))
UptP <- dcast(UptDataNPK, FARMCODE + TRIALTYPE + SEASON ~ TREATMENT + NEW.TREATMENT,value.var=c("P_Uptake.kg.ha"))
UptK <- dcast(UptDataNPK, FARMCODE + TRIALTYPE + SEASON ~ TREATMENT + NEW.TREATMENT,value.var=c("K_Uptake.kg.ha"))
#select only farms with complete datasets 
ii<- which(UptK[,"SEASON"] == "LR2016" & UptK[,"NP_NONE"] != "NA" & UptK[,"NK_NONE"] != "NA" &
           UptP[,"SEASON"] == "LR2016" & UptP[,"NP_NONE"] != "NA" & UptP[,"NK_NONE"] != "NA")
jj<- which(UptK[,"SEASON"] == "LR2018" & UptP[,"SEASON"] == "LR2018")
UptN <- UptN[c(ii, jj),]
UptP <- UptP[c(ii, jj),]
UptK <- UptK[c(ii, jj),]

#To enable comparisons with other datasets
UptN[,"Crop"] <- "maize"
UptP[,"Crop"] <- "maize"
UptK[,"Crop"] <- "maize"

#Evaluate differences in Nrec between farms
UptN[,"Nrec"] <- (UptN[,"NPK_NONE"] - UptN[,"PK_NONE"]) / 150
#replace Nrec for farm F23 with missing N uptake estimate for PK treatment
ii <- which(is.na(UptN[,"Nrec"]))
UptN[ii,"Nrec"] <- mean(UptN[,"Nrec"], na.rm=TRUE)
#Set general model parameters=================================

parList={}
for(i in 1:length(parListNames)){
  Inputs_yield = data.frame(Crop = "maize",
                            Seasons=seq(0,25,1), #Start at 0 because harvest 1 is at the end of the season
                            Year=seq(0,25,1), 
                            Nfert = Nrate.kg.ha[i], 
                            Pfert = 0, 
                            Pfert_placed = pPrate.kg.ha[i], 
                            Kfert = Krate.kg.ha[i], 
                            FYMN = 0, FYMP = 0, FYMK = 0,
                            Nrec = 0.35, FYMNrec = 0, Nrec_FYM = 0,  #recovery is replaced by fitted values per farm
                            Nuptake_kgha = 25) #N uptakes are replaced by fitted values per farm
  if(parListNames[i] == "Control"){
    parList[[i]]  = RCKP_param(Scenario = parListNames[i],
                               Crops = "maize",
                               type = "TSP", Inputs_reference = Inputs_yield, 
                               CalSetName = ParSET,
                               UseMeasuredNUptake = USE_MEASURED_UPTAKES)
  }else{
    parList[[i]]  = RCKP_param(Scenario = parListNames[i],
                               Crops = "maize",
                               type = "TSP", Inputs_reference = Inputs_yield, 
                               CalSetName = ParSET,
                               UseMeasuredNUptake = USE_MEASURED_UPTAKES)
  }
}


#Call this function for all the computations!
out <- RC_KP_Njoroge_dataset(  parList = parList,
                               UptN = UptN,
                               UptP = UptP,
                               UptK = UptK,
                               UseMeasuredNUptake = USE_MEASURED_UPTAKES,
                               TIMESTEP_PRINT = 1)#Small values give good figures...  
print(c(RMSE_P_NK16 = out$RMSE_P_NK,
        RMSE_K_NP16 = out$RMSE_K_NP))
print(out$RMSE_2016)
print(out$NSE_2016)
print(out$RMSE_2018)
print(out$NSE_2018)
print(out$RMSE)
print(out$NSE)

#Save results in file
Write_Meas_Pred_NPK(ScriptNr = 9,
                    Dataset = "Siaya",
                    parListNames = parListNames,
                    UptN = out$UptN,
                    UptP = out$UptP,
                    UptK = out$UptK,
                    cols = cols,
                    pchs = pchs)


windows()
plot_results_trials_meas_vs_pred_Nuptake(UptN = out$UptN)
  #figFarmError(out)
#windows() 
  #plot_labile_pools_y1y7(DF_POOLS = out$DF_POOLS)
windows()
  plot_results_trials_SN_uptake_measurements(UptP = out$UptP, UptK = out$UptK)


#windows()
#  plot_results_trials_meas_vs_pred(UptP = out$UptP, UptK = out$UptK)

pdf(outfile_farmerr)
  figFarmError(out)
dev.off()


pdf(outfile_pools)
   plot_labile_pools_y1y7(DF_POOLS = out$DF_POOLS)
dev.off()

pdf(outfile)
   plot_results_trials_meas_vs_pred(UptP = out$UptP, UptK = out$UptK)
dev.off()
  
pdf(outfile_details)
  plot_results_trials_SN_uptake_measurements(UptP = out$UptP, UptK = out$UptK)
dev.off()


pdf(outfile_farm_pools)
  for(trt in unique(out$results_out[,"TREATMENT"])){
    for(trtN in unique(out$results_out[,"NEW.TREATMENT"])){
      compare_states_farms(TREATMENT= trt, NEW.TREATMENT = trtN, results_out = out$results_out)
    }
  }
dev.off()

#compare_states_farms(TREATMENT= "NPK", NEW.TREATMENT = "NPK", results_out = out$results_out)

