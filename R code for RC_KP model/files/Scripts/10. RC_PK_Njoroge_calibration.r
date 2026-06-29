#Calibration for the Hanninghof dataset
#
#Authors: AGT Schut and W. Reymann
#         Plant Production Systems group 
#         Wageningen University, 2021


require('deSolve')        
require('Rfast')
#source('9. RC_PK_Njoroge dataset_run.r') #this loads required parameter sets

#OUPUT=============================================================================================
OutFile = "..//Results//10. Njoroge dataset calibrated parameters.csv"


#Function needed for calibration. Input parameters are log-transformed!
RCKP_model_fun =  function(parOpt=parOpt,
                         parOptNames = parOptNames,
                         parList = parList,
                         UptN = UptN,
                         UptP = UptP,
                         UptK = UptK)  {
  
  # back-transform the input parameters
  parOpt = exp(parOpt) 

  #ensure that parameters to calibrate are adapted
  nr=0
  for(i in 1:length(parOptNames)){
    nv = (nr+1):(nr + length(parList[[1]][[parOptNames[i]]]) )
    for(j in 1:length(parList)){
      parList[[j]][[parOptNames[i]]] = parOpt[nv]
    }
    nr=max(nv)
  }  
  
  #Call this function for all the computations!
  out <- RC_KP_Njoroge_dataset(  parList = parList,
                                 UptN = UptN,
                                 UptP = UptP,
                                 UptK = UptK,
                                 IS.CALIBRATION = TRUE,
                                 UseMeasuredNUptake = TRUE)  
  #weigh the errors.
  werr = unname( 10 * out$RMSE_P_NK + 2.5 * out$RMSE_K_NP 
           + out$RMSE_2016["RMSE_N_2016"] + 4 * out$RMSE_2016["RMSE_P_2016"] + out$RMSE_2016["RMSE_K_2016"]
           + out$RMSE_2018["RMSE_N_2018"] + 4 * out$RMSE_2018["RMSE_P_2018"] + out$RMSE_2018["RMSE_K_2018"])
  
  print("Intermediate result below:")
  print(c(werr = werr,
          RMSE_P_NK16 = out$RMSE_P_NK,
          RMSE_K_NP16 = out$RMSE_K_NP))
  print(out$RMSE)
  print(out$NSE)
  print(parOpt)
  
  return( werr )
}

#CALIBRATION============================================================

#define parameters to calibrate
if(ParSET == "Njoroge"){
  parOptNames <- c("r_labile_P", "r_labile_K", "CropQUEFTS_Dry.rur_P_placed", 
                   "CropQUEFTS_Dry.rur_P", "CropQUEFTS_Dry.rur_K")
}


#Get initial values for parOpt and log transform them to ensure they do not become negative
parOpt = NULL
for(i in 1:length(parOptNames)){
  parOpt = c(parOpt, log(parList[[1]][[parOptNames[i]]] ) )
}

#CALIBRATION
print(" ")
print(" ")
print(" ")
print("=========CALIBRATION==============")
#Find optimum values, note that these are returned on the log scale
fitOpt <- optim(par = parOpt,       #parameters to optimize on log scale
                fn = RCKP_model_fun,  #the function to do the calculations
                parOptNames = parOptNames,
                parList = parList,
                UptN = UptN,
                UptP = UptP,
                UptK = UptK)


parN <- NULL
for(pn in parOptNames){
  if(!is.null(strfind(pn,"CropQUEFTS_Dry"))  ){
    pn = paste0(pn, "-", parList[[1]]$CropQUEFTS_Dry.crop)
  }
  parN <- c(parN, pn)
}
#Show best fitting values
fitOpt = data.frame(parName = parN,
                    initVal = exp(parOpt),
                    optValues = exp(fitOpt$par))

write.csv(fitOpt, OutFile)

print(fitOpt)


