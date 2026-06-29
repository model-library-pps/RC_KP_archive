#Calibration for the Hanninghof dataset
#
#Authors: AGT Schut and W. Reymann
#         Plant Production Systems group 
#         Wageningen University, 2021
require('deSolve')        
require('Rfast')
source('4. RC_KP_Hanninghof_run.R') #this loads required observations and settings


#Function needed for calibration. Input parameters are log-transformed!
RCKP_model_fun =  function(parOpt=parOpt,
                         parOptNames = parOptNames,
                         parList = parList,
                         Pupt_US = Pupt_US,
                         Kupt_US = Kupt_US,
                         UptN = UptN,
                         UptP = UptP,
                         UptK = UptK)  {
  
  # back-transform the input parameters
  parOpt = exp(parOpt) 
  
  print(parOptNames)
  print(parOpt)

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
  listStates <- RCKP_rotation_Hanninghof(parList = parList,
                                        Pupt_US = Pupt_US,
                                        Kupt_US = Kupt_US,
                                        YEARS = TESTYEARS)
  #Determine errors
  out <- Determine_PK_errors(parList = parList,
                             listStates = listStates,
                             UptN = UptN,
                             UptP = UptP,
                             UptK = UptK)    
  
  
  print(c(RMSE_N = out$RMSE_N, 
          RMSE_P = out$RMSE_P, 
          RMSE_K = out$RMSE_K))
  
  # Return the total residual sum of squares
  # Both errors have about equal weights!
  return(out$RMSE_N +  4 * out$RMSE_P + out$RMSE_K)
}

#CALIBRATION============================================================
CROPS <- c("potato","oats","rye")

for(ParSET in c("LMpred_FYM_Hanninghof")){#Options: "original_Hanninghof"   "LMpred_Hanninghof" "LMpred_FYM_Hanninghof"
  
  # if(ParSET == "LMpred_Hanninghof"){
  #   parListNames <- c("NPK"  ,  "PK",     "NK", "Control",    "N",   "NP",  "NPKMg")
  #   #define parameters to calibrate
  #   parOptNames <- c("r_labile_P","r_labile_K","CropQUEFTS_Dry.rur_P","CropQUEFTS_Dry.rur_K")
  # }else if(ParSET == "LMpred_FYM_Hanninghof"){
  #   parListNames <- c("FYMNPK",   "FYM",   "FYMPK",     "FYMNK",       "FYMNP",    "FYMNPKMg")
  #   #define parameters to calibrate
  #   #parOptNames <- c("r_labile_P_FYM", "r_labile_K_FYM", "CropQUEFTS_Dry.rur_P_FYM", "CropQUEFTS_Dry.rur_K_FYM")
  #   parOptNames <- c("r_labile_P_FYM", "r_labile_K_FYM")
  # }
  parOptNames <- c("r_labile_P","r_labile_K","r_labile_P_FYM", "r_labile_K_FYM","CropQUEFTS_Dry.rur_P","CropQUEFTS_Dry.rur_K")
  
  #Prepare list with parameter sets
  parList <- PrepareListParameterSets(parListNames = parListNames, CROPS = CROPS, 
                                      ParSET= ParSET, Trt_PK = "PK",
                                      Ydata = Ydata,
                                      UseMeasuredUptake = TRUE)
  
  #Get initial values for parOpt and log transform them to ensure they do not become negative
  parOpt=NULL
  for(i in 1:length(parOptNames)){
    parOpt = c(parOpt, log(parList[[1]][[parOptNames[i]]] ) )
  }
  
  #CALIBRATION
  print(" ")
  print(ParSET)
  print(" ")
  print("=========CALIBRATION==============")
  #Find optimum values, note that these are returned on the log scale
  fitOpt <- optim(par = parOpt,       #parameters to optimize on log scale
                  fn = RCKP_model_fun,  #the function to do the calculations
                  parOptNames = parOptNames,
                  parList = parList,
                  Pupt_US = Pupt_US,
                  Kupt_US = Kupt_US,
                  UptN = UptN,
                  UptP = UptP,
                  UptK = UptK)
  
  parN <- NULL
  for(pn in parOptNames){
    if(length(parList[[1]][[pn]]) > 1){
      pn = paste0(pn, "-", parList[[1]]$CropQUEFTS_Dry.crop)
    }
    parN <- c(parN, pn)
  }
  
  #Show best fitting values
  fitOpt = data.frame(initVal = exp(parOpt),
                      optValues = exp(fitOpt$par),
                      parName = parN)
  
  
  print(fitOpt)
  
  write.csv(fitOpt, paste("..//Results//5. Calibrated values for parameter set ",ParSET, ".csv"))
}

