
dNS1 <- read.csv("..//Data//Processed//6a. Njoroge estimated yields and N uptake PHASE1.csv")
dNS2 <- read.csv("..//Data//Processed//6a. Njoroge estimated yields and N uptake PHASE2.csv")


#Workhorse function for the Njoroge et al. dataset
RC_KP_Njoroge_dataset <- function(parList = parList,
                                  UptN = UptN,
                                  UptP = UptP,
                                  UptK = UptK,
                                  IS.CALIBRATION = FALSE,
                                  UseMeasuredNUptake = TRUE,
                                  TIMESTEP_PRINT = 1.0){ #the time step afor saving state values
  
   
  DF_POOLS = NULL 
  parListNames = NULL
  for(i in 1:length(parList)){
    parListNames[i] <- parList[[i]]$Scenario
  }

  NSEASONS = 11
  # The P and K uptakes in the NK and NP treatments of LR2016, 
  # that are used to initialise the pools, needs to be backcasted for 7 seasons to find
  # the initial pools at the start of the experiment
  NSEASONS_SINCE_START = 7
  SEASON_SWITCH = 7 # new treatments from season 7 onwards for restoration trials
  SEASON_SWITCH_SUBPLOTS = 8 # new treatments from season 7 onwards for subplot trials
  
  #get values for look-up table to correct P and K pools for the given parameters
  #It is developed as a function of yield, but yield level proved not important.
  DF_corrections <- PoolRatio(Y0 = 2500, 
                              paramNP = parList[[which(parListNames == "NP")]], 
                              paramNK = parList[[which(parListNames == "NK")]], 
                              times = seq(1,NSEASONS_SINCE_START,1))

  #Ensure to only include farms with uptake data from the LR2016 to initialise the states
  i2016 <- which(UptP[,"SEASON"] == "LR2016")
  uFARMCODE <- unique(UptP[i2016,"FARMCODE"])
  #Run through all farm codes
  results_out <- NULL
  for(uFC in uFARMCODE){
    #print(uFC)
    iRow2016 <- which(UptP[,"FARMCODE"] == uFC & UptP[,"SEASON"] == "LR2016")
    iRow2018 <- which(UptP[,"FARMCODE"] == uFC & UptP[,"SEASON"] == "LR2018")

    farmcode <- UptP[iRow2016,"FARMCODE"]
    trialtype <- UptP[iRow2016,"TRIALTYPE"]
    Farminphase2 = 0
    
    SWITCH <- NSEASONS
    if(trialtype == "NOT-RESTAURATION"){#RESTAURATION TRIAL
      SWITCH <- SEASON_SWITCH
      NewTreatmentList <- "NPK"
    }else if(trialtype == "NOT-SUBPLOTS"){#SUBPLOT TRIAL
      SWITCH <- SEASON_SWITCH_SUBPLOTS
      NewTreatmentList <- c("PK","NK","NP","NPK")
    }
    
    #Add LM predicted PK or control yields for this farm to estimate N supply from soil
    #Convert to DM yields...
    dns <- subset(dNS1, FARMCODE == uFC & SEASON_NR <= SWITCH)
    for(i in 1:length(parListNames)){
      ii <- which(parList[[i]]$Inputs_reference.Year %in% dns[,"SEASON_NR"])
      colname <- paste0(parList[[i]]$Scenario,"_NONE")
      
      parList[[i]]$Inputs_reference.Yield_kgha[ii] = parList[[i]]$CropQUEFTS_Dry.DMc * pmax(100, 1000 * dns[,"PK_Y_FM"])
      parList[[i]]$Inputs_reference.Nrec[ii] <- dns[,"Nrec"]
      parList[[i]]$Inputs_reference.Nuptake_kgha[ii] = dns[,"PK_NS"]
      
      if(!UseMeasuredNUptake & parListNames[i] == "Control"){ #Always soil supply from control yields
        parList[[i]]$Inputs_reference.Yield_kgha[ii] = parList[[i]]$CropQUEFTS_Dry.DMc * pmax(100, 1000 * dns[,"Control_Y_FM"])
        parList[[i]]$Inputs_reference.Nrec[ii] <- 0
        parList[[i]]$Inputs_reference.Nuptake_kgha[ii] = dns[,"Control_NS"]
      }else if(UseMeasuredNUptake & parListNames[i] == "Control"){ #Always soil supply estimated from yield
        parList[[i]]$Inputs_reference.Nrec[ii] <- 0 
        parList[[i]]$Inputs_reference.Nuptake_kgha[ii]  <- rep(UptN[iRow2016,"Control_NONE"],length(ii))
      }else if(UseMeasuredNUptake & !is.na(UptN[iRow2016, colname])){
        #Set all values to 2016 measured uptake
        iSeas <- which(parList[[i]]$Inputs_reference.Year == NSEASONS_SINCE_START - 1) #Uptake set to year 6
        parList[[i]]$Inputs_reference.Nrec[iSeas] <- 0
        parList[[i]]$Inputs_reference.Nuptake_kgha[iSeas]  <- UptN[iRow2016, colname]
      }
    }
    
    param_PK = parList[[which(parListNames == "PK")]]
    param_NK = parList[[which(parListNames == "NK")]]
    param_NP = parList[[which(parListNames == "NP")]]
    
    #Get parameters to initialise soil pools for LR2016 from experimental data
    #US is unfertilized soil & FS is fertilized soil
    #Numbers in comments indicate equation line in Table 3 of Wolf et al 1987
    Kupt_US <- UptK[iRow2016, "NP_NONE"] #K uptake from unfertilized soil #3 (uptake in NP treatment)
    Pupt_US <- UptP[iRow2016, "NK_NONE"] #P uptake from unfertilized soil #3 (uptake in NK treatment)

    ini <- RCKP_estimate_initial_pools(param = param_PK,   #All parameters
                                       Crop = "maize",     #For extraction of crop=specific parameters
                                       Kupt_US = Kupt_US,  #K uptake from unfertilized soil #3 (uptake in NP treatment)
                                       Pupt_US = Pupt_US)  #P uptake from unfertilized soil #3 (uptake in NK treatment)
    
    #Backcast to find the initial conditions of the field.
    states_iniK <- ode(ini,seq(NSEASONS_SINCE_START, 0, by = -0.01  ), RCKP_fertilizer, param_NP,  #method = "ode45",
                       events = PrepareEventList(param = param_NP,
                                                 fyear_fert = 0.1, #At start of season 
                                                 fyear_FYM  = 0.1))
    states_iniP <- ode(ini,seq(NSEASONS_SINCE_START, 0, by = -0.01  ), RCKP_fertilizer, param_NK,  #method = "ode45",
                       events = PrepareEventList(param = param_NK,
                                                 fyear_fert = 0.1, #At start of season 
                                                 fyear_FYM  = 0.1))

    #Check if pools 
    nr <- nrow(states_iniK)
    df <- data.frame(FARMCODE = c(uFC, uFC), SEASON = c(1, NSEASONS_SINCE_START))
    
    df_pools <- cbind(df, cbind(states_iniK[c(nr, 1),c("K_labile","K_stable")],states_iniP[c(nr, 1),c("P_labile","P_stable")]))
    DF_POOLS <- rbind(DF_POOLS, df_pools)
    
    #Get the estimated pool values for the first year, on the last row of the resulting states
    #Correct these estimates using known error when assuming balance after n seasons
    iniPK <- RCKP_ini(P_labile = as.numeric(states_iniP[nrow(states_iniP),"P_labile"]) / DF_corrections[1,"P_labile"],
                      P_stable = as.numeric(states_iniP[nrow(states_iniP),"P_stable"]) / DF_corrections[1,"P_stable"],
                      P_labile_placed = 0,#must be 0!
                      K_labile = as.numeric(states_iniK[nrow(states_iniK),"K_labile"]) / DF_corrections[1,"K_labile"],
                      K_stable = as.numeric(states_iniK[nrow(states_iniK),"K_stable"]) / DF_corrections[1,"K_stable"])

    for(i in 1:length(parList)){
      #####ONLY PHASE 1#############################
      if(SWITCH == NSEASONS) {
        states <- ode(iniPK,seq(0, NSEASONS_SINCE_START, by = TIMESTEP_PRINT), 
                      RCKP_fertilizer, parList[[i]],  #method = "ode45",
                      events = PrepareEventList(param = parList[[i]],
                                                fyear_fert = 0.1, #At start of season 
                                                fyear_FYM  = 0.1))
        #Get result data frames
        #Add predictions for LR2016, the 7th season with uptake up to the 7th season  
        if(length(iRow2016) > 0){
          ii <- which(states[,"time"] == NSEASONS_SINCE_START)
          UptN[iRow2016,paste0("pred_N_UP_", parList[[i]]$Scenario)] <- states[ii[1],"N_up"]
          UptP[iRow2016,paste0("pred_P_UP_", parList[[i]]$Scenario)] <- states[ii[1],"P_up"]
          UptK[iRow2016,paste0("pred_K_UP_", parList[[i]]$Scenario)] <- states[ii[1],"K_up"]
        }
        if(IS.CALIBRATION == FALSE){
          states <- as.data.frame(states)
          results <- cbind(data.frame(FARMCODE = rep(uFC,nrow(states)),
                                      TREATMENT = rep(parList[[i]]$Scenario,nrow(states)),
                                      NEW.TREATMENT = rep("NONE",nrow(states))), 
                           states)
          results_out <- rbind(results_out, results)
        }
      }else if(SWITCH < NSEASONS) {
        #####PHASE 1 + 2#############################
        # #Get initial amounts of pools at start of phase 2
        # states <- as.data.frame(states)
        # iState <- nrow(states)
        # iniPhase2 <- RCKP_ini(P_labile = states[iState,"P_labile"], 
        #                       P_stable = states[iState,"P_stable"],
        #                       P_labile_placed = states[iState,"P_labile_placed"],
        #                       K_labile = states[iState,"K_labile"], 
        #                       K_stable = states[iState,"K_stable"])
        # 
          #Make combinations for superimposed treatments
          for(j in 1:length(parList)){
            iScen <- which(parList[[j]]$Scenario == NewTreatmentList)
            if(length(iScen) > 0){
              param <- parList[[i]]
              #Combine fertilizations from phase 1 and phase 2
              
              #farm-specific values for PK yield in subplots 
              dns <- subset(dNS2, FARMCODE == uFC & TRT == parList[[i]]$Scenario)
              ii <- which(param$Inputs_reference.Year %in% dns[,"SEASON_NR"])
              param$Inputs_reference.Nrec[ii] <- dns[,"Nrec"]
              param$Inputs_reference.Yield_kgha[ii] = (param$CropQUEFTS_Dry.DMc * 
                                                               pmax(100, 1000 * dns[,"PK_Y_FM"]))
              param$Inputs_reference.Nuptake_kgha[ii] = dns[,"PK_NS"]
              
              #Add fertilizer inputs for Phase2
              param$Inputs_reference.Nfert[ii] = parList[[j]]$Inputs_reference.Nfert[ii]
              param$Inputs_reference.Pfert[ii] = parList[[j]]$Inputs_reference.Pfert[ii]
              param$Inputs_reference.Pfert_placed[ii] = parList[[j]]$Inputs_reference.Pfert_placed[ii]
              param$Inputs_reference.Kfert[ii] = parList[[j]]$Inputs_reference.Kfert[ii]
              
              colname <- paste0(parList[[i]]$Scenario,"_",parList[[j]]$Scenario)
              if(!is.na(UptN[iRow2018,colname]) & UseMeasuredNUptake){
                #Set all values after switch year to 2018 measured uptake
                iSeas <- which(param$Inputs_reference.Year == NSEASONS - 1) #Uptake set to year 10 results in N uptake accumulating to year 11
                param$Inputs_reference.Nrec[iSeas] <- 0
                param$Inputs_reference.Nuptake_kgha[iSeas]  <- UptN[iRow2018,colname]
              }

              states_Phase2  <- ode(iniPK, seq(0, NSEASONS, by = TIMESTEP_PRINT), 
                                  RCKP_fertilizer, param,  #method = "ode45",
                                  events = PrepareEventList(param = param,
                                                   fyear_fert = 0.1, #At start of season 
                                                   fyear_FYM  = 0.1))
              colname_REF <- paste0(parList[[i]]$Scenario, "_", parList[[j]]$Scenario)
              if(length(iRow2018) > 0){
                #Add predictions for LR2018
                ii <- which(states_Phase2[,"time"] == NSEASONS)
                UptN[iRow2018,paste0("pred_N_UP_", colname_REF)] <- states_Phase2[ii[1],"N_up"]
                UptP[iRow2018,paste0("pred_P_UP_", colname_REF)] <- states_Phase2[ii[1],"P_up"]
                UptK[iRow2018,paste0("pred_K_UP_", colname_REF)] <- states_Phase2[ii[1],"K_up"]
              }
              #Get result data frames
              if(IS.CALIBRATION == FALSE){
                states_Phase2 <- as.data.frame(states_Phase2)
                results_Phase2 <- cbind(data.frame(FARMCODE = rep(uFC,nrow(states_Phase2)),
                                                  TREATMENT = rep(parList[[i]]$Scenario,nrow(states_Phase2)),
                                                  NEW.TREATMENT = rep(parList[[j]]$Scenario,nrow(states_Phase2)))
                                       ,states_Phase2)
               
                results_out <- rbind(results_out, results_Phase2)
              }
            }
          }
      }
    }
  }

  MVP16 = NULL
  MVK16 = NULL
  MVPK16 = NULL
  MVPK18 = NULL
  for( i in 1:length(parList)){
    mvpk = data.frame(Phase1 = parList[[i]]$Scenario,
                      Phase2 = "NONE",
                      measured_N_up =  UptN[,paste0(parList[[i]]$Scenario, "_NONE")], 
                      predicted_N_up = UptN[,paste0("pred_N_UP_", parList[[i]]$Scenario)],
                      measured_P_up =  UptP[,paste0(parList[[i]]$Scenario, "_NONE")],
                      predicted_P_up = UptP[,paste0("pred_P_UP_", parList[[i]]$Scenario)],
                      measured_K_up =  UptK[,paste0(parList[[i]]$Scenario, "_NONE")], 
                      predicted_K_up = UptK[,paste0("pred_K_UP_", parList[[i]]$Scenario)])
    if(parList[[i]]$Scenario == "NK"){#This treatment is used to intialise P: do not include!!
      MVP16 = rbind(MVP16, mvpk)
      mvpk[,"measured_N_up"] = NA
      mvpk[,"measured_P_up"] = NA
      mvpk[,"predicted_P_up"] = NA
    }else if(parList[[i]]$Scenario == "NP"){#This treatment is used to initialise K:  do not include!!
      MVK16=rbind(MVK16, mvpk)
      mvpk[,"measured_N_up"] = NA
      mvpk[,"measured_K_up"] = NA
      mvpk[,"predicted_K_up"] = NA
    }
    MVPK16 = rbind(MVPK16, mvpk)
    
    for( j in 1:length(parList)){
       if(parList[[j]]$Scenario != "Control"){
          colname_REF <- paste0(parList[[i]]$Scenario, "_", parList[[j]]$Scenario)
          mvpk = data.frame(Phase1 = parList[[i]]$Scenario,
                            Phase2 = parList[[j]]$Scenario,
                            measured_N_up = UptN[,colname_REF],
                            predicted_N_up = UptN[,paste0("pred_N_UP_", colname_REF)],
                            measured_P_up = UptP[,colname_REF],
                            predicted_P_up = UptP[,paste0("pred_P_UP_", colname_REF)],
                            measured_K_up =  UptK[,colname_REF], 
                           predicted_K_up = UptK[,paste0("pred_K_UP_", colname_REF)])
          MVPK18 = rbind(MVPK18, mvpk)
      }
    }
  }

  N_NSE16 <- NSE(model_up = MVPK16[,"predicted_N_up"], meas_up = MVPK16[,"measured_N_up"])
  P_NSE16 <- NSE(model_up = MVPK16[,"predicted_P_up"], meas_up = MVPK16[,"measured_P_up"])
  K_NSE16 <- NSE(model_up = MVPK16[,"predicted_K_up"], meas_up = MVPK16[,"measured_K_up"])

  N_NSE18 <- NSE(model_up = MVPK18[,"predicted_N_up"], meas_up = MVPK18[,"measured_N_up"])
  P_NSE18 <- NSE(model_up = MVPK18[,"predicted_P_up"], meas_up = MVPK18[,"measured_P_up"])
  K_NSE18 <- NSE(model_up = MVPK18[,"predicted_K_up"], meas_up = MVPK18[,"measured_K_up"])

  N_NSE <- NSE(model_up = c(MVPK16[,"predicted_N_up"],MVPK18[,"predicted_N_up"]),
               meas_up = c(MVPK16[,"measured_N_up"],MVPK18[,"measured_N_up"]))
  P_NSE <- NSE(model_up = c(MVPK16[,"predicted_P_up"],MVPK18[,"predicted_P_up"]),
               meas_up = c(MVPK16[,"measured_P_up"],MVPK18[,"measured_P_up"]))
  K_NSE <- NSE(model_up = c(MVPK16[,"predicted_K_up"],MVPK18[,"predicted_K_up"]),
               meas_up = c(MVPK16[,"measured_K_up"],MVPK18[,"measured_K_up"]))
  
  return(list(RMSE_P_NK = RMSE(model_up = MVP16[,"predicted_P_up"], meas_up = MVP16[,"measured_P_up"]), 
              RMSE_K_NP = RMSE(model_up = MVK16[,"predicted_K_up"], meas_up = MVK16[,"measured_K_up"]), 
              RMSE_2016 = c(RMSE_N_2016 = RMSE(model_up = MVPK16[,"predicted_N_up"], meas_up = MVPK16[,"measured_N_up"]),
                            RMSE_P_2016 = RMSE(model_up = MVPK16[,"predicted_P_up"], meas_up = MVPK16[,"measured_P_up"]), 
                            RMSE_K_2016 = RMSE(model_up = MVPK16[,"predicted_K_up"], meas_up = MVPK16[,"measured_K_up"])), 
              RMSE_2018 = c(RMSE_N_2018 = RMSE(model_up = MVPK18[,"predicted_N_up"], meas_up = MVPK18[,"measured_N_up"]), 
                            RMSE_P_2018 = RMSE(model_up = MVPK18[,"predicted_P_up"], meas_up = MVPK18[,"measured_P_up"]), 
                            RMSE_K_2018 = RMSE(model_up = MVPK18[,"predicted_K_up"], meas_up = MVPK18[,"measured_K_up"])), 
              RMSE = c(RMSE_N = RMSE(model_up = c(MVPK16[,"predicted_N_up"],MVPK18[,"predicted_N_up"]), 
                                          meas_up  = c(MVPK16[,"measured_N_up"], MVPK18[,"measured_N_up"])), 
                            RMSE_P = RMSE(model_up = c(MVPK16[,"predicted_P_up"],MVPK18[,"predicted_P_up"]), 
                                               meas_up  = c(MVPK16[,"measured_P_up"], MVPK18[,"measured_P_up"])), 
                            RMSE_K = RMSE(model_up = c(MVPK16[,"predicted_K_up"],MVPK18[,"predicted_K_up"]), 
                                               meas_up  = c(MVPK16[,"measured_K_up"], MVPK18[,"measured_K_up"]))), 
              NSE_2016=c(NUP_NSE16 = N_NSE16, P_NSE16 =P_NSE16, K_NSE16 =K_NSE16),
              NSE_2018=c(NUP_NSE18 = N_NSE18, P_NSE18 =P_NSE18, K_NSE18 =K_NSE18),
              NSE = c(NUP_NSE = N_NSE, P_NSE =P_NSE, K_NSE = K_NSE),
              UptN = UptN, 
              UptP = UptP, 
              UptK = UptK,
              MVPK16 = MVPK16,
              MVPK18 = MVPK18,
              DF_POOLS = DF_POOLS,
              results_out = results_out
              ))  
}

