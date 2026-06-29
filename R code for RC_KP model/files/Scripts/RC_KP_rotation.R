Get_inputs <- function(df, TESTYEARS=TESTYEARS, dfNUP, IsTreatMeasuredUptake = FALSE){
  Nrec = 0
  Nrec_FYM = 0
  FYMNrec = 0
  Nuptake_kgha = NA

  if(is.data.frame(dfNUP)){
    #Filter out rows for years without uptake measured
    ii <- which(!is.na(dfNUP[,"N.offtake.kg.ha"]))
    df <- df[ii,]
    dfNUP <- dfNUP[ii,]
    Nuptake_kgha = dfNUP[,"N.offtake.kg.ha"]  
  }
  #The Nuptake include fertilizer effects when using measured treatment data,
  #If not, then SN is based on uptake in pk or control treatments only.
  if(!IsTreatMeasuredUptake){
    Nrec = df[,"rec_N"]
    Nrec_FYM = df[,"rec_N_FYM"] #Rec. from NPK: diff between FYM and FYMNPK
    FYMNrec = df[,"rec_FYMN"]
  }
    
  Inputs_yield = data.frame(Crop = as.character(df[,"Crop"]),
                            Year = df[,"Year"],
                            Nfert = df[,"N.rate.kg.ha"], 
                            Pfert = df[,"P.rate.kg.ha"], 
                            Pfert_placed = 0, 
                            Kfert = df[,"K.rate.kg.ha"],
                            FYMN = df[,"FYMN.rate.kg.ha"], 
                            FYMP = df[,"FYMP.rate.kg.ha"], 
                            FYMK = df[,"FYMK.rate.kg.ha"],
                            Nrec = Nrec,
                            Nrec_FYM = Nrec_FYM,
                            FYMNrec = FYMNrec,
                            Nuptake_kgha = Nuptake_kgha)
  return(Inputs_yield)
}

Get_inputsLM <- function(df, TESTYEARS=TESTYEARS, dfNUP, IsTreatMeasuredUptake = FALSE){
  Nrec = 0
  Nrec_FYM = 0
  FYMNrec = 0
  Nuptake_kgha = NA
  
  if(is.data.frame(dfNUP)){
    Nuptake_kgha = dfNUP[,"LMpred.N.offtake.kg.ha"]  
  }
  #The Nuptake include fertilizer effects when using measured treatment data,
  #If not, then SN is based on uptake in pk or control treatments only.
  if(!IsTreatMeasuredUptake){
    Nrec = df[,"LMpred.rec_N"]
    Nrec_FYM = df[,"LMpred.rec_N_FYM"] #Rec. from NPK: diff between FYM and FYMNPK
    FYMNrec = df[,"LMpred.rec_FYMN"]
  }
  
  Inputs_yield = data.frame(Crop = as.character(df[,"Crop"]),
                            Year = df[,"Year"],
                            Nfert = df[,"N.rate.kg.ha"], 
                            Pfert = df[,"P.rate.kg.ha"], 
                            Pfert_placed = 0, 
                            Kfert = df[,"K.rate.kg.ha"],
                            FYMN = df[,"FYMN.rate.kg.ha"], 
                            FYMP = df[,"FYMP.rate.kg.ha"], 
                            FYMK = df[,"FYMK.rate.kg.ha"],
                            Nrec = Nrec,
                            Nrec_FYM = Nrec_FYM,
                            FYMNrec = FYMNrec,
                            Nuptake_kgha = Nuptake_kgha)
  return(Inputs_yield)
}

PrepareListParameterSets <- function(parListNames, 
                                     CROPS = CROPS, 
                                     ParSET= ParSET, 
                                     Ydata = Ydata, 
                                     Trt_PK = "PKMg",
                                     UseMeasuredUptake){
  
  #Get average yield for Control plot to assess soil N supply
  d_Control <- subset(Ydata, Year >= min(TESTYEARS) & Year <= max(TESTYEARS) & Trt == "Control")
  #fit <- lm(Yield.kg.ha ~ 1 + Year * Crop, d_Control)
  #sfit <- summary(fit)
  #Get average yield for all other plot to assess soil N supply
  d_PK <- subset(Ydata, Year >= min(TESTYEARS) & Year <= max(TESTYEARS) & Trt == Trt_PK)
  #Fit a linear regression to the measured yield data
  #fit_PK <- lm(Yield.kg.ha ~ 1 + Year + Crop, d_PK) #interaction crop:year not significant
  #sfit <- summary(fit_PK)
  
  parList = {}
  for (i in 1:length(parListNames)) {
    d_TRT<-subset(Ydata, Year >= min(TESTYEARS) 
                  & Year <= max(TESTYEARS) 
                  & Trt == parListNames[i])
    if (!is.null(strfind(ParSET,"original")) & parListNames[i] == "Control") {
      Inputs_yield <- Get_inputs(df = d_Control,
                                 TESTYEARS = TESTYEARS, 
                                 dfNUP = d_Control,
                                 IsTreatMeasuredUptake = UseMeasuredUptake)
    }else if (!is.null(strfind(ParSET,"original"))) {
      #TRT with measured N uptake in PK treatment for soil N supply
      if(UseMeasuredUptake){
        Inputs_yield <- Get_inputs(df = d_TRT, 
                                   TESTYEARS = TESTYEARS, 
                                   dfNUP = d_TRT,
                                   IsTreatMeasuredUptake = UseMeasuredUptake) #estimate yields from PK treatment plots
      }else{
        #N recovery for FYM from FYM-PK
        #N recovery from fertilizer from FYMNPK-FYM
        Inputs_yield <- Get_inputs(df = d_TRT, 
                                   TESTYEARS = TESTYEARS, 
                                   dfNUP = d_PK,
                                   IsTreatMeasuredUptake = UseMeasuredUptake) #estimate yields from PK treatment plots
      }
    }else {
      #TRT with LMpredicted N uptake for soil N supply
      if(UseMeasuredUptake){
        Inputs_yield <- Get_inputsLM(df = d_TRT, 
                                     TESTYEARS = TESTYEARS, 
                                     dfNUP = d_TRT,
                                     IsTreatMeasuredUptake = UseMeasuredUptake) #estimate yields from PK treatment plots
      }else{
        #LMpredicted N uptake from PK treatment for soil N supply
        #LM predicted values for recovery: 
        #    N recovery based on NPK-PKMg difference;
        #    FYMN recovery based on PK-FYM difference;
        #    FYMNPK-NPKMg for N recovery for N from fertilizer in treatments including FYM
        Inputs_yield <- Get_inputsLM(df = d_TRT, 
                                     TESTYEARS = TESTYEARS, 
                                     dfNUP = d_PK,
                                     IsTreatMeasuredUptake = UseMeasuredUptake) #estimate yields from PK treatment plots
      }
    }
    parList[[i]]  = RCKP_param(Scenario = parListNames[i],type = "TSP",
                               Crops = CROPS,
                               Inputs_reference = Inputs_yield, 
                               CalSetName = ParSET,
                               UseMeasuredNUptake = UseMeasuredUptake)
  }
  return(parList)
}



#Workhorse function for datasets with a rotation (Broadbalk)
RCKP_rotation_Broadbalk <- function(parList = parList,
                                    Pupt_US = Pupt_US, #P uptake in NP treatment in 1968 for wheat
                                    Kupt_US = Kupt_US, #K uptake in NP treatment in 1968 for wheat
                                    YEARS = TESTYEARS){
  iniPK <- RCKP_estimate_initial_pools(param = parList[[1]],            #All parameters
                                       Crop = "wheat",           #For extraction of crop=specific parameters
                                       Kupt_US = Kupt_US,  #K uptake from unfertilized soil #3 (uptake in NP treatment)
                                       Pupt_US = Pupt_US)
  
  #Experiment started in 1852. But the wheat experiment with fertilizers changed in 1926. Backcast for initial conditions.
  #Yield and offtakes are pretty noise / uncertain for earlier years. See Fishr dataset (.xls)
  #Should run forward. It determines correction factors after n seasons of unbalanced fertilization
  FIRSTYEAR <- 1926
  iniPK_BC <- Backcast_IniPK_Broadbalk(iniPK = iniPK, times = seq(FIRSTYEAR, min(YEARS),1))

  listStates = {}
  for (i in 1:length(parList)) {
    iniPK <- Forecast_IniPK_Broadbalk(iniPK = iniPK_BC, 
                                      times = seq(FIRSTYEAR, min(YEARS), 1), 
                                      param = parList[[i]])
    state <- ode(iniPK, times = YEARS,
                 RCKP_fertilizer, parList[[i]],  #method = "ode45",
                 events = PrepareEventList(param = parList[[i]]) )
    # state <- ode(iniPK, times = seq(min(YEARS), max(YEARS), 0.1),
    #              RCKP_fertilizer, parList[[i]],  #method = "ode45",
    #              events = list(func = rootFertilizerApplication, root = TRUE),
    #              root = rootTrigger)
    listStates[[i]] <- as.data.frame(state) 
  }
  return(listStates)
}

RCKP_rotation_Hanninghof <- function(parList = parList,
                                     Pupt_US = Pupt_US, #P uptake in NP treatment in first year
                                     Kupt_US = Kupt_US, #K uptake in NP treatment in first year
                                     YEARS = TESTYEARS){
  #Estimate P and K pools in the first year
  iniPK <- RCKP_estimate_initial_pools(param = parList[[1]],            #All parameters
                                       Crop = parList[[1]]$Inputs_reference.Crop[1],    #For extraction of crop=specific parameters
                                       Kupt_US = Kupt_US,   #K uptake from unfertilized soil, uptake in NP treatment
                                       Pupt_US = Pupt_US)   #P uptake from unfertilized soil, uptake in NK treatment
  
  listStates = {}
  for (i in 1:length(parList)) {
    #The experiment was already running for a few years. Run two rotations to adjust estimates.
    iniTRT <- AdjustIniPK_Hanninghof(iniPK = iniPK, times = YEARS[1]:(YEARS[1] + 5), param = parList[[i]])
    
    state <- ode(iniTRT, YEARS, RCKP_fertilizer, parList[[i]],  #method = "ode45",
                 events = PrepareEventList(param = parList[[i]],  
                                           fyear_fert = 74/365, #Spring application, mid March
                                           fyear_FYM  = 60/365))
    listStates[[i]] <- as.data.frame(state) 
  }
  return(listStates)
}

Determine_PK_errors <- function(parList = parList,
                                listStates = listStates,
                                UptN = UptN,
                                UptP = UptP,
                                UptK = UptK,
                                ShowErrors = FALSE) {
  N_error = NULL
  for (YEAR in UptN[,"Year"]) {
    #Add predictions  
    ii <- which(UptN[,"Year"] == YEAR)
    if (length(ii > 0)) {
      for (i in 1:length(parList)) {
        #Uptake values for the full year are found 
        #at the first day of the next year, before uptakes are reset
        jj <- which(listStates[[i]][,"time"] == YEAR + 1)[1] #Multiple rows for events, us first row for values before event  
        UptN[ii, paste0("pred_N_UP_", parList[[i]]$Scenario)] <-  listStates[[i]][jj, "N_up"] 
        error <- data.frame(meas = UptN[ii, parList[[i]]$Scenario],
                              pred = UptN[ii, paste0("pred_N_UP_", parList[[i]]$Scenario)])
        error[,"error"] <- error[,"meas"] - error[,"pred"]
        N_error = rbind(N_error, error)
      }
    }
  }
  
  
  P_error = NULL
  for (YEAR in UptP[,"Year"]) {
    ii <- which(UptP[,"Year"] == YEAR)
    if (length(ii > 0)) {
      for (i in 1:length(parList)) {
        #Uptake values for the full year are found 
        #at the first day of the next year, before uptakes are reset
        jj <- which(listStates[[i]][,"time"] == YEAR + 1)[1] #Multiple rows for events, us first row for values before event   
        UptP[ii, paste0("pred_P_UP_", parList[[i]]$Scenario)] <-  listStates[[i]][jj, "P_up"] 
        error <- data.frame(meas = UptP[ii, parList[[i]]$Scenario],
                            pred = UptP[ii, paste0("pred_P_UP_", parList[[i]]$Scenario)])
        error[,"error"] <- error[,"meas"] - error[,"pred"]
        P_error = rbind(P_error, error)
      }
    }   
  }
  
  K_error = NULL
  for (YEAR in UptK[,"Year"]) {
    #Add predictions  
    ii <- which(UptK[,"Year"] == YEAR)
    if (length(ii > 0)) {
      for (i in 1:length(parList)) {
        #Uptake values for the full year are found 
        #at the first day of the next year, before uptakes are reset
        jj <- which(listStates[[i]][,"time"] == YEAR + 1)[1] #Multiple rows for events, us first row for values before event   
        UptK[ii, paste0("pred_K_UP_", parList[[i]]$Scenario)] <-  listStates[[i]][jj, "K_up"] 
        error <- data.frame(meas = UptK[ii, parList[[i]]$Scenario],
                            pred = UptK[ii, paste0("pred_K_UP_", parList[[i]]$Scenario)])
        error[,"error"] <- error[,"meas"] - error[,"pred"]
        K_error = rbind(K_error, error)
      }
    }
  }
  
  if(ShowErrors){
    #Show errors per treatment
    for (i in 1:length(parList)) {
      n_error = UptN[, parList[[i]]$Scenario] - UptN[, paste0("pred_N_UP_", parList[[i]]$Scenario)]
      p_error = UptP[, parList[[i]]$Scenario] - UptP[, paste0("pred_P_UP_", parList[[i]]$Scenario)]
      k_error = UptK[, parList[[i]]$Scenario] - UptK[, paste0("pred_K_UP_", parList[[i]]$Scenario)]
      
      print(paste0("Scenario: ",parList[[i]]$Scenario))
      print(paste0(" N_RMSE ",round(sqrt(mean(n_error^2, na.rm = TRUE)),1),
                   " P_RMSE ",round(sqrt(mean(p_error^2, na.rm = TRUE)),1),
                   " K_RMSE ",round(sqrt(mean(k_error^2, na.rm = TRUE)),1)  ))    
      print(paste0(" N_ME ",round(mean(n_error, na.rm = TRUE),1),
                   " P_ME ",round(mean(p_error, na.rm = TRUE),1),
                   " K_ME ",round(mean(k_error, na.rm = TRUE),1) ))    
    }
    

  }

  return(list(RMSE_N = RMSE(model_up = N_error[,"pred"], meas_up = N_error[,"meas"]),
              RMSE_P = RMSE(model_up = P_error[,"pred"], meas_up = P_error[,"meas"]), 
              RMSE_K = RMSE(model_up = K_error[,"pred"], meas_up = K_error[,"meas"]), 
              N_NSE = NSE(model_up = N_error[,"pred"], meas_up = N_error[,"meas"]),
              P_NSE = NSE(model_up = P_error[,"pred"], meas_up = P_error[,"meas"]),
              K_NSE = NSE(model_up = K_error[,"pred"], meas_up = K_error[,"meas"]),
              UptN = UptN, 
              UptP = UptP, 
              UptK = UptK))
}

AdjustIniPK_Hanninghof <- function(iniPK, times, param){
  #For the Hanninghof experiment
  states = ode(iniPK, func = RCKP_fertilizer, times = times, parms = param,  #method = "ode45",
                 events = PrepareEventList(param = param,  
                                           fyear_fert = 74/365, #Spring application, mid March
                                           fyear_FYM  = 60/365))
  nr <- nrow(states)
  
  iniPK["P_labile"]     = states[nr,"P_labile"]
  iniPK["P_stable"]     = states[nr,"P_stable"]
  iniPK["P_labile_FYM"] = states[nr,"P_labile_FYM"]
  iniPK["K_labile"]     = states[nr,"K_labile"]
  iniPK["K_labile_FYM"] = states[nr,"K_labile_FYM"]
  iniPK["K_stable"]     = states[nr,"K_stable"]
  return(iniPK)
}

Backcast_IniPK_Broadbalk <- function(iniPK, times){
  #For the Broadbalk experiment:
  #Backcast to 1926, the start of the fertilizer experiment
  #Only wheat was grown as monocrop
  
  #Strip 11: NP treatment, including N2 PMg
  #Yields are for control treatment
  param        = RCKP_param(type = "TSP",
                            Crops = c("wheat"), UseMeasuredNUptake = FALSE)
  param$Inputs_reference.Year <- seq(min(times),max(times),1)
  param$Inputs_reference.Crop <- rep("wheat",length(param$Inputs_reference.Year))
  #Yield for unmanured treatment to estimate N supply from soil
  #Source: http://www.era.rothamsted.ac.uk/dataset/rbk1/01-OAWWYields
  #in 1968, 34 kg N/ha was taken up in the control plot with wheat
  #Yields went up from 1000 kg/ha in 1926 to 2600 in 1968 for control
  param$Inputs_reference.Nuptake_kgha <- interp1(x = c(min(times),max(times)), 
                                                     y = c(20, 34),
                                                     xi = seq(min(times),max(times),1))
  param$Inputs_reference.Nfert <- rep(96,length(param$Inputs_reference.Year))
  param$Inputs_reference.Pfert <- rep(35,length(param$Inputs_reference.Year))
  param$Inputs_reference.Pfert_placed <- rep(0,length(param$Inputs_reference.Year))
  param$Inputs_reference.Kfert <- rep(0,length(param$Inputs_reference.Year))
  #N, P and K recovery is the LM estimated recovery for wheat in 1969
  #Old cultivars bused before 1968 yielded 2.5-3.0 t/ha, hybrids used since 1968 yielded 5.5-6.5 t/ha
  #To account for lower NPK removal rates, recovery was taken as 50% of the post-68 value
  param$Inputs_reference.Nrec <- rep(0.5 * 0.682,length(param$Inputs_reference.Year))
  param$Inputs_reference.Prec <- rep(0.5 * 0.402,length(param$Inputs_reference.Year))
  param$Inputs_reference.Krec <- rep(0.5 * 0.208,length(param$Inputs_reference.Year))
  param$Inputs_reference.FYMNrec <- rep(0.5 * 0.319,length(param$Inputs_reference.Year))
  param$Inputs_reference.FYMPrec <- rep(0.5 * 0.218,length(param$Inputs_reference.Year))
  param$Inputs_reference.FYMKrec <- rep(0.5 * 0.253,length(param$Inputs_reference.Year))
  
  param$Inputs_reference.FYMN <- rep(0, length(param$Inputs_reference.Year))
  param$Inputs_reference.FYMP <- rep(0, length(param$Inputs_reference.Year))
  param$Inputs_reference.FYMK <- rep(0, length(param$Inputs_reference.Year))
  param$Inputs_reference.FYMNrec <- rep(0, length(param$Inputs_reference.Year))
  
  param_NP <- param
  #Strip 20: NK treatment, including N2 KMg
  #Yields are for control treatment
  param_NK <- param
  param_NK$Inputs_reference.Pfert <- rep(0,length(param_NK$Inputs_reference.Year))
  param_NK$Inputs_reference.Kfert <- rep(90,length(param_NP$Inputs_reference.Year))

  #get values for look-up table to correct P and K pools for the given parameters
  #It is developed as a function of yield, but yield level proved not important.
  DF_corrections <- PoolRatio(Y0 = 1000, 
                              paramNP = param_NP, 
                              paramNK = param_NK, 
                              times = times)#1 season per year

  #                       year n            * ini year 1/year n
  # factors are smaller than due to NP for K pools and NK treatment for P pools 
  iniPK["P_labile"]     = iniPK["P_labile"] / DF_corrections[1,"P_labile"]
  iniPK["P_stable"]     = iniPK["P_stable"] / DF_corrections[1,"P_stable"]
  iniPK["K_labile"]     = iniPK["K_labile"] / DF_corrections[1,"K_labile"]
  iniPK["K_stable"]     = iniPK["K_stable"] / DF_corrections[1,"K_stable"]
  
  
  #TS: backcasting over such a long period is error prone: it results in negative pools!!
  #Skip for now.
  #Do the backcasting
  # statesNP = ode(iniPK, func = RCKP_fertilizer, times = times, parms = param_NP,  #method = "ode45",
  #                events = list(func = rootFertilizerApplication, root = TRUE),
  #                root = rootTrigger)
  # statesNK = ode(iniPK, func = RCKP_fertilizer, times = times, parms = param_NK,  #method = "ode45",
  #                events = list(func = rootFertilizerApplication, root = TRUE),
  #                root = rootTrigger)
  # nr <- nrow(statesNK)
  # print(subset(statesNP, time %in% c(seq(1965, 1964, -0.05), 1948, 1938, 1928), select = c("time", "N_up", "P_up", "K_up", "K_labile", "K_stable", "P_labile", "P_stable")))
  # print(subset(statesNK, time %in% c(seq(1968, 1958, -1), 1948, 1938, 1928), select = c("time", "N_up", "P_up", "K_up", "K_labile", "K_stable", "P_labile", "P_stable")))
  # 
  # 
  # iniPK["P_labile"]     = statesNK[nr,"P_labile"] 
  # iniPK["P_stable"]     = statesNK[nr,"P_stable"] 
  # iniPK["P_labile_FYM"] = statesNK[nr,"P_labile_FYM"]
  # iniPK["K_labile"]     = statesNP[nr,"K_labile"] 
  # iniPK["K_labile_FYM"] = statesNK[nr,"K_labile_FYM"]
  # iniPK["K_stable"]     = statesNP[nr,"K_stable"] 
  return(iniPK)
}

Forecast_IniPK_Broadbalk <- function(iniPK, times, param){
  #For the Broadbalk experiment:
  nyr <- length(times)
  #Only wheat was grown as monocrop from 1926-1967 on all sections
  param$Inputs_reference.Year <- times
  param$Inputs_reference.Crop <- rep("wheat", nyr)
  #Yield for PK treatment to estimate N supply from soil and deposition
  #Yield for unmanured treatment to estimate N supply from soil
  #Source: http://www.era.rothamsted.ac.uk/dataset/rbk1/01-OAWWYields
  #in 1968, 37 kg N/ha was taken up in the PK plot with wheat
  #Yields went up from 1000 kg/ha in 1926 to 2600 in 1968 for control
  param$Inputs_reference.Nuptake_kgha <- interp1(x = c(min(times),max(times)), 
                                                 y = c(20, 37),
                                                 xi = seq(min(times),max(times),1))
  #N, P and K recovery is the LM estimated recovery for wheat in 1969: for N 0.682, FYMN 0.319
  #Recoveries are set of half that value as yields roughly doubled after 1968 due to new cultivars
  param$Inputs_reference.Nrec <- rep(0.5 * 0.682,nyr)
  param$Inputs_reference.FYMNrec <- rep(0.5 * 0.319,nyr)
  param$Inputs_reference.Nrec_FYM <- rep(0.5 * 0.319,nyr)
  

  param$Inputs_reference.Nfert <- rep( ifelse(param$Inputs_reference.Nfert[1] > 0, param$Inputs_reference.Nfert[1], 0), nyr)
  param$Inputs_reference.Pfert <- rep( ifelse(param$Inputs_reference.Pfert[1] > 0, param$Inputs_reference.Pfert[1], 0), nyr)
  param$Inputs_reference.Kfert <- rep( ifelse(param$Inputs_reference.Kfert[1] > 0, param$Inputs_reference.Kfert[1], 0), nyr)
  param$Inputs_reference.FYMN <- rep( ifelse(param$Inputs_reference.FYMN[1] > 0, param$Inputs_reference.FYMN[1], 0), nyr)
  param$Inputs_reference.FYMP <- rep( ifelse(param$Inputs_reference.FYMP[1] > 0, param$Inputs_reference.FYMP[1], 0), nyr)
  param$Inputs_reference.FYMK <- rep( ifelse(param$Inputs_reference.FYMK[1] > 0, param$Inputs_reference.FYMK[1], 0), nyr)
  param$Inputs_reference.Pfert_placed <- rep( 0, nyr)
  
  if (param$Scenario == "FYMNPK") {
    #This strip 1 received no NPK or FYM before 1968
    #Nothing was removed?
    #If these settings are applied, 
    #Uptake estimates in 1968 (of about 25 kg P, 105 kg K/ha for WW) are strongly underestimated.
    param$Inputs_reference.Nuptake_kgha <- rep( 0, nyr)
    param$Inputs_reference.FYMN <- rep( 0, nyr)
    param$Inputs_reference.FYMP <- rep( 0, nyr)
    param$Inputs_reference.FYMK <- rep( 0, nyr)
    param$Inputs_reference.Nfert <- rep( 0, nyr)
    param$Inputs_reference.Pfert <- rep( 0, nyr)
    param$Inputs_reference.Kfert <- rep( 0, nyr)
  }
  
  if (param$Scenario == "NPKMg") {
    #This NPKMg treatment was NPMg before 1968
    param$Inputs_reference.Kfert <- rep( 0, nyr)
  }

  # states = ode(iniPK, func = RCKP_fertilizer, times = times, parms = param,  #method = "ode45",
  #                events = list(func = rootFertilizerApplication, root = TRUE),
  #                root = rootTrigger)
  states = ode(iniPK, func = RCKP_fertilizer, times = times, parms = param,  #method = "ode45",
               events = PrepareEventList(param = param))
  
  nr <- nrow(states)
  
  iniPK["P_labile"]     = states[nr,"P_labile"]
  iniPK["P_stable"]     = states[nr,"P_stable"]
  iniPK["P_labile_FYM"] = states[nr,"P_labile_FYM"]
  iniPK["K_labile"]     = states[nr,"K_labile"]
  iniPK["K_labile_FYM"] = states[nr,"K_labile_FYM"]
  iniPK["K_stable"]     = states[nr,"K_stable"]

  if (param$Scenario == "FYMNPK") {
    #This strip 1 received no NPK or FYM before 1968
    #Uptake estimates in 1968 (of about 25 kg P, 105 kg K/ha for WW)
    #With a WW rur of 0.165, the uptake/labile pool is 0.15, so the labile P pool is then 167.
    #For K: a WW rur 0.069, uptake/labile is about 0.065, the labile K pool is then 1615.4
    iniPK["P_labile"] <- 167
    iniPK["P_stable"] <- 10 * iniPK["P_labile"]
    iniPK["K_labile"] <- 1615
    iniPK["K_stable"] <- 10 * iniPK["K_labile"]
  }  
  
  return(iniPK)
}


