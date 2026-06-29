#RC_KP model
#Inspired on the RC-P model, developed and tested by Joost Wolf and Bert H. Janssen
#P model was expanded to better describe P uptake when P is placed
#see chapter 4 in PhD thesis of Samuel Njoroge, 2019
#
#RC_KP includes 2 states for P and 3 states for K
#
#Authors: AGT Schut and W. Reymann
#         Plant Production Systems group 
#         Wageningen University, 2021
require('deSolve')        #used for solving ODEs 
require('pracma')         #used for linear interpolation 

#calibrated parameter values
CalFile_Njoroge <- "..//Data//Parameters//10. Njoroge dataset calibrated parameters.csv"
CalFile_Hanninghof <- "..//Data//Parameters//5. Calibrated values for parameter set LMpred_FYM_Hanninghof.csv"
CalFile_Broadbalk <- "..//Data//Parameters//15. Calibrated values for parameter set LMpred_BBalk.csv"

#names of state variables with initial values
RCKP_ini <- function(P_labile=20.0,
                     P_stable=120.0,
                     P_labile_placed = 0.0,
                     P_labile_FYM = 0.0,
                     K_labile=20.0,
                     K_labile_FYM=0,
                     K_stable=120){
  #Initial values for state variables
  # kg/ha
  y <- c(P_labile = P_labile,
         P_stable = P_stable,
         P_labile_placed = P_labile_placed,
         P_labile_FYM = P_labile_FYM,
         K_labile = K_labile,
         K_stable = K_stable,
         K_labile_FYM = K_labile_FYM,
         N_up = 0, #to accumulate annual uptakes
         P_up = 0, #to accumulate annual uptakes
         K_up = 0, #to accumulate annual uptakes
         PIN    = 0, #mass balance term
         POUT   = 0, #mass balance term
         KIN    = 0, #mass balance term
         KOUT   = 0) #mass balance term
         
  return(y) 
}

RCKP_param <- function(Scenario="",
                       type="", 
                       Inputs_reference = data.frame(Crop = "maize", 
                                                     Year = seq(1958,2008,1),
                                                     Pfert = 0, 
                                                     Pfert_placed = 0, 
                                                     Nfert = 0, 
                                                     Nrec = 0,
                                                     Kfert = 0,
                                                     FYMN = 0, FYMP = 0, FYMK = 0,
                                                     FYMNrec = 0,
                                                     Nrec_FYM = 0,
                                                     Nuptake_kgha = NA), 
                       #Parameters fertilizers in kg (N, P, K) /y, yield in kg/ha

                       #optimum uptake when placed and incorporated P do not differ
                       #131 kg P/ha, (van der Eijk et al, 2006), at recovery of 8% is equivalent of 10.5 kg P uptake 
                       #at 2000kg fresh maize yield, uptake of P is 0.4 + 2000/388 = 5.554639 kg P/ha
                       #At yield of 8000 kg, difference in gone, this is about 0.4 + 8000/388 = 21.02 kg P/ha
                       #Source: van der Eijk et al, 2006.
                       P_uptake_balance = 23.30817607, #5.554639 + 10.5, # vd Eijk 2006. At larger applications, no extra yield effect of placing P fertilizer 
                       
                       #ONLY USED FOR NJOROGE TRIAL: to assess balance, it is an arbitrary choice that influences size of initial pools.
                       #Fraction of uptake from labile pool in unfertilized soil that is transferred from stable pool to labile pool
                       #The labile pools in season 7 are not affected, but stable pools are and hence backcasted labile pools change.
                       #Smaller value increase stable pools in season 7 and hence initial labile pools
                       #A value of 0.4 decreases labile pools with a factor 1/0.26 for K and 1/0.72 for P from years 1 to 7.
                       #Yields in the NP and NK treatments without FYM (chapter 6, Njoroge's thesis) 
                       #decreased between LR2013 and LR2016 from 4.3 to 0.5 t/ha
                       Crops = c("potato","oats","rye","wheat", "beans", "silage maize"),
                       CalSetName = "LMpred",
                       UseMeasuredNUptake #user needs to specify!
                       ){
  if(type == "RSP"){
    fsol_P   <- 0.15    # gPsol/gP applied
  }else if(type == "TSP"){     #Triple super phosphate
    fsol_P   <- 0.8    # gPsol/gP applied
  }else if(type == "ASP"){#Ammonium super phosphate
    fsol_P   <- 1.0    # gPsol/gP applied
  }
  
  fsol_K   <- 1.0    # gKsol/gK applied #initial:0.95
  fsol_FYMP <- 0.88  # About 78-80% of P in slurry and pig manure is inorganic and hence soluble when pH < 6
                     # Most of organically bound P is mineralised in the first year, 
                     # Recovery of P (0.34-0.39) is only slightly lower than fertiliser (42%) 
                     # See Smith and van Dijk, 1987:in van der Meer et al. (Eds.), Animal Manure on Grassland and Fodder Crops. Fertilizer or Waste?
                     # Sattari et al. 2010 used a fsol fraction of 0.8 for P.
                     # Up to 48-87% of water soluble. Li et al., 2014. PLOS ONE 9, e102698.
  
  fsol_FYMK <- 1.0  # silent parameter: all goes to the sol pools
  

  #Get crop parameters and calibrated parameter settings 
  CalSetValues <- GetCalValues(CalSetName = CalSetName, Crops= Crops)
  
  return(c(Scenario=Scenario,
           #General parameters
           fsol_P=fsol_P, fsol_FYMP = fsol_FYMP, 
           fsol_K=fsol_K, fsol_FYMK = fsol_FYMK,
           P_uptake_balance=P_uptake_balance,
           Inputs_reference = Inputs_reference, 
           UseMeasuredNUptake = UseMeasuredNUptake,
           #Site-specific parameters
           r_labile_P = unname(CalSetValues$pars["r_labile_P"]),
           r_labile_P_FYM = unname(CalSetValues$pars["r_labile_P_FYM"]),
           r_stable_P = unname(CalSetValues$pars["r_stable_P"]),
           r_labile_K = unname(CalSetValues$pars["r_labile_K"]), 
           r_labile_K_FYM =  unname(CalSetValues$pars["r_labile_K_FYM"]),
           r_stable_K = unname(CalSetValues$pars["r_stable_K"]), 
           #Crop specific parameters
           CropQUEFTS_Dry = CalSetValues$CropQUEFTS_Dry,
           CropQUEFTS_Fresh = CalSetValues$CropQUEFTS_Fresh))
}

GetCalValues <- function(CalSetName, Crops){
  #Some defaults
  pars <- c(r_stable_P   = 1/30, # gP/(gP.y).  ART=30 years
            r_stable_K   = 1/30, # Set this value to one choice: many combinations with r_labile_K give same result...
            r_labile_P   = 0.3,  # gP/(gP.y). 
            r_labile_K   = 0.3,  
            r_labile_P_FYM = 0.0,
            r_labile_K_FYM = 0.0)
  
  #Get crop specific parameters
  CropQUEFTS <- GetCropValues(CalSetName = CalSetName)

  calv <- NULL
  if (!is.null(strfind(CalSetName,"Njoroge"))) {
    calv <- read.csv(CalFile_Njoroge)
  }else if (!is.null(strfind(CalSetName,"Hanninghof"))){
    calv <- read.csv(CalFile_Hanninghof)
  }else if (!is.null(strfind(CalSetName,"BBalk"))) {
    calv <- read.csv(CalFile_Broadbalk)
  }

  if(!is.null(calv)){
    print(paste0("Calibrated parameters for: ",CalSetName))
    
    for(i in 1:nrow(calv)){
      parname <- as.character(calv[i,"parName"])
      parvalue <- calv[i,"optValues"]
      if(is.null(strfind(parname,"CropQUEFTS_Dry"))){
        pars[parname] <- parvalue
      }else{
        #calibration is done for CropQUEFTS_Dry, 
        #but CropQUEFTS_Fresh values are the same for these parameters below.
        #parameters in columns, crops in rows
        for(cn in c("rur_P", "rur_K", "rur_P_placed")){
          for(nr in 1:nrow(CropQUEFTS)){
            comb_crop_param <- paste0("CropQUEFTS_Dry.",cn,"-", CropQUEFTS[nr,"crop"])
            if(parname == comb_crop_param){
              CropQUEFTS[nr, cn] <- parvalue
            }
          }
        }
      }
    }
  }else{
    print("No calibrated parameters added")
  }
  
  #Select only rows for desired crops
  sel_rows = NULL
  for (cr in Crops) {
    sel_rows <- c(sel_rows, which(CropQUEFTS[,"crop"] == cr))
  }
  CropQUEFTS_Fresh <- CropQUEFTS[sel_rows,]
  CropQUEFTS_Fresh[,"crop"] <- as.character(CropQUEFTS_Fresh[,"crop"])
  CropQUEFTS_Dry <- CropQUEFTS_Fresh
  CropQUEFTS_Dry[,"aN"] <- CropQUEFTS_Fresh[,"aN"] * CropQUEFTS_Fresh[,"DMc"]
  CropQUEFTS_Dry[,"dN"] <- CropQUEFTS_Fresh[,"dN"] * CropQUEFTS_Fresh[,"DMc"]
  CropQUEFTS_Dry[,"aP"] <- CropQUEFTS_Fresh[,"aP"] * CropQUEFTS_Fresh[,"DMc"]
  CropQUEFTS_Dry[,"dP"] <- CropQUEFTS_Fresh[,"dP"] * CropQUEFTS_Fresh[,"DMc"]
  CropQUEFTS_Dry[,"aK"] <- CropQUEFTS_Fresh[,"aK"] * CropQUEFTS_Fresh[,"DMc"]
  CropQUEFTS_Dry[,"dK"] <- CropQUEFTS_Fresh[,"dK"] * CropQUEFTS_Fresh[,"DMc"]
  
  if (!is.null(strfind(CalSetName,"BBalk"))) {
    pars["r_labile_P_FYM"] <- pars["r_labile_P"]
    pars["r_labile_K_FYM"] <- pars["r_labile_K"]
  }
  
  return(list(pars=pars, CropQUEFTS_Fresh = CropQUEFTS_Fresh, CropQUEFTS_Dry = CropQUEFTS_Dry))
}

GetCropValues <- function(CalSetName){
  #General crop settings
  Maize <- data.frame(source = "Njoroge", crop = "maize",
                      UptakeStart = 0, UptakeStop = 1, #counted in fraction of year or seasons
                      rN = 5.0, aN = 36.0, dN = 100.0,
                      rP = 0.4, aP = 188.0,dP = 588.0, #aP <- dN / PNratio_max  
                      rK = 2.0, aK = 24.0, dK = 114.0, #aP <- dN / PNratio_max 
                      KNratio_max = 2.25,
                      PNratio_max = 0.4,
                      DMc = 0.88, 
                      rur_P = 0.04, rur_K = 0.06,  rur_P_placed = 0)
  #From Hanninghof data: all in kg fresh yield / kg nutrient
  SilageMaize <- data.frame(source = "Broadbalk", crop = "silage maize",
                            UptakeStart = 0, UptakeStop = 1, #counted in fraction of year or seasons
                            rN = 0.5, aN = 250.0, dN = 600.0,
                            rP = 0.2, aP = 1000.0,dP = 2667.0,
                            rK = 2.0, aK = 187.5, dK = 700.0,#aK from Broadbalk (250 Hh), dK from Hanninghof
                            DMc   = 0.26, #0.26 was measured in Broadbalk
                            KNratio_max = 2.25,
                            PNratio_max = 0.4,
                            rur_P = 0.01, rur_K = 0.05, rur_P_placed = 0)
  Potato <- data.frame(source = "Hanninghof&Broadbalk", crop = "potato",
                       UptakeStart = 0, UptakeStop = 1, #counted in fraction of year or seasons
                       rN = 5.0, aN = 200.0, dN = 500.0, #dN from broadbalk (400 Hh), aN from Hanninghof
                       rP = 0.4, aP = 1100.0,dP = 2500.0,#both from Hanninghof
                       rK = 2.0, aK = 85, dK = 350.0, #dN from Broadbalk (350 Hh), aN from Hanninghof
                       DMc   = 0.23, #0.2275 was measured in Broadbalk
                       KNratio_max = 2.5,
                       PNratio_max = 0.28,
                       rur_P = 0.007, rur_K = 0.07, rur_P_placed = 0) 
  Oats <- data.frame(source = "Hanninghof&Broadbalk", crop = "oats",
                     UptakeStart = 0, UptakeStop = 1, #counted in fraction of year or seasons
                     rN = 5.0, aN = 45.0, dN = 105.0,#aN from Hanninghof, dN from Bradbalk (85 HH))
                     rP = 0.4, aP = 144.0,dP = 400.0,#dP from Broadbalk (300 Hh), aK from Hanninghof
                     rK = 2.0, aK = 33.0, dK = 200.0,#dk from Broadbalk (114 Hh), aK from Hanninghof
                     DMc   = 0.88, #0.88 was measured in Broadbalk
                     KNratio_max = 1.5,
                     PNratio_max = 0.4,
                     rur_P = 0.01, rur_K = 0.05, rur_P_placed = 0) 
  
  Rye <- data.frame(source = "Hanninghof", crop = "rye", #Winter rye
                    UptakeStart = 0, UptakeStop = 1, #counted in fraction of year or seasons
                    rN = 5.0, aN = 43.0, dN = 85.0,
                    rP = 0.4, aP = 175.0,dP = 425.0,
                    rK = 2.0, aK = 48.0, dK = 220.0, 
                    DMc   = 0.88, 
                    KNratio_max = 1.0,
                    PNratio_max = 0.3,
                    rur_P = 0.01, rur_K = 0.05, rur_P_placed = 0) 
  Wheat <- data.frame(source = "Broadbalk", crop = "wheat", #Winter wheat
                    UptakeStart = 0, UptakeStop = 1, #counted in fraction of year or seasons
                    rN = 5.0, aN = 22.9, dN = 85.0,
                    rP = 0.4, aP = 150.0,dP = 600.0, #rP and rK guestimated from Liu et al.
                    rK = 2.0, aK = 25.0, dK = 200.0, #2.0
                    DMc   = 0.85, 
                    KNratio_max = 0.8,
                    PNratio_max = 0.25,
                    rur_P = 0.01, rur_K = 0.05, rur_P_placed = 0) 
  Beans <- data.frame(source = "Broadbalk", crop = "beans",
                    UptakeStart = 0, UptakeStop = 1, #counted in fraction of year or seasons
                    rN = 5.0, aN = 17.0, dN = 28.0,
                    rP = 0.4, aP = 120.0,dP = 275.0,
                    rK = 2.0, aK = 28.0, dK = 100.0,
                    DMc   = 0.80,
                    KNratio_max = 0.7,
                    PNratio_max = 0.13,
                    rur_P = 0.02, rur_K = 0.05, rur_P_placed = 0) 
  
  CropQUEFTS_Fresh <- rbind(Maize, SilageMaize, Potato, Oats, Rye, Wheat, Beans)
  
  return(CropQUEFTS_Fresh)
}

# From QUEFTS, see Janssen et al., Sattari et al.
# Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
nutrient_uptake <- function(S1=NA, S2=NA, d1=NA, a1=NA, d2=NA, a2=NA, r1=NA, r2=NA)
{
  # N, P and K uptakes based on QUEFTS
  if (S1 < r1){
    uptakeS1_givenS2 = S1
  }
  else if (S1 < r1 + (S2 - r2) * a2 / d1) {
    #S1 uptake is yield limiting
    uptakeS1_givenS2 = S1
  }  
  else if (S1 > r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1))){  
    #S2 determines maximum possible yield
    uptakeS1_givenS2 = r1 + (S2 - r2) * d2 / a1
  }
  else{
    #Curved relationship
    uptakeS1_givenS2 = S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))**2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
  }
  
  # Nutrient uptake given availability of other nutrient
  return(uptakeS1_givenS2)
}

# From QUEFTS, see Janssen et al., Sattari et al.
# Actual uptake of nutrients as determined by the availability of the other nutrients
actual_uptake <- function(supply, param, UseMeasuredNUptake)
{
  with(c(supply, param),
       {
         aP <- dN / PNratio_max 
         aK <- dN / KNratio_max

         if(UseMeasuredNUptake){
           #SN is here actual uptake, so no need to estimate!!!
           UN <- SN
         }else{
           UNP <- nutrient_uptake(S1 = SN, S2 = SP, d1 = dN, a1 = aN, d2 = dP, a2 = aP, r1 = rN, r2 = rP)
           UNK <- nutrient_uptake(S1 = SN, S2 = SK, d1 = dN, a1 = aN, d2 = dK, a2 = aK, r1 = rN, r2 = rK)
           UN <- min(UNP, UNK)
         }
         #Set maximum to SP and SK that can be taken up given actual N uptake
         #Estimates of UP and UK are assuming that the "other" nutrient is not limiting
         #Accumulation of P and K do not occur when N is deficient.
         #Largest accumulations for K are found for NK treatment
         
         UPN <- nutrient_uptake(S1 = SP, S2 = SN, d1 = dP, a1 = aP, d2 = dN, a2 = aN, r1 = rP, r2 = rN)
         UPK <- nutrient_uptake(S1 = SP, S2 = SK, d1 = dP, a1 = aP, d2 = dK, a2 = aK, r1 = rP, r2 = rK)
         UP <- min(UPN, UPK)
         
         UKN <- nutrient_uptake(S1 = SK, S2 = SN, d1 = dK, a1 = aK, d2 = dN, a2 = aN, r1 = rK, r2 = rN)
         UKP <- nutrient_uptake(S1 = SK, S2 = SP, d1 = dK, a1 = aK, d2 = dP, a2 = aP, r1 = rK, r2 = rP)
         UK <- min(UKN, UKP)
         
         return(c(UN = UN, UP = UP, UK = UK))
       })
}

#Function with rates
RCKP_fertilizer <- function(Time, State, Pars){
  # Now assuming that fertilizer is placed just below the seed 
  # Plants take up from that pool directly and some from the soluble pool.
  # Uptake is proportional to volume of enriched soil
  Upt_KP <- RCKP_uptake(Time, State, Pars)
  with(as.list(c(State, Pars, Upt_KP)),{

    RATES <- c( dP_labile_dt  = r_stable_P * P_stable
                              - r_labile_P * P_labile 
                              - P_up_labile,
                dP_stable_dt  = (r_labile_P * P_labile 
                              + r_labile_P * P_labile_placed
                              + r_labile_P_FYM * P_labile_FYM
                              - r_stable_P * P_stable),
                dP_labile_placed_dt = - r_labile_P * P_labile_placed - P_up_labile_placed, 
                dP_labile_FYM_dt    = - r_labile_P_FYM * P_labile_FYM - P_up_labile_FYM,
                dK_labile_dt        = r_stable_K * K_stable - r_labile_K * K_labile - K_up_labile,
                dK_stable_dt       =  r_labile_K * K_labile + r_labile_K_FYM * K_labile_FYM - r_stable_K * K_stable,
                dK_labile_FYM_dt    = - r_labile_K_FYM * K_labile_FYM - K_up_labile_FYM,
                dN_up_dt = UN,
                dP_up_dt = UP,
                dK_up_dt = UK)
    MB <- c(dPINdt   = 0,
            dPOUTdt  = UP,
            dKINdt   = 0,
            dKOUTdt  = UK)
    
  
    AUX <- c(P_up_labile = unname(P_up_labile),
             P_up_labile_placed = unname(P_up_labile_placed),
             P_up_labile_FYM = unname(P_up_labile_FYM))
    
    return(list( c(RATES, MB),AUX))
  })  
}

RCKP_uptake <- function(Time, State, Pars){
  with(as.list(c(State, Pars)),{
    #No crop and no reference yield present!
    uptakeList <- list(P_up_labile  = 0,
                       P_up_labile_placed  = 0,
                       P_up_labile_FYM    = 0,
                       K_up_labile  = 0, 
                       K_up_labile_FYM    = 0,
                       UN      = 0,
                       UP      = 0,
                       UK      = 0)
    
    
    indexYear <- which(Inputs_reference.Year == floor(Time))
    if ( length(indexYear) > 0) {
      #Get QUEFTS parameters for this croptype
      indexCrop <- which(as.character(CropQUEFTS_Dry.crop) == as.character(Inputs_reference.Crop[indexYear]))
       
      #Check if time is within growing season
      if ( (Time - floor(Time) ) >= CropQUEFTS_Dry.UptakeStart[indexCrop] & 
                 (Time - floor(Time) ) <= CropQUEFTS_Dry.UptakeStop[indexCrop]) {
        #Get QUEFTS parameters for this croptype
        indexCrop <- which(as.character(CropQUEFTS_Dry.crop) == as.character(Inputs_reference.Crop[indexYear]))
        ##Set rur to crop type specific value
        rur_P   <- CropQUEFTS_Dry.rur_P[indexCrop] 
        rur_P_placed <- CropQUEFTS_Dry.rur_P_placed[indexCrop]
        rur_K   <- CropQUEFTS_Dry.rur_K[indexCrop] 

        paramQUEFTS_Dry <- c(rN = CropQUEFTS_Dry.rN[indexCrop], aN = CropQUEFTS_Dry.aN[indexCrop], dN = CropQUEFTS_Dry.dN[indexCrop],
                             rP = CropQUEFTS_Dry.rP[indexCrop], aP = CropQUEFTS_Dry.aP[indexCrop], dP = CropQUEFTS_Dry.dP[indexCrop],
                             rK = CropQUEFTS_Dry.rK[indexCrop], aK = CropQUEFTS_Dry.aK[indexCrop], dK = CropQUEFTS_Dry.dK[indexCrop],
                             KNratio_max = CropQUEFTS_Dry.KNratio_max[indexCrop],
                             PNratio_max = CropQUEFTS_Dry.PNratio_max[indexCrop])
        
        
        if ( length(indexCrop) == 0 ){
          print(as.character(Inputs_reference.Crop[indexYear]))
          print(as.character(CropQUEFTS_Dry.crop))
          print(floor(Time))
          stop("crop type not defined in dataframe for QUEFTS parameters for this specific year")
        }
        #Normal uptake of K and P from the soluble pool
        #When placed, direct uptake bypassing the soluble pool, if the labile P pool is very small. 
        #At a supply of 130 kg P/ha, no yield difference between broadcast and placed
        #This formulation means that extra uptake only takes place in the first 3-4 years after first P... 
        P_labile_balance = P_uptake_balance / rur_P
        rur_P_placed_corr = max(rur_P, rur_P_placed * (1 - pmin(1, P_labile/P_labile_balance)))
    

        #QUEFTS function
        #Soil supply is here defined as the uptake realized by crops
        #So it is the uptake from the soil pool measured in e.g. P in the NK plot + P-recovery * P fertiliser.
        #It may be that the proportional uptake from the fraction of P uptake from the soil pool is larger in the 
        #omission treatments than in fertilized treatments (e.g. due to priming effects or rooting effects).
        #N is mostly limiting, assuming some more dilution than for balanced nutrition
    
       #Calculate actual uptake limited by N, P or K from potential supply
        #Uptakes from soil pool cannot be negative!! May occur when backcasting....
        if (length(Inputs_reference.Nuptake_kgha) == length(Inputs_reference.Year) 
            & !is.na(Inputs_reference.Nuptake_kgha[indexYear])) {
          SN <- Inputs_reference.Nuptake_kgha[indexYear]
        }else{
          stop("Error: N uptake estimate is required but NA or missing")
        }
        
        #N recovery of fertilizer N may be different when also FYM is applied
        #FYMNrec = (N uptake FYM - N uptake PK) / FYMNrate
        NFYM <-  Inputs_reference.FYMNrec[indexYear] * Inputs_reference.FYMN[indexYear]
        
        #Nrec_FYM = (N uptake FYMNPK - N uptake NPK) / Nrate
        #Nrec = (N uptake NPK - N uptake PK) / Nrate
        NF <- ifelse(Inputs_reference.FYMN[indexYear] == 0, 
                     Inputs_reference.Nrec[indexYear] * Inputs_reference.Nfert[indexYear],
                     Inputs_reference.Nrec_FYM[indexYear] * Inputs_reference.Nfert[indexYear])
        #Annual applications in kg/ha need to be converted to rates in kg/(ha y)
        #has unit y
        DurationGrowingSeason <- CropQUEFTS_Dry.UptakeStop[indexCrop] - CropQUEFTS_Dry.UptakeStart[indexCrop]
  
        supply = list(SN = (SN + NFYM + NF) / DurationGrowingSeason, 
                      SP = rur_P * P_labile + rur_P * P_labile_FYM + rur_P_placed_corr * P_labile_placed,
                      SK = rur_K * K_labile + rur_K * K_labile_FYM)
  
        if(is.na(supply$SN) | is.na(supply$SP) | is.na(supply$SK)){
          print(paramQUEFTS_Dry)
          print(data.frame(Year = floor(Time),
                           YearRef = Inputs_reference.Year[indexYear],
                           Crop = as.character(Inputs_reference.Crop[indexYear]),
                           Nuptake = Inputs_reference.Nuptake_kgha[indexYear],
                           Nfert = Inputs_reference.Nfert[indexYear],
                           FYMN = Inputs_reference.FYMN[indexYear],
                           Nrec = Inputs_reference.Nrec[indexYear], 
                           Nrec_FYM = Inputs_reference.Nrec_FYM[indexYear],
                           FYMNrec = Inputs_reference.FYMNrec[indexYear]))
          print(data.frame(P_labile = P_labile, 
                           P_labile_FYM = P_labile_FYM,
                           P_labile_placed = P_labile_placed,
                           K_labile = K_labile,
                           K_labile_FYM = K_labile_FYM))
          print(supply)
          stop("SN, SP or SK supply not available")
        }
        
        Uptake_NPK <- actual_uptake(supply,param = paramQUEFTS_Dry, UseMeasuredNUptake = UseMeasuredNUptake)
  
        #Ensure that uptakes do not become negative!!
        Uptake_NPK["UN"] <- max(0, Uptake_NPK["UN"])
        Uptake_NPK["UP"] <- max(0, Uptake_NPK["UP"])
        Uptake_NPK["UK"] <- max(0, Uptake_NPK["UK"])
        
        #Actual uptakes from fertilizer corrected for imbalanced supply
        #Uptake from fertilizer and soil pool are adjusted proportionally
        P_up_prop <- Uptake_NPK["UP"] / (rur_P * P_labile + rur_P * P_labile_FYM + rur_P_placed_corr * P_labile_placed)
        K_up_prop <- Uptake_NPK["UK"] / (rur_K * K_labile + rur_K * K_labile_FYM)
        
        uptakeList <- list(P_up_labile         = P_up_prop * rur_P * P_labile,
                           P_up_labile_placed  = P_up_prop * rur_P_placed_corr * P_labile_placed,
                           P_up_labile_FYM     = P_up_prop * rur_P * P_labile_FYM,
                           K_up_labile         = K_up_prop * rur_K * K_labile, 
                           K_up_labile_FYM     = K_up_prop * rur_K * K_labile_FYM,
                           UN                  = Uptake_NPK["UN"],
                           UP                  = Uptake_NPK["UP"],
                           UK                  = Uptake_NPK["UK"])
       }
    }
    return(uptakeList)
  })
}

#Initialisation of P and K pools at the start of the experiment
RCKP_estimate_initial_pools <- function(param = param,            #All parameters
                                        Crop = "wheat",           #For extraction of crop=specific parameters
                                        Kupt_US = UptK[1,NPtrt],  #K uptake from unfertilized soil #3 (uptake in NP treatment)
                                        Pupt_US = UptP[1,NKtrt]){ #P uptake from unfertilized soil #3 (uptake in NK treatment)
  #US is unfertilized soil & FS is fertilized soil
  #Numbers in comments indicate equation line in Table 3 of Wolf et al 1987
  #Note that P and K from manure is not included here as uptakes are from treatments 
  #with only PK, NK or NPK without FYM!!
  
  #Get crop specific fupt
  indexCrop <- which(param$CropQUEFTS_Dry.crop == Crop)
  rur_P <- param$CropQUEFTS_Dry.rur_P[indexCrop] 
  rur_K <- param$CropQUEFTS_Dry.rur_K[indexCrop] 
  
  #Initial situation: labile (LP) and stable pools (SP) for unfertilized soil (US)
  #Assuming balance: uptake from labile pool is fully matched by transfer from stable pool
  LKUS     <- Kupt_US  / rur_K                #14 Size of LP for K
  LK_SK_US <- LKUS     * param$r_labile_K     #15 and 5 Transfer LP to SP for K
  SK_LK_US <- Kupt_US  + LK_SK_US             #16 transfer SP to LP for K
  SKUS     <- SK_LK_US * (1/param$r_stable_K) #17 and 6 size of SP for K
  
  LPUS     <- Pupt_US  / rur_P                #14 Size of LP for P
  LP_SP_US <- LPUS     * param$r_labile_P     #15 and 6 Transfer LP to SP for P
  SP_LP_US <- Pupt_US  + LP_SP_US             #16 transfer SP to LP for P
  SPUS     <- SP_LP_US * (1/param$r_stable_P) #17 and 7 size of SP for P
  
  #K & P  pools and uptake assuming K&P omission continued in NP/NK plot, extra lines for safety
  #First row, this is the first year in the series to test
  iniPK <- RCKP_ini(P_labile = unname(LPUS), P_stable = unname(SPUS),
                    K_labile = unname(LKUS), K_stable = unname(SKUS))
  return(iniPK)
}



#Yields are considered to decline linearly by 50 kg/ha per year.
ShowBackcasting <- function(nyears = 25){
  Inputs_reference = data.frame(Crop = "maize", 
                                    Year = seq(0,50,1),
                                    Nfert = 0, 
                                    FYMN = 0,
                                    Nrec = 0.5,
                                    FYMNrec = 0.25,
                                    Pfert = 0, 
                                    Pfert_placed = 0, 
                                    FYMP = 0,
                                    Kfert = 0,
                                    FYMK = 0,
                                    Nuptake_kgha = NA) 
  

   ini = RCKP_ini()
   param = RCKP_param(type = "ASP",Inputs_reference = Inputs_reference, Crops = "maize", UseMeasuredNUptake = FALSE)

   #For simplicity, both NK and NP plots have balanced uptake, so a yield of 3000 kg equals
   #an uptake of about 3000/50=60 kg N/ha, 3000/400=7.5 kg P/ha, and 3000/75=40 kg K/ha.
   #Define pools based on P uptake of 7.5 kgP/ha in NK plot and K uptake of 40 kgK/ha in NP plot
   Inputs_reference[,"Pfert_placed"] <- 0
   Inputs_reference[,"Pfert"] <- 0 
   Inputs_reference[,"Nfert"] <- 150
   Inputs_reference[,"Kfert"] <- 60
   paramNK = RCKP_param(type = "ASP", Inputs_reference = Inputs_reference, Crops="maize", UseMeasuredNUptake = FALSE)
   
   Inputs_reference[,"Pfert_placed"] <- 40
   Inputs_reference[,"Pfert"] <- 0 
   Inputs_reference[,"Nfert"] <- 150
   Inputs_reference[,"Kfert"] <- 0
   paramNP = RCKP_param(type = "ASP", Inputs_reference = Inputs_reference, Crops="maize", UseMeasuredNUptake = FALSE)
   
   ini <- RCKP_estimate_initial_pools(param = paramNK,            #All parameters
                                      Crop = "maize",           #For extraction of crop=specific parameters
                                      Kupt_US = 7.5,  #K uptake from unfertilized soil #3 (uptake in NP treatment)
                                      Pupt_US = 40)

   #Run for n years
   statesNK <- ode(ini,seq(0, nyears, by = 1), RCKP_fertilizer, paramNK,  #method = "ode45",
                   events = list(func = rootFertilizerApplication, root = TRUE),
                   root = rootTrigger)

   #get last year as initial value
   ii <- nrow(statesNK)
   ini_finalNK <- RCKP_ini(P_labile=as.numeric(statesNK[ii,"P_labile"]),
                           P_stable=as.numeric(statesNK[ii,"P_stable"]),
                           P_labile_placed=as.numeric(statesNK[ii,"P_labile_placed"]),
                           K_labile=as.numeric(statesNK[ii,"K_labile"]),
                           K_stable=as.numeric(statesNK[ii,"K_stable"]))
   #Backcast to first year. 
   statesNK_reverse <- ode(ini_finalNK, seq(nyears, 0, by = -0.01  ), RCKP_fertilizer, paramNK,#  method = "ode45",
                           events = list(func = rootFertilizerApplication, root = TRUE),
                           root = rootTrigger)

   
   par(mfrow=c(2,3))
   plot(statesNK[,"time"], statesNK[,"P_labile"],col="black", type="b",
        xlab="Time",ylab="states NK, P_labile",main="P_labile")
   lines(statesNK_reverse[,"time"], statesNK_reverse[,"P_labile"],col="red")
   
   plot(statesNK[,"time"], statesNK[,"P_stable"],col="black", type="b",
        xlab="Time",ylab="states NK, P Ins",main="P_stable")
   lines(statesNK_reverse[,"time"], statesNK_reverse[,"P_stable"],col="red")
   
   plot(statesNK[,"time"], statesNK[,"P_up"],col="black", type="b",
        xlab="Time",ylab="up_states NK",main="P_up")
   lines(statesNK_reverse[,"time"], statesNK_reverse[,"P_up"],col="red")
   
   
   #Run for n years
   statesNP <- ode(ini,seq(0, nyears, by = 1), RCKP_fertilizer, paramNP,#  method = "ode45",
                   events = list(func = rootFertilizerApplication, root = TRUE),
                   root = rootTrigger)
   statesNP <- as.data.frame(statesNP)
   #get last year as inital value
   ii<-nrow(statesNP)
   ini_finalNP <- RCKP_ini(P_labile=as.numeric(statesNP[ii,"P_labile"]),
                           P_stable=as.numeric(statesNP[ii,"P_stable"]),
                           P_labile_placed=as.numeric(statesNP[ii,"P_labile_placed"]),
                           K_labile=as.numeric(statesNP[ii,"K_labile"]),
                           K_stable=as.numeric(statesNP[ii,"K_stable"]))
   statesNP_reverse <- ode(ini_finalNP, seq(nyears, 0, by = -0.01  ), RCKP_fertilizer, paramNP,#  method = "ode45",
                           events = list(func = rootFertilizerApplication, root = TRUE),
                           root = rootTrigger)

   
   plot(statesNP[,"time"], statesNP[,"K_labile"],col="black", type="b",
        xlab="Time",ylab="states NP, K_labile",main="K_labile")
   lines(statesNP_reverse[,"time"], statesNP_reverse[,"K_labile"],col="red")
   
   plot(statesNP[,"time"], statesNP[,"K_stable"],col="black", type="b",
        xlab="Time",ylab="states NP, K_labile",main="K_stable")
   lines(statesNP_reverse[,"time"], statesNP_reverse[,"K_stable"],col="red")
   legend("topright",legend=c("time forward","backcast"),col=c("black","red"),pch=c(1,-1),lty=1,bty="n")
   
   plot(statesNP[,"time"], statesNP[,"K_up"],col="black", type="b",
        xlab="Time",ylab="up_states NP",main="K_up")
   lines(statesNP_reverse[,"time"], statesNP_reverse[,"K_up"],col="red")
   return(rbind(statesNP, statesNP_reverse))
}
 
#aa <- ShowBackcasting(nyears = 25)


RCKP_MassBalance <- function(iniStates, States){
  States <- as.data.frame(States)
  for (col in c("P_labile","P_stable","P_labile_placed","P_labile_FYM","K_labile","K_stable","K_labile_FYM")){
    States[,paste0(col,"_ini")] <- iniStates[col]
  }
  MB_P <- (  States[,"P_labile"]     + States[,"P_stable"] + States[,"P_labile_placed"] + States[,"P_labile_FYM"]
           - States[,"P_labile_ini"] - States[,"P_stable_ini"] - States[,"P_labile_placed_ini"] - States[,"P_labile_FYM_ini"] 
           - States[,"PIN"]         + States[,"POUT"])
  MB_K <- (  States[,"K_labile"]     + States[,"K_stable"]     + States[,"K_labile_FYM"] 
           - States[,"K_labile_ini"] - States[,"K_stable_ini"] - States[,"K_labile_FYM_ini"]
           - States[,"KIN"]         + States[,"KOUT"])
  States[,"MB_P"] <- MB_P
  States[,"MB_K"] <- MB_K
  
  plot(States[,"time"],States[,"MB_P"], type="l", col="blue", 
       ylim=c(min(c(States[,"MB_P"],States[,"MB_K"])),
              max(c(States[,"MB_P"],States[,"MB_K"]))),
       xlab="Time", ylab="Mass balance, kg/ha")
  lines(States[,"time"],States[,"MB_K"], col="red")
  legend("topleft",legend=c("P","K"),lty=1,col=c("blue","red"),bty="n")

  return(States)
} 
#Testing of equilibria: run for nyears with some P+K input and large N supply 
#Testing of equilibria: run for nyears without P+K input and large N supply 
#Uptake is now depending on P and K supply but is not limited by N
TestEquilibria <- function(nyears =  5000){
  Inputs_reference = data.frame(Crop = "maize", 
                                    Year = seq(0,nyears,1),
                                    Pfert = 0, 
                                    Pfert_placed = 40, 
                                    Nfert = 150, 
                                    Kfert = 75,
                                    FYMN = 0, FYMP = 0, FYMK = 0,
                                    Nrec = 0.5, FYMNrec = 0,
                                    Nuptake_kgha = 45) 
   ini = RCKP_ini()
   param=RCKP_param(type="ASP", Inputs_reference = Inputs_reference, Crops= "maize", UseMeasuredNUptake = FALSE)
   states1 = ode(ini, func = RCKP_fertilizer, times= seq(0,nyears,1),parms = param, method="ode45" )

   Inputs_reference = data.frame(Crop = "maize", 
                                       Year = seq(0,nyears,1),
                                       Pfert = 0, 
                                       Pfert_placed = 0, 
                                       Nfert = 150, 
                                       Kfert = 0,
                                       FYMN = 0, FYMP = 0, FYMK = 0,
                                       Nrec = 0.5, FYMNrec = 0,
                                       Nuptake_kgha = 45) 
   param = RCKP_param(type= "ASP", Inputs_reference = Inputs_reference, Crops= "maize", UseMeasuredNUptake = FALSE)
   states2 = ode(ini, func = RCKP_fertilizer, times= seq(0,nyears,1),parms = param, method="ode45" )
   return(list(states1 = states1,
            states2 = states2))
}


# rootTrigger  <- function(Time, State, Pars){
#   #  This root function triggers an event when the return is positive
#   with(as.list(c(State, Pars)),{
#     #triggers an event.
#     rv <- -1
#     if( mod(Time, 1) < 1E-20){
#       rv <- 1
#     }
#     return(rv)
#   })
# }
# rootFertilizerApplication  <- function(Time, State, Pars){
#   #There seem to be a problem with if difference returns a tiny result 
#   #An extra condition was added to prevent double applications.
#   with(as.list(c(State, Pars)),{
#     if( mod(Time, 1) < 1E-20){
#       #print(paste0("Fertilizer application: ", Time))
#       indexYear <- which(Inputs_reference.Year == floor(Time))
#       if ( length(indexYear) == 0 ){
#         Pfert        <-  0 
#         Pfert_placed <-  0
#         Kfert        <-  0
#         Pfert_FYM    <-  0
#         Kfert_FYM    <-  0
#       }else{
#         Pfert        <-  Inputs_reference.Pfert[indexYear] 
#         Pfert_placed <-  Inputs_reference.Pfert_placed[indexYear] 
#         Kfert        <-  Inputs_reference.Kfert[indexYear] 
#         Pfert_FYM    <-  Inputs_reference.FYMP[indexYear]
#         Kfert_FYM    <-  Inputs_reference.FYMK[indexYear]
#       }
#         
#         #Add whatever is left from the placed P to the stable pool
#         #Representing ploughing, when placed P gets mixed in the soil.
#         P_stable <- P_stable + P_labile_placed 
#         
#         y <- c( P_labile        = P_labile + fsol_P * Pfert,
#               P_stable        = P_stable + (1 - fsol_P) * Pfert + (1 - fsol_P) * Pfert_placed + (1 - fsol_FYMP) * Pfert_FYM,
#               P_labile_placed = fsol_P * Pfert_placed,
#               P_labile_FYM    = P_labile_FYM + fsol_FYMP * Pfert_FYM, 
#               K_labile        = K_labile + fsol_K * Kfert,
#               K_labile_FYM    = K_labile_FYM + fsol_FYMK * Kfert_FYM, #do nothing here
#               K_stable        = K_stable + (1 - fsol_K) * Kfert + (1 - fsol_FYMK) * Kfert_FYM, 
#               N_up            = 0, #Reset to 0: to accumulate annual uptakes
#               P_up            = 0, #Reset to 0: to accumulate annual uptakes
#               K_up            = 0, #Reset to 0: to accumulate annual uptakes
#               PIN             = PIN + Pfert + Pfert_placed + Pfert_FYM,
#               POUT            = POUT,
#               KIN             = KIN + Kfert + Kfert_FYM,
#               KOUT            = KOUT)
#       } else{
#         #do nothing
#           y <- c( P_labile        = P_labile,
#                   P_stable        = P_stable,
#                   P_labile_placed = P_labile_placed,
#                   P_labile_FYM    = P_labile_FYM,
#                   K_labile        = K_labile,
#                   K_labile_FYM    = K_labile_FYM,
#                   K_stable        = K_stable, 
#                   N_up            = N_up, #to accumulate annual uptakes
#                   P_up            = P_up, #to accumulate annual uptakes
#                   K_up            = K_up, #to accumulate annual uptakes
#                   PIN             = PIN,
#                   POUT            = POUT,
#                   KIN             = KIN,
#                   KOUT            = KOUT)
#       }
#       return(y)
#     })
# }

PrepareEventList <- function(param, 
                             fyear_fert = 74/365, #Spring application, mid March
                             fyear_FYM = 1/365){ #Autumn application, 30/365 for Nov, Jan 1
  #Set accumulated N, P and K uptakes to 0 at start of year
  n_up <- data.frame(var =  "N_up", time = param$Inputs_reference.Year, value= 0, method="multiply")
  p_up <- data.frame(var =  "P_up", time = param$Inputs_reference.Year, value= 0, method="multiply")
  k_up <- data.frame(var =  "K_up", time = param$Inputs_reference.Year, value= 0, method="multiply")
  

  #Prepare fertilizer events
  pfertLP <- data.frame(var =  "P_labile_placed",
                       time= param$Inputs_reference.Year + fyear_fert,
                       value= param$Inputs_reference.Pfert_placed * param$fsol_P,
                       method="add")
  pfertL <- data.frame(var =  "P_labile",
                       time= param$Inputs_reference.Year + fyear_fert,
                       value= param$Inputs_reference.Pfert * param$fsol_P,
                       method="add")
  pfertS <- data.frame(var =  "P_stable",
                       time= param$Inputs_reference.Year + fyear_fert,
                       value= (param$Inputs_reference.Pfert 
                               + param$Inputs_reference.Pfert_placed) * (1 - param$fsol_P),
                       method="add")
  pfertI <- data.frame(var =  "PIN",
                       time= param$Inputs_reference.Year + fyear_fert,
                       value= param$Inputs_reference.Pfert + param$Inputs_reference.Pfert_placed,
                       method="add")
 
  kfertL <- data.frame(var =  "K_labile",
                       time= param$Inputs_reference.Year + fyear_fert,
                       value= param$Inputs_reference.Kfert * param$fsol_K,
                       method="add")
  kfertS <- data.frame(var =  "K_stable",
                       time= param$Inputs_reference.Year + fyear_fert,
                       value= param$Inputs_reference.Kfert * (1 - param$fsol_K),
                       method="add")
  kfertI <- data.frame(var =  "KIN",
                       time= param$Inputs_reference.Year + fyear_fert,
                       value= param$Inputs_reference.Kfert,
                       method="add")
  
  #Prepare FYM
  pfymL <- data.frame(var =  "P_labile_FYM",
                       time= param$Inputs_reference.Year + fyear_FYM,
                       value= param$Inputs_reference.FYMP * param$fsol_FYMP,
                       method="add")
  pfymS <- data.frame(var =  "P_stable",
                       time= param$Inputs_reference.Year + fyear_FYM,
                       value= param$Inputs_reference.FYMP * (1 - param$fsol_FYMP),
                       method="add")
  pfymI <- data.frame(var =  "PIN",
                       time= param$Inputs_reference.Year + fyear_FYM,
                       value= param$Inputs_reference.FYMP,
                       method="add")
  
  kfymL <- data.frame(var =  "K_labile_FYM",
                       time= param$Inputs_reference.Year + fyear_FYM,
                       value= param$Inputs_reference.FYMK * param$fsol_FYMK,
                       method="add")
  kfymS <- data.frame(var =  "K_stable",
                       time= param$Inputs_reference.Year + fyear_FYM,
                       value= param$Inputs_reference.FYMK * (1 - param$fsol_FYMK),
                       method="add")
  kfymI <- data.frame(var =  "KIN",
                       time= param$Inputs_reference.Year + fyear_FYM,
                       value= param$Inputs_reference.FYMK,
                       method="add")
  
  
  fert <- rbind(n_up, p_up, k_up,
                pfertLP,
                pfertL, pfertS, pfertI, 
                kfertL, kfertS, kfertI,
                pfymL, pfymS, pfymI, 
                kfymL, kfymS, kfymI)
  sfert <- sort(fert[,"time"], index.return = TRUE)
  
  return(list(data = fert[sfert$ix,] ))
}



#out <- TestEquilibria(nyears = 200)


#Add Mass balances
#states1 <- RCKP_MassBalance(ini, out[[1]])

#par(mfcol=c(1,2), mar=c(4,4,1,1))
#plot(states1[,"time"], states1[,"MB_P"], type="l", col="black", main = "MB for P")
#plot(states1[,"time"], states1[,"MB_K"], type="l", col="red", main = "MB for K")


PoolRatio <- function(Y0, paramNP, paramNK, times){
  #Get crop specific fupt for P
  #Uptake fraction labile pool P #13
  indexYear <- which(paramNP$Inputs_reference.Year == times[1])
  crop =paramNP$Inputs_reference.Crop[indexYear]
  indexCrop <- which(paramNP$CropQUEFTS_Dry.crop == crop)
  rur_P <- paramNP$CropQUEFTS_Dry.rur_P[indexCrop] 
  rur_K <- paramNP$CropQUEFTS_Dry.rur_K[indexCrop] 
  
  #Uptake is from soluble pool
  #For 2500 kg maize yield, balanced uptake is:
  Pup0 <- Y0 / (0.5*(paramNP$CropQUEFTS_Dry.aP[indexCrop] 
                     + paramNP$CropQUEFTS_Dry.dP[indexCrop])) + paramNP$CropQUEFTS_Dry.rP[indexCrop]
  Kup0 <- Y0 / (0.5*(paramNP$CropQUEFTS_Dry.aK[indexCrop] 
                     + paramNP$CropQUEFTS_Dry.dK[indexCrop])) + paramNP$CropQUEFTS_Dry.rK[indexCrop]
  
  ini <- RCKP_estimate_initial_pools(param = paramNP, #All parameters
                                     Crop = crop,     #For extraction of crop=specific parameters
                                     Kupt_US = Kup0,  #K uptake from unfertilized soil #3 (uptake in NP treatment)
                                     Pupt_US = Pup0)
  
  #Determine pools after nseasons
  # statesNP = ode(ini, func = RCKP_fertilizer, times = times, parms = paramNP, #method="ode45",
  #                events = list(func = rootFertilizerApplication, root = TRUE),
  #                root = rootTrigger)
  # 
  # statesNK = ode(ini, func = RCKP_fertilizer, times = times, parms = paramNK, #method="ode45",
  #                events = list(func = rootFertilizerApplication, root = TRUE),
  #                root = rootTrigger)
  
  statesNP = ode(ini, func = RCKP_fertilizer, times = times, parms = paramNP, #method="ode45",
                 events = PrepareEventList(param = paramNP))
  
  statesNK = ode(ini, func = RCKP_fertilizer, times = times, parms = paramNK, #method="ode45",
                 events = PrepareEventList(param = paramNK))  
  
  #After n seasons, pools are no longer in balance.
  #Overestimation of pools
  #These are correction factors to adjust pools when first year with recorded uptake 
  #is available only after n seasons of the experiments
  nr <- nrow(statesNP)
  return( data.frame(Yield = Y0,
                     K_labile = statesNP[nr,"K_labile"] / statesNP[1,"K_labile"],
                     K_stable = statesNP[nr,"K_stable"] / statesNP[1,"K_stable"],
                     P_labile = statesNK[nr,"P_labile"] / statesNK[1,"P_labile"],
                     P_stable = statesNK[nr,"P_stable"] / statesNK[1,"P_stable"]))
}



initial_PK_pool_correction_factors <- function(paramNP, paramNK, times){
  DF <- NULL
  indexYear <- which(paramNP$Inputs_reference.Year == times[1])
  crop =paramNP$Inputs_reference.Crop[indexYear]
  if(crop == "wheat"){
    y0 <- seq(500, 4000, 500) #kg DM / ha
  }else if(crop == "maize"){
    y0 <- seq(500, 4000, 500) #kg DM / ha
  }else if(crop == "potato"){
    y0 <- seq(5000, 25000, 5000) #kg DM / ha
  }else{
    print(crop)
    stop("initial_PK_pool_correction_factors: not implemented for this crop")
  }
  #For monocropping system. Set all crops to this crop
  for (Y0 in y0){
    DF <- rbind(DF, PoolRatio(Y0, 
                              paramNP = paramNP, 
                              paramNK = paramNK, times))
  }
  return(DF)
}


NSE <- function(model_up, meas_up){
  nse <- 1 - sum((model_up - meas_up)^2, na.rm = TRUE) / sum((meas_up-mean(meas_up, na.rm = TRUE))^2, na.rm = TRUE)
  return(nse)
}  

RMSE <- function(model_up, meas_up){
  err <- model_up - meas_up
  rmse <- sqrt(mean(err^2,na.rm=TRUE))
  return(rmse)
}  
R2<-function(model_est, measured_val){
  SSE <- sum((model_est - measured_val)^2, na.rm=TRUE)
  MV <- mean(measured_val, na.rm=TRUE)
  SSV <- sum((measured_val - MV)^2, na.rm=TRUE)
  return(c(r2 = 1 - SSE / SSV))
}

#Meas vs predicted 
Write_Meas_Pred_NPK <- function(ScriptNr,
                                Dataset,
                                parListNames,
                                listStates = listStates,
                                UptN = out$UptN,
                                UptP = out$UptP,
                                UptK = out$UptK,
                                cols = cols,
                                pchs = pchs){
    MvP=NULL
    if(Dataset == "Siaya"){
      nr <- 0
      for(trt in parListNames){
        nr <- nr+1
        trtName <- paste0(trt,"_NONE")
        mvp <- data.frame(Dataset = Dataset, 
                          Season = UptN[,"SEASON"],
                          Crop = UptN[,"Crop"],
                          Treatment = trt,
                          Measured_N_Uptake.kg.ha = UptN[,trtName], 
                          Measured_P_Uptake.kg.ha = UptP[,trtName], 
                          Measured_K_Uptake.kg.ha = UptK[,trtName], 
                          Predicted_N_Uptake.kg.ha = UptN[,paste0("pred_N_UP_", trt)],
                          Predicted_P_Uptake.kg.ha = UptP[,paste0("pred_P_UP_", trt)],
                          Predicted_K_Uptake.kg.ha = UptK[,paste0("pred_K_UP_", trt)],
                          cols = cols[nr],
                          pchs = pchs[nr])  
        MvP <- rbind(MvP, mvp)
        nr1 <- 0
        for(Newtrt in parListNames[parListNames != "Control"]){
          nr1 <- nr1+1
          trtName <- paste0(trt,"_",Newtrt)
          mvp <- data.frame(Dataset = Dataset, 
                            Season = UptN[,"SEASON"],
                            Crop = UptN[,"Crop"],
                            Treatment = Newtrt,
                            Measured_N_Uptake.kg.ha = UptN[,trtName], 
                            Measured_P_Uptake.kg.ha = UptP[,trtName], 
                            Measured_K_Uptake.kg.ha = UptK[,trtName], 
                            Predicted_N_Uptake.kg.ha = UptN[,paste0("pred_N_UP_", trtName)],
                            Predicted_P_Uptake.kg.ha = UptP[,paste0("pred_P_UP_", trtName)],
                            Predicted_K_Uptake.kg.ha = UptK[,paste0("pred_K_UP_", trtName)],
                            cols = cols[nr],
                            pchs = pchs[nr])  
          MvP <- rbind(MvP, mvp)
        }
      }
    }else{
      nr <- 0
      for(trt in parListNames){
        nr <- nr+1
        dfS <- subset(listStates[[nr]], time %in% UptN[,"Year"])
        mvp <- data.frame(Dataset = Dataset, 
                          Season = UptN[,"Year"],
                          Crop = UptN[,"Crop"],
                          Treatment = trt,
                          Measured_N_Uptake.kg.ha = UptN[,trt], 
                          Measured_P_Uptake.kg.ha = UptP[,trt], 
                          Measured_K_Uptake.kg.ha = UptK[,trt], 
                          Predicted_N_Uptake.kg.ha = UptN[,paste0("pred_N_UP_", trt)],
                          Predicted_P_Uptake.kg.ha = UptP[,paste0("pred_P_UP_", trt)],
                          Predicted_K_Uptake.kg.ha = UptK[,paste0("pred_K_UP_", trt)],
                          P_labile = dfS[,"P_labile"],
                          P_stable = dfS[,"P_stable"],
                          P_labile_placed = dfS[,"P_labile_placed"],
                          P_labile_FYM = dfS[,"P_labile_FYM"],
                          K_labile = dfS[,"K_labile"],
                          K_stable = dfS[,"K_stable"],
                          K_labile_FYM = dfS[,"K_labile_FYM"],
                          cols = cols[nr],
                          pchs = pchs[nr])  
        MvP <- rbind(MvP, mvp)
      }
    }
    write.csv(MvP, paste0("..//Results//",ScriptNr,". ",Dataset,"_measured_predicted_NPKuptake.csv"))
}



#For testing!!
# param = RCKP_param(type= "ASP",
#                    Inputs_reference = data.frame(Crop = rep(c("potato","oats","wheat"),10),
#                                                  Year = seq(1981,2010,1),
#                                                  Pfert = rep(c(40,30,30),10),
#                                                  Pfert_placed = rep(c(0,0,0),10),
#                                                  Nfert = rep(c(40,30,30),10),
#                                                  Kfert = rep(c(150,50,50),10),
#                                                  FYMN = rep(c(100,50,50),10), 
#                                                  FYMP = rep(c(40,20,20),10), 
#                                                  FYMK = rep(c(120,60,60),10),
#                                                  Nrec = 0.5,
#                                                  FYMNrec = 0.3,
#                                                  Nrec_FYM = 0.4,
#                                                  Nuptake_kgha = rep(c(40,30,30),10)),
# 
#                    Crops = c("potato","oats","wheat"),
#                    CalSetName = "Hanninghof",
#                    UseMeasuredNUptake = FALSE)
# paramNK <- param
# paramNK$Inputs_reference.Pfert <- rep(0,30)
# paramNK$Inputs_reference.FYMN <- rep(0,30)
# paramNK$Inputs_reference.FYMP <- rep(0,30)
# paramNK$Inputs_reference.FYMK <- rep(0,30)
# paramNP <- param
# paramNK$Inputs_reference.Kfert <- rep(0,30)
# paramNK$Inputs_reference.FYMN <- rep(0,30)
# paramNK$Inputs_reference.FYMP <- rep(0,30)
# paramNK$Inputs_reference.FYMK <- rep(0,30)
# 
# NSEASONS <- 7
# DF <- initial_PK_pool_correction_factors(paramNP = paramNP, paramNK = paramNK, times = seq(1981,1981+NSEASONS,1))
# 
# #Show pool correction factors
# plot(DF[,"Yield"], DF[,"K_labile"], type="l",
#      xlab="Yield", ylab=paste0("Fraction of initial pool after ", NSEASONS," seasons"), ylim=c(0.8,1.8),
#      col="red", lty=1, main=paste0(NSEASONS, " seasons of NP or NK"))
# lines(DF[,"Yield"], DF[,"K_stable"], col="black", lty=1)
# lines(DF[,"Yield"], DF[,"P_labile"], col="cyan", lty=2)
# lines(DF[,"Yield"], DF[,"P_stable"], col="blue", lty=2)
# 
# 
# #TESTING
# ini <- RCKP_estimate_initial_pools(param = param,      #All parameters
#                                    Crop = "potato",           #For extraction of crop=specific parameters
#                                    Kupt_US = 60,  #K uptake from unfertilized soil #3 (uptake in NP treatment)
#                                    Pupt_US = 10)
# 
# #Run for n years
# states <- ode(ini,seq(1981, 2010, by = 0.1), RCKP_fertilizer, param,  #method = "ode45",
#                 events = PrepareEventList(param, fyear_fert = 74/365, fyear_FYM = 1/365))
# #Determine and show balances
# states_MB <- RCKP_MassBalance(iniStates = states[1,], States = states)
