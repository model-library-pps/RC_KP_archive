#Compile yields, nutrient content and nutrient uptakes in LR 2016 and LR 2018 season
#From this, we will model K pools in the NP and NPK plots
#This will allow assessment of changes in K pools in fertilized and unfertilized plots
require(lme4)
require(lmerTest)

##INPUT#################
#load yield data
lr16yd<-read.csv("..//Data//Njoroge NOT LR2016 HARVEST DATA_DM.csv")
#from restoration plots
lr18yd<-read.csv("..//Data//Njoroge NOT LR2018 Restored_DM.csv")
#From subplots
lr18ydsub<-read.csv("..//Data//Njoroge NOT LR2018 subplots_DM.csv")
#load plant nutrient data
nut<- read.csv("..//Data//Njoroge Grain and stover data.csv")
#Add management data
FieldMan<-read.csv("..//Data//Njoroge NOT Field.csv")
##INPUT#################

##OUPUT#################
outfile <- "..//Data//Processed//6. Njoroge dataset. Yield stover nutrient contents and uptake.csv"
outpdf <- "..//Results//6. Njoroge dataset fitted linear models to N P K uptakes.pdf"
##OUPUT#################


# colnames(Ydata)[colnames(Ydata)=="NEWTREATMENT"] <- "NEW.TREATMENT"
# fit <- lm(yield.t.AIRDRY.ha.1 ~ FARMCODE + SEASON * TREATMENT, Ydata[Ydata[,"YEAR"] <= 2016, ])
# sfit <- summary(fit)
# fit2 <- lm(yield.t.AIRDRY.ha.1 ~ FARMCODE + TREATMENT + SEASON * NEW.TREATMENT, Ydata[Ydata[,"YEAR"] > 2016, ])
# sfit2 <- summary(fit2)


lr16yd[,"NEW.TREATMENT"] <- "NONE"
lr16yd<- subset(lr16yd,select = c("Farm.CODE","PLOTCODE","TREATMENT","NEW.TREATMENT","Season","yield.t.ha","stov.t.ha")) 
colnames(lr16yd)[colnames(lr16yd)=="Season"]<-"SEASON"

lr18yd<- subset(lr18yd,select = c("Farm.CODE","PLOTCODE","TREATMENT","NEW.TREATMENT","Season","yield.t.ha","stov.t.ha")) 
colnames(lr18yd)[colnames(lr18yd)=="Season"]<-"SEASON"

#select NP and NPK plots with NPK or with NP
#lr18ydsub<- subset(lr18ydsub,NEW.TREATMENT=="NPK" | NEW.TREATMENT=="NP",
#                   select = c("Farm.CODE","PLOT.CODE","TREATMENT","NEW.TREATMENT","Season","yield.t.ha","stov.t.ha")) 

lr18ydsub<- subset(lr18ydsub, select = c("Farm.CODE","PLOT.CODE","TREATMENT","NEW.TREATMENT","Season","yield.t.ha","stov.t.ha")) 
colnames(lr18ydsub)[colnames(lr18ydsub)=="Season"]<-"SEASON"
colnames(lr18ydsub)[colnames(lr18ydsub)=="PLOT.CODE"]<-"PLOTCODE"

#Add the datasets from the NOT trials, the restoration trials and the subplot trials
lr1618yd<-rbind(lr16yd,lr18yd,lr18ydsub)
colnames(lr1618yd)[colnames(lr1618yd) == "Farm.CODE"] <- "FARMCODE"

#Select only relevant variables and select LR 2016 season (LR 2016)
#names(nut)
nut<- subset(nut,select = c("COMPONENT","SEASON","PLOTCODE","N","P","K","S","MG","CA","MN","B","CU","MO","FE","ZN")) 
nut1618<-subset(nut,SEASON=='LR2016' | SEASON=='LR2018')

gnut<-subset(nut1618,COMPONENT=='GRAIN')
stnut<-subset(nut1618,COMPONENT=='STOVER')

UptDatag<-merge(lr1618yd,gnut,by=c("SEASON","PLOTCODE"))
UptDatas<-merge(lr1618yd,stnut,by=c("SEASON","PLOTCODE"))





#Determine uptakes 
#                   kg/ha   kg/t        t airdry/ha       g DM/g airdry   g/100g      100g/g                 
UptDatag[,"N_GrainUptake.kg.ha"]<-1000  * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"N"]/100) #as % or g/100 g
UptDatag[,"P_GrainUptake.kg.ha"]<-1000  * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"P"]/100)
UptDatag[,"K_GrainUptake.kg.ha"]<-1000  * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"K"]/100)
UptDatag[,"S_GrainUptake.kg.ha"]<-1000  * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"S"]/100)
UptDatag[,"Mg_GrainUptake.kg.ha"]<-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"MG"]/100)
UptDatag[,"Ca_GrainUptake.kg.ha"]<-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"CA"]/100)
#                   kg/ha   kg/t        t airdry/ha       g DM/g airdry   mg/kg      kg/mg                 
UptDatag[,"Mn_GrainUptake.kg.ha"]<-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"MN"]/1E6) #in ppm, or mg/kg
UptDatag[,"B_GrainUptake.kg.ha"] <-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"B"] /1E6)
UptDatag[,"Cu_GrainUptake.kg.ha"]<-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"CU"]/1E6)
UptDatag[,"Mo_GrainUptake.kg.ha"]<-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"MO"]/1E6)
UptDatag[,"Fe_GrainUptake.kg.ha"]<-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"FE"]/1E6)
UptDatag[,"Zn_GrainUptake.kg.ha"]<-1000 * UptDatag[,"yield.t.ha"] * 88/100 * (UptDatag[,"ZN"]/1E6)

UptDatas[,"N_StovUptake.kg.ha"]<- 1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"N"]/100)
UptDatas[,"P_StovUptake.kg.ha"]<- 1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"P"]/100)
UptDatas[,"K_StovUptake.kg.ha"]<- 1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"K"]/100)
UptDatas[,"S_StovUptake.kg.ha"]<- 1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"S"]/100)
UptDatas[,"Mg_StovUptake.kg.ha"]<-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"MG"]/100)
UptDatas[,"Ca_StovUptake.kg.ha"]<-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"CA"]/100)
UptDatas[,"Mn_StovUptake.kg.ha"]<-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"MN"]/1E6)
UptDatas[,"B_StovUptake.kg.ha"] <-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"B"] /1E6)
UptDatas[,"Cu_StovUptake.kg.ha"]<-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"CU"]/1E6)
UptDatas[,"Mo_StovUptake.kg.ha"]<-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"MO"]/1E6)
UptDatas[,"Fe_StovUptake.kg.ha"]<-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"FE"]/1E6)
UptDatas[,"Zn_StovUptake.kg.ha"]<-1000 * UptDatas[,"stov.t.ha"] * 88/100 * (UptDatas[,"ZN"]/1E6)

#Select required variables and merge files for total uptake calculation
names(UptDatag)
names(UptDatas)
UptDatag<- subset(UptDatag,select = c("FARMCODE","PLOTCODE","TREATMENT","NEW.TREATMENT","SEASON","yield.t.ha",
                                      "N_GrainUptake.kg.ha","P_GrainUptake.kg.ha","K_GrainUptake.kg.ha",
                                      "S_GrainUptake.kg.ha","Mg_GrainUptake.kg.ha","Ca_GrainUptake.kg.ha",
                                      "Mn_GrainUptake.kg.ha","B_GrainUptake.kg.ha","Cu_GrainUptake.kg.ha",
                                      "Mo_GrainUptake.kg.ha","Fe_GrainUptake.kg.ha","Zn_GrainUptake.kg.ha")) 
UptDatas<- subset(UptDatas,select = c("FARMCODE","PLOTCODE","TREATMENT","NEW.TREATMENT","SEASON","stov.t.ha",
                                      "N_StovUptake.kg.ha","P_StovUptake.kg.ha","K_StovUptake.kg.ha",
                                      "S_StovUptake.kg.ha","Mg_StovUptake.kg.ha","Ca_StovUptake.kg.ha",
                                      "Mn_StovUptake.kg.ha","B_StovUptake.kg.ha","Cu_StovUptake.kg.ha",
                                      "Mo_StovUptake.kg.ha","Fe_StovUptake.kg.ha","Zn_StovUptake.kg.ha")) 
UptData<-merge(UptDatag,UptDatas,by=c("FARMCODE","PLOTCODE","TREATMENT","NEW.TREATMENT","SEASON"))
#Rename column
#colnames(UptData)[colnames(UptData) == "Farm.CODE"]<-"FARMCODE"

UptData[,"N_Uptake.kg.ha"] = UptData[,"N_StovUptake.kg.ha"]  + UptData[,"N_GrainUptake.kg.ha"]
UptData[,"P_Uptake.kg.ha"] = UptData[,"P_StovUptake.kg.ha"]  + UptData[,"P_GrainUptake.kg.ha"]
UptData[,"K_Uptake.kg.ha"] = UptData[,"K_StovUptake.kg.ha"]  + UptData[,"K_GrainUptake.kg.ha"]
UptData[,"S_Uptake.kg.ha"] = UptData[,"S_StovUptake.kg.ha"]  + UptData[,"S_GrainUptake.kg.ha"]
UptData[,"Mg_Uptake.kg.ha"]= UptData[,"Mg_StovUptake.kg.ha"] + UptData[,"Mg_GrainUptake.kg.ha"]
UptData[,"Ca_Uptake.kg.ha"]= UptData[,"Ca_StovUptake.kg.ha"] + UptData[,"Ca_GrainUptake.kg.ha"]
UptData[,"Mn_Uptake.kg.ha"]= UptData[,"Mn_StovUptake.kg.ha"] + UptData[,"Mn_GrainUptake.kg.ha"]
UptData[,"B_Uptake.kg.ha"] = UptData[,"B_StovUptake.kg.ha"]  + UptData[,"B_GrainUptake.kg.ha"]
UptData[,"Cu_Uptake.kg.ha"]= UptData[,"Cu_StovUptake.kg.ha"] + UptData[,"Cu_GrainUptake.kg.ha"]
UptData[,"Mo_Uptake.kg.ha"]= UptData[,"Mo_StovUptake.kg.ha"] + UptData[,"Mo_GrainUptake.kg.ha"]
UptData[,"Fe_Uptake.kg.ha"]= UptData[,"Fe_StovUptake.kg.ha"] + UptData[,"Fe_GrainUptake.kg.ha"]
UptData[,"Zn_Uptake.kg.ha"]= UptData[,"Zn_StovUptake.kg.ha"] + UptData[,"Zn_GrainUptake.kg.ha"]


UptData<-merge(UptData, subset(FieldMan,select=c("FARMCODE","Man.freq", "Man.Use", "Type")),by="FARMCODE")



#make different columns for old and new plotnames
for(i in 1:nrow(UptData)){
  s<-unlist(strsplit(as.character(UptData[i,"PLOTCODE"]),"[/]"))
  if(length(s) == 1){#No subplot code
    p<-unlist(strsplit(as.character(s[1]),"[K]"))
    if(length(p) == 2){#FARMCODE "K" PLOT
      UptData[i,"PLOT"]<-paste0("K",p[2])
    }else{#FARMCODE PLOT, replace FARMCODE by K
      UptData[i,"PLOT"]<-sub(UptData[i,"FARMCODE"],"K",p[1])
    }
    UptData[i,"NEWPLOT"]<-NA
  }
  else if(length(s) == 2){#including subplot code
    p<-unlist(strsplit(as.character(s[1]),"[K]"))
    if(length(p) == 2){#No subplot code
      UptData[i,"PLOT"]<-paste0("K",p[2])
    }else{#FARMCODE PLOT, replace FARMCODE by K
      UptData[i,"PLOT"]<-sub(UptData[i,"FARMCODE"],"K",p[1])
    }
    UptData[i,"NEWPLOT"]<-s[2]
  }else{
    UptData[i,"PLOT"]<-"NA"
    UptData[i,"NEWPLOT"]<-"NA"
  }
}

#Add trial type. Field yet without a new treatment are part of the NOT trial.
#All fields where a new treatment with NP, PK or NK are NOT-SUBPLOTS, others are NOT-RESTAURATION

uFarm<- unique(UptData[,"FARMCODE"])
UptData[,"TRIALTYPE"] <- "NONE"
for (uf in uFarm){
  #All fields are NOT until LR2016
  ii <- which(UptData[,"FARMCODE"] == uf)
  UptData[ii,"TRIALTYPE"]<-"NOT"
  jj <- which(UptData[,"FARMCODE"] == uf & UptData[,"NEW.TREATMENT"] == "NPK")
  if(length(jj)> 0){
    #Assign this farm to the NOT-RESTAURATION in phase 2 following the NOT phase 1
    UptData[ii,"TRIALTYPE"] <- "NOT-RESTAURATION"
  }
  kk <- which(UptData[,"FARMCODE"] == uf & UptData[,"NEW.TREATMENT"] == "NP")
  if(length(kk) > 0){
    #Assign this farm to the NOT-SUBPLOTS in phase 2 following the NOT phase 1
    UptData[ii,"TRIALTYPE"] <-"NOT-SUBPLOTS"
  }
  
}

fitLMmix <- function(fit, UptData, colname, season){
  print(paste0(season, " ", colname))
  print(summary(fit))
  print(anova(fit))
  df<-data.frame(x=UptData[,colname],y=predict(fit, UptData))
  xymin=min(c(df[,"x"],df[,"y"]))
  xymax=max(c(df[,"x"],df[,"y"]))
  plot(df[,"x"], df[,"y"],type="p", col="black", xlim=c(xymin,xymax),  ylim=c(xymin,xymax), main=paste0(season," ",colname))
  abline(0,1,col="black")

  
  fit <- lm(y ~ 1 + x, df)
  sfit<-summary(fit)
  abline(sfit$coefficients[1],sfit$coefficients[2],col="red")
  text(x=xymin + 0.5*(xymax-xymin), y = xymin + 0.05 * (xymax -xymin), 
       labels=paste0("y = ",round(sfit$coefficients[1],1),
                     " + ",round(sfit$coefficients[2],2),
                     " x; R2=", round(sfit$r.squared,2)))
  varexpl <- R2(model_est = df[,"y"], measured_val = df[,"x"])                                 

  text(x=xymin + 0.5*(xymax-xymin), y = xymin + 0.1 * (xymax -xymin), 
       labels=paste0("Var. expl: ", round(varexpl,2)))
  return(df[,"y"])
}

#Fit a mixed LMs to remove noise
pdf(file = outpdf)
  par(mfrow=c(2,3),mar=c(2,2,1,0), oma=c(2,2,0.5,0.5))
  for(season in c("LR2016","LR2018")){
    ii<-which(UptData[,"SEASON"] == season & !is.na(UptData[,"N_Uptake.kg.ha"]))
    if( length(unique(UptData[ii,"NEW.TREATMENT"])) == 1){
      fitN <- lmer(N_Uptake.kg.ha ~1 + TREATMENT*Man.Use + (1|FARMCODE), UptData[ii,])
    }else{
      fitN <- lmer(N_Uptake.kg.ha ~1 + TREATMENT*Man.Use + NEW.TREATMENT + (1|FARMCODE), UptData[ii,])
    }
 
    UptData[ii,"LMER.N_Uptake.kg.ha"] <- fitLMmix(fit = fitN, 
                                             UptData = UptData[ii,], 
                                             colname = "N_Uptake.kg.ha",
                                             season = season)
    
    ii<-which(UptData[,"SEASON"] == season & !is.na(UptData[,"P_Uptake.kg.ha"]))
    if( length(unique(UptData[ii,"NEW.TREATMENT"]))== 1){
      fitP <- lmer(P_Uptake.kg.ha ~1 + TREATMENT*Man.Use + (1|FARMCODE), UptData[ii,])
    }else{
      fitP <- lmer(P_Uptake.kg.ha ~1 + TREATMENT*Man.Use + NEW.TREATMENT + (1|FARMCODE), UptData[ii,])
    }    

    UptData[ii,"LMER.P_Uptake.kg.ha"] <- fitLMmix(fit = fitP, 
                                             UptData = UptData[ii,], 
                                             colname = "P_Uptake.kg.ha",
                                             season = season)
    
    ii<-which(UptData[,"SEASON"] == season & !is.na(UptData[,"K_Uptake.kg.ha"]))
    if( length(unique(UptData[ii,"NEW.TREATMENT"])) == 1){
      fitK <- lmer(K_Uptake.kg.ha ~1 + TREATMENT*Man.Use + (1|FARMCODE), UptData[ii,])
    }else{
      fitK <- lmer(K_Uptake.kg.ha ~1 + TREATMENT*Man.Use + NEW.TREATMENT + (1|FARMCODE), UptData[ii,])
    }    

    UptData[ii,"LMER.K_Uptake.kg.ha"] <- fitLMmix(fit = fitK, 
                                             UptData = UptData[ii,], 
                                             colname = "K_Uptake.kg.ha",
                                             season = season)
  }    
dev.off()


#Fit a mixed LMs to remove noise
pdf(file = outpdf)
par(mfrow=c(2,3),mar=c(2,2,1,0), oma=c(2,2,0.5,0.5))
for(season in c("LR2016","LR2018")){
  ii<-which(UptData[,"SEASON"] == season & !is.na(UptData[,"N_Uptake.kg.ha"]))
  if( length(unique(UptData[ii,"NEW.TREATMENT"])) == 1){
    fitN <- lmer(N_Uptake.kg.ha ~1 + TREATMENT*Man.Use + (1|FARMCODE), UptData[ii,])
  }else{
    fitN <- lmer(N_Uptake.kg.ha ~1 + TREATMENT*Man.Use + NEW.TREATMENT + (1|FARMCODE), UptData[ii,])
  }
  
  UptData[ii,"LMER.N_Uptake.kg.ha"] <- fitLMmix(fit = fitN, 
                                                UptData = UptData[ii,], 
                                                colname = "N_Uptake.kg.ha",
                                                season = season)
  
  ii<-which(UptData[,"SEASON"] == season & !is.na(UptData[,"P_Uptake.kg.ha"]))
  if( length(unique(UptData[ii,"NEW.TREATMENT"]))== 1){
    fitP <- lmer(P_Uptake.kg.ha ~1 + TREATMENT*Man.Use + (1|FARMCODE), UptData[ii,])
  }else{
    fitP <- lmer(P_Uptake.kg.ha ~1 + TREATMENT*Man.Use + NEW.TREATMENT + (1|FARMCODE), UptData[ii,])
  }    
  
  UptData[ii,"LMER.P_Uptake.kg.ha"] <- fitLMmix(fit = fitP, 
                                                UptData = UptData[ii,], 
                                                colname = "P_Uptake.kg.ha",
                                                season = season)
  
  ii<-which(UptData[,"SEASON"] == season & !is.na(UptData[,"K_Uptake.kg.ha"]))
  if( length(unique(UptData[ii,"NEW.TREATMENT"])) == 1){
    fitK <- lmer(K_Uptake.kg.ha ~1 + TREATMENT*Man.Use + (1|FARMCODE), UptData[ii,])
  }else{
    fitK <- lmer(K_Uptake.kg.ha ~1 + TREATMENT*Man.Use + NEW.TREATMENT + (1|FARMCODE), UptData[ii,])
  }    
  
  UptData[ii,"LMER.K_Uptake.kg.ha"] <- fitLMmix(fit = fitK, 
                                                UptData = UptData[ii,], 
                                                colname = "K_Uptake.kg.ha",
                                                season = season)
}    
dev.off()

#Print overall percentage of variation explained by models
print(c(var.expl_N = R2(measured_val = UptData[,"N_Uptake.kg.ha"], model_est = UptData[,"LMER.N_Uptake.kg.ha"]),
        var.expl_P = R2(measured_val = UptData[,"P_Uptake.kg.ha"], model_est = UptData[,"LMER.P_Uptake.kg.ha"]),
        var.expl_K = R2(measured_val = UptData[,"K_Uptake.kg.ha"], model_est = UptData[,"LMER.K_Uptake.kg.ha"])))

write.csv(UptData, outfile)
