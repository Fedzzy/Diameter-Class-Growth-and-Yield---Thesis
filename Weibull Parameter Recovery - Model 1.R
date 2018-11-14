#Weibull Data Analysis for - Model 1

#Loading in plyr() from library
library(plyr)

#Loading in needed functions
source(file="WeibullParametersEstimation.R")
source(file="WeibullProportions.R")

#Loading in needed datasets
load(file="Pred_Mod1_PS05.Rdata")
load(file="Pred_Mod1_PS10.Rdata")
load(file="Pred_Mod1_PS15.Rdata")
load(file="Pred_Mod1_PS20.Rdata")
load(file="Pred_Mod1_PS25.Rdata")

#Assigning plot sizes to datasets in order to run them all at once through ddply
PM1_PS05$PS<-.05
PM1_PS10$PS<-.10
PM1_PS15$PS<-.15
PM1_PS20$PS<-.20
PM1_PS25$PS<-.25

#Combining all datasets into one object to run through ddply
AllDat<-rbind(PM1_PS05, PM1_PS10, PM1_PS15, PM1_PS20, PM1_PS25)

AllDat2<-AllDat[order(AllDat$MeasmtObs,AllDat$PS),]

#Using ddply to calculate weibull parameters based on predicted values previously acquired
WeiDat<-ddply(AllDat2, .(MeasmtObs, PS), summarize,
              
              Age=min(Age),
              
              DomHtM=min(DomHtM),
              
              TPH=min(TPH),
              
              RS=min(RS),
              
              NumTrees=min(NumTrees),
              
              dbhMin=min(D0_Mod1),
              
              #Moments Recovery
              
              PWM_A=WeiMomentA(D0_Mod1),
              
              PWM_B=WeiMomentB(D0_Mod1, AMD_Mod1, QMD_Mod1),
              
              PWM_C=WeiMomentC(D0_Mod1, AMD_Mod1, QMD_Mod1),
              
              #Percentiles Recovery
              
              PWP_A=WeiPctleA(NumTrees, D0_Mod1, D50_Mod1),
              
              PWP_B=WeiPctleB(NumTrees, D0_Mod1, D25_Mod1, D50_Mod1, D95_Mod1, QMD_Mod1),
              
              PWP_C=WeiPctleC(NumTrees, D0_Mod1, D25_Mod1, D50_Mod1, D95_Mod1),
              
              #Moment-Percentile Hybrid Recovery
              
              PWH_A=WeiHyA(D0_Mod1),
              
              PWH_B=WeiHyB(D0_Mod1, D93_Mod1, QMD_Mod1),
              
              PWH_C=WeiHyC(D0_Mod1, D93_Mod1, QMD_Mod1) 
              )

#Separate the plot sizes into different data sets
PWM1_05<-subset(WeiDat, PS == .05)
PWM1_10<-subset(WeiDat, PS == .10)
PWM1_15<-subset(WeiDat, PS == .15)
PWM1_20<-subset(WeiDat, PS == .20)
PWM1_25<-subset(WeiDat, PS == .25)

AllPlotSizeDat<-WeiDat

save(AllPlotSizeDat,PWM1_05, PWM1_10, PWM1_15, PWM1_20, PWM1_25, file="Model 1 Predicted Weibull Parameter Data.Rdata")
