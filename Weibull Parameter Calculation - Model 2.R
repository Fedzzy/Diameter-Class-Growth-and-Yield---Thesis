#Weibull Data Analysis for - Model 2

#Loading in plyr() from library
library(plyr)

#Loading in needed functions
source(file="WeibullParametersEstimation.R")
source(file="WeibullProportions.R")

#Loading in needed datasets
load(file="Pred_Mod2_PS05.Rdata") 
load(file="Pred_Mod2_PS10.Rdata") 
load(file="Pred_Mod2_PS15.Rdata") 
load(file="Pred_Mod2_PS20.Rdata") 
load(file="Pred_Mod2_PS25.Rdata") 

#Assigning plot sizes to datasets in order to run them all at once through ddply
PM2_PS05$PS<-.05
PM2_PS10$PS<-.10
PM2_PS15$PS<-.15
PM2_PS20$PS<-.20
PM2_PS25$PS<-.25

#Combining all datasets into one object to run through ddply
AllDat<-rbind(PM2_PS05, PM2_PS10, PM2_PS15, PM2_PS20, PM2_PS25)

#Using ddply to calculate weibull parameters based on predicted values previously acquired
WeiDat<-ddply(AllDat, .(MeasmtObs, PS), summarize,
              
              Age=min(Age),
              
              DomHtM=min(DomHtM),
              
              TPH=min(TPH),
              
              NumTrees=min(NumTrees),
              
              RS=min(RS),
              
              dbhMin=min(Pred_D0),
              
              #Moments Recovery
              
              PWM_A=WeiMomentA(Pred_D0),
              
              PWM_B=WeiMomentB(Pred_D0, Pred_AMD, Pred_QMD),
              
              PWM_C=WeiMomentC(Pred_D0, Pred_AMD, Pred_QMD),
              
              #Percentiles Recovery
              
              PWP_A=WeiPctleA(NumTrees, Pred_D0, Pred_D50),
              
              PWP_B=WeiPctleB(NumTrees, Pred_D0, Pred_D25, Pred_D50, Pred_D95, Pred_QMD),
              
              PWP_C=WeiPctleC(NumTrees, Pred_D0, Pred_D25, Pred_D50, Pred_D95),
              
              #Moment-Percentile Hybrid Recovery
              
              PWH_A=WeiHyA(Pred_D0),
              
              PWH_B=WeiHyB(Pred_D0, Pred_D93, Pred_QMD),
              
              PWH_C=WeiHyC(Pred_D0, Pred_D93, Pred_QMD))

#Subsetting Data to create individual objects
PWM2_05<-subset(WeiDat, PS == .05)
PWM2_10<-subset(WeiDat, PS == .10)
PWM2_15<-subset(WeiDat, PS == .15)
PWM2_20<-subset(WeiDat, PS == .20)
PWM2_25<-subset(WeiDat, PS == .25)

save(WeiDat, PWM2_05, PWM2_10, PWM2_15, PWM2_20, PWM2_25, file="Model 2 Predicted Weibull Parameter Data.Rdata")
