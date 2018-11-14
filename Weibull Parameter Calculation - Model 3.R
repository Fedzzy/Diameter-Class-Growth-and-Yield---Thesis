#Weibull Data Analysis for - Model 2

#Loading in plyr() from library
library(plyr)

#Loading in needed functions
source(file="WeibullParametersEstimation.R")
source(file="WeibullProportions.R")

#Loading in needed datasets
load(file="Pred_Mod3_PS05.Rdata") 
load(file="Pred_Mod3_PS10.Rdata") 
load(file="Pred_Mod3_PS15.Rdata") 
load(file="Pred_Mod3_PS20.Rdata") 
load(file="Pred_Mod3_PS25.Rdata") 

#Assigning plot sizes to datasets in order to run them all at once through ddply
PM3_PS05$PS<-.05
PM3_PS10$PS<-.10
PM3_PS15$PS<-.15
PM3_PS20$PS<-.20
PM3_PS25$PS<-.25

#Combining all datasets into one object to run through ddply
AllDat<-rbind(PM3_PS05, PM3_PS10, PM3_PS15, PM3_PS20, PM3_PS25)

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
PWM3_05<-subset(WeiDat, PS == .05)
PWM3_10<-subset(WeiDat, PS == .10)
PWM3_15<-subset(WeiDat, PS == .15)
PWM3_20<-subset(WeiDat, PS == .20)
PWM3_25<-subset(WeiDat, PS == .25)

save(WeiDat, PWM3_05, PWM3_10, PWM3_15, PWM3_20, PWM3_25, file="Model 3 Predicted Weibull Parameter Data.Rdata")
