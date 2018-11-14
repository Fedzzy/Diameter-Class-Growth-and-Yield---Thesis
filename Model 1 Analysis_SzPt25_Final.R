#======================Regression Analysis of Different Varibles - Model 1 - SzPt25=======================

#Loading in Sourced Functions
source(file="Compiled Analysis Functions.R")

#Load in Plot Level Parameter Data.Rdata
load(file="Plot Level Parameter Data.RData")

#========================================Nonlinear Regression for AMD - Model 1

AMD_Mod1<-nls(AMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt25ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))

summary(AMD_Mod1)

#Prediction
SzPt25ParamDat$AMD_Mod1<-predict(AMD_Mod1)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$AMDat, SzPt25ParamDat$AMD_Mod1, 4)


#========================================Nonlinear Regression for QMD - Model 1

QMD_Mod1<-nls(QMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt25ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(QMD_Mod1)

#Prediction
SzPt25ParamDat$QMD_Mod1<-predict(QMD_Mod1)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$QMDat, SzPt25ParamDat$QMD_Mod1, 4)


#========================================Nonlinear Regression for D0 - Model 1

D0_Mod1<-nls(D0~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
             
             data=(SzPt25ParamDat),
             
             start=list(b0=105, b1=-0.2, b2=-10, b3=.006))

#Obtaining summary of results
summary(D0_Mod1)  #RMSE 1.886

#Prediction
SzPt25ParamDat$D0_Mod1<-predict(D0_Mod1)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D0, SzPt25ParamDat$D0_Mod1, 4)

#<><><><><><><><><><>Testing alternative models for prediction dbhMin<><><><><><><><><><><><><><><><><>

#This is being tested to try and reduce the high error associated with dbhMin prediction for Model 1

#Running alternative model 1 for dbhMin prediciton
D0_E1<-nls(D0~b0*AMDat^b1*exp(b2*(1/Age)),
           
           data=(SzPt25ParamDat),
           
           start=list(b0=.035, b1=1.826, b2=.774))

#Obtaining summary of results
summary(D0_E1) #RMSE found to be 1.716

#=======================================================================================

#Running alternative model 2 for dbhMin prediction
D0_E2<-nls(D0~b0*QMDat^b1*exp(b2*(1/Age)),
           
           data=(SzPt25ParamDat),
           
           start=list(b0=.035, b1=1.826, b2=.416))

#Obtaining summary of results
summary(D0_E2)  #RMSE was found to be 1.836


#Alternative model 1(AMD) was found to yield the lowest RMSE of the three 


#========================================Nonlinear Regression for D25 - Model 1

D25_Mod1<-nls(D25~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt25ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D25_Mod1)

#Prediction
SzPt25ParamDat$D25_Mod1<-predict(D25_Mod1)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D25, SzPt25ParamDat$D25_Mod1, 4)

#========================================Nonlinear Regression for D50 - Model 1

D50_Mod1<-nls(D50~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt25ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D50_Mod1)

#Prediction
SzPt25ParamDat$D50_Mod1<-predict(D50_Mod1)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D50, SzPt25ParamDat$D50_Mod1, 4)

#========================================Nonlinear Regression for D93 - Model 1

D93_Mod1<-nls(D93~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))),
              
              data=(SzPt25ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10))


summary(D93_Mod1)

#Prediction
SzPt25ParamDat$D93_Mod1<-predict(D93_Mod1)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D93, SzPt25ParamDat$D93_Mod1, 3)

#========================================Nonlinear Regression for D95 - Model 1

D95_Mod1<-nls(D95~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))),
              
              data=(SzPt25ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10))


summary(D95_Mod1)

#Prediction
SzPt25ParamDat$D95_Mod1<-predict(D95_Mod1)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D95, SzPt25ParamDat$D95_Mod1, 3)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#========================================================================================

#Separating Plot Data for Further Analysis

#Get column subsets
PlotSz25<-SzPt25ParamDat[,c("MeasmtObs","Age","DomHtM","TPH","RS",
                            "AMD_Mod1", "QMD_Mod1","D0_Mod1","D25_Mod1","D50_Mod1","D93_Mod1",
                            "D95_Mod1")]



save(PlotSz25,file="K:/csabatia/GraduateStudentWork/JoshBankston/Data Analysis/Distribution Recovery Analyses/Model 1_Cao2004/PlotLevelAnalysis25.RData")
