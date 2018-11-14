#======================Regression Analysis of Different Varibles - Model 1 - SzPt10=======================

#Loading in Sourced Functions
source(file="K:/csabatia/GraduateStudentWork/JoshBankston/Regression Analysis/Compiled Analysis Functions.R")

#Load in Plot Level Parameter Data.Rdata
load(file="K:/csabatia/GraduateStudentWork/JoshBankston/Regression Analysis/Plot Level Parameter Data.RData")

#========================================Nonlinear Regression for AMD - Model 1

AMD_Mod1<-nls(AMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt10ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))

summary(AMD_Mod1)

#Prediction
SzPt10ParamDat$AMD_Mod1<-predict(AMD_Mod1)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$AMDat, SzPt10ParamDat$AMD_Mod1, 4)


#========================================Nonlinear Regression for QMD - Model 1

QMD_Mod1<-nls(QMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt10ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(QMD_Mod1)

#Prediction
SzPt10ParamDat$QMD_Mod1<-predict(QMD_Mod1)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$QMDat, SzPt10ParamDat$QMD_Mod1, 4)


#========================================Nonlinear Regression for D0 - Model 1

D0_Mod1<-nls(D0~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
             
             data=(SzPt10ParamDat),
             
             start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D0_Mod1)

#Prediction
SzPt10ParamDat$D0_Mod1<-predict(D0_Mod1)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D0, SzPt10ParamDat$D0_Mod1, 4)

#========================================Nonlinear Regression for D25 - Model 1

D25_Mod1<-nls(D25~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt10ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D25_Mod1)

#Prediction
SzPt10ParamDat$D25_Mod1<-predict(D25_Mod1)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D25, SzPt10ParamDat$D25_Mod1, 4)

#========================================Nonlinear Regression for D50 - Model 1

D50_Mod1<-nls(D50~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt10ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D50_Mod1)

#Prediction
SzPt10ParamDat$D50_Mod1<-predict(D50_Mod1)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D50, SzPt10ParamDat$D50_Mod1, 4)

#========================================Nonlinear Regression for D93 - Model 1

D93_Mod1<-nls(D93~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt10ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D93_Mod1)

#Prediction
SzPt10ParamDat$D93_Mod1<-predict(D93_Mod1)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D93, SzPt10ParamDat$D93_Mod1, 4)

#========================================Nonlinear Regression for D95 - Model 1

D95_Mod1<-nls(D95~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt10ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D95_Mod1)

#Prediction
SzPt10ParamDat$D95_Mod1<-predict(D95_Mod1)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D95, SzPt10ParamDat$D95_Mod1, 4)
