#Non-Linear Regression and Prediction for Model 3 - SzPt20

#Loading in required functions
source(file="K:/csabatia/GraduateStudentWork/JoshBankston/Regression Analysis/Compiled Analysis Functions.R")

#Loading in Required Parameter Data
load(file="K:/csabatia/GraduateStudentWork/JoshBankston/Regression Analysis/Plot Level Parameter Data.RData")

#Creating 1/age column in Parameter data file
SzPt20ParamDat$AgeN<-1/SzPt20ParamDat$Age
#============================================  AMD  =====================================================

#Running non-linear regression for AMD
AMD_Mod3<-nls(AMDat~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt20ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(AMD_Mod3)

#Predicting Values
SzPt20ParamDat$Pred_AMD<-predict(AMD_Mod3)

#Calculating RMSE
RMSE(SzPt20ParamDat, SzPt20ParamDat$AMDat, SzPt20ParamDat$Pred_AMD, 4)

#============================================  QMD  =====================================================

#Running non-linear regression for QMD
QMD_Mod3<-nls(QMDat~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt20ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(QMD_Mod3)

#Predicting Values
SzPt20ParamDat$Pred_QMD<-predict(QMD_Mod3)

#Calculating RMSE
RMSE(SzPt20ParamDat, SzPt20ParamDat$QMDat, SzPt20ParamDat$Pred_QMD, 4)

#============================================  D0  =====================================================

#Running non-linear regression for D0
D0_Mod3<-nls(D0~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
             
             data=SzPt20ParamDat,
             
             start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D0_Mod3)

#Predicting Values
SzPt20ParamDat$Pred_D0<-predict(D0_Mod3)

#Calculating RMSE
RMSE(SzPt20ParamDat, SzPt20ParamDat$D0, SzPt20ParamDat$Pred_D0, 4)

#============================================  D25  =====================================================

#Running non-linear regression for D25
D25_Mod3<-nls(D25~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt20ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D25_Mod3)

#Predicting Values
SzPt20ParamDat$Pred_D25<-predict(D25_Mod3)

#Calculating RMSE
RMSE(SzPt20ParamDat, SzPt20ParamDat$D25, SzPt20ParamDat$Pred_D25, 4)

#============================================  D50  =====================================================

#Running non-linear regression for D50
D50_Mod3<-nls(D50~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt20ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D50_Mod3)

#Predicting Values
SzPt20ParamDat$Pred_D50<-predict(D50_Mod3)

#Calculating RMSE
RMSE(SzPt20ParamDat, SzPt20ParamDat$D50, SzPt20ParamDat$Pred_D50, 4)

#============================================  D93  =====================================================

#Running non-linear regression for D93
D93_Mod3<-nls(D93~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt20ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D93_Mod3)

#Predicting Values
SzPt20ParamDat$Pred_D93<-predict(D93_Mod3)

#Calculating RMSE
RMSE(SzPt20ParamDat, SzPt20ParamDat$D93, SzPt20ParamDat$Pred_D93, 4)

#============================================  D95  =====================================================

#Running non-linear regression for D95
D95_Mod3<-nls(D95~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt20ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D95_Mod3)

#Predicting Values
SzPt20ParamDat$Pred_D95<-predict(D95_Mod3)

#Calculating RMSE
RMSE(SzPt20ParamDat, SzPt20ParamDat$D95, SzPt20ParamDat$Pred_D95, 4)