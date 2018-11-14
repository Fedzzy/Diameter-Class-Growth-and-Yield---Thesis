#Non-Linear Regression and Prediction for Model 3 - SzPt05

#Loading in required functions
source(file="Compiled Analysis Functions.R")

#Loading in Required Parameter Data
load(file="Plot Level Parameter Data.RData")

#Creating 1/age column in Parameter data file
SzPt05ParamDat$AgeN<-1/SzPt05ParamDat$Age

#============================================  AMD  =====================================================

#Running non-linear regression for AMD
AMD_Mod3<-nls(AMDat~b0*DomHtM^b1*TPH^b2,

              data=SzPt05ParamDat,

              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(AMD_Mod3)

#Predicting Values
SzPt05ParamDat$Pred_AMD<-predict(AMD_Mod3)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$AMDat, SzPt05ParamDat$Pred_AMD, 3)

#============================================  QMD  =====================================================

#Running non-linear regression for QMD
QMD_Mod3<-nls(QMDat~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt05ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(QMD_Mod3)

#Predicting Values
SzPt05ParamDat$Pred_QMD<-predict(QMD_Mod3)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$QMDat, SzPt05ParamDat$Pred_QMD, 3)

#============================================  D0  =====================================================

#Running non-linear regression for D0
D0_Mod3<-nls(D0~DomHtM^b1*TPH^b2,
              
              data=SzPt05ParamDat,
              
              start=list(b1=0.7, b2=-0.25))

#Generating summary of results
summary(D0_Mod3)

#Predicting Values
SzPt05ParamDat$Pred_D0<-predict(D0_Mod3)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D0, SzPt05ParamDat$Pred_D0, 2)

#============================================  D25  =====================================================

#Running non-linear regression for D25
D25_Mod3<-nls(D25~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt05ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(D25_Mod3)

#Predicting Values
SzPt05ParamDat$Pred_D25<-predict(D25_Mod3)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D25, SzPt05ParamDat$Pred_D25, 3)

#============================================  D50  =====================================================

#Running non-linear regression for D50
D50_Mod3<-nls(D50~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt05ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(D50_Mod3)

#Predicting Values
SzPt05ParamDat$Pred_D50<-predict(D50_Mod3)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D50, SzPt05ParamDat$Pred_D50, 3)

#============================================  D93  =====================================================

#Running non-linear regression for D93
D93_Mod3<-nls(D93~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt05ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(D93_Mod3)

#Predicting Values
SzPt05ParamDat$Pred_D93<-predict(D93_Mod3)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D93, SzPt05ParamDat$Pred_D93, 3)

#============================================  D95  =====================================================

#Running non-linear regression for D95
D95_Mod3<-nls(D95~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt05ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(D95_Mod3)

#Predicting Values
SzPt05ParamDat$Pred_D95<-predict(D95_Mod3)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D95, SzPt05ParamDat$Pred_D95, 3)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#========================================================================================

#Separating Plot Data for Further Analysis

#Get column subsets
M3_PlotSz05<-SzPt05ParamDat[,c("MeasmtObs","Age","DomHtM","TPH","RS",
                               "Pred_AMD", "Pred_QMD","Pred_D0","Pred_D25","Pred_D50","Pred_D93",
                               "Pred_D95")]



save(M3_PlotSz05,file="K:/csabatia/GraduateStudentWork/JoshBankston/Data Analysis/Distribution Recovery Analyses/Model 3_BF87/M3_PlotLevelAnalysis05.RData")
