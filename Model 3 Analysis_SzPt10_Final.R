#Non-Linear Regression and Prediction for Model 3 - SzPt10

#Loading in required functions
source(file="Compiled Analysis Functions.R")

#Loading in Required Parameter Data
load(file="Plot Level Parameter Data.RData")

#Creating 1/age column in Parameter data file
SzPt10ParamDat$AgeN<-1/SzPt10ParamDat$Age
#============================================  AMD  =====================================================

#Running non-linear regression for AMD
AMD_Mod3<-nls(AMDat~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt10ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(AMD_Mod3)

#Predicting Values
SzPt10ParamDat$Pred_AMD<-predict(AMD_Mod3)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$AMDat, SzPt10ParamDat$Pred_AMD, 3)

#============================================  QMD  =====================================================

#Running non-linear regression for QMD
QMD_Mod3<-nls(QMDat~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt10ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(QMD_Mod3)

#Predicting Values
SzPt10ParamDat$Pred_QMD<-predict(QMD_Mod3)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$QMDat, SzPt10ParamDat$Pred_QMD, 3)

#============================================  D0  =====================================================

#Running non-linear regression for D0
D0_Mod3<-nls(D0~DomHtM^b1*TPH^b2*exp(b3*AgeN),
             
             data=SzPt10ParamDat,
             
             start=list(b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D0_Mod3)  #RMSE 2.033

#Predicting Values
SzPt10ParamDat$Pred_D0<-predict(D0_Mod3)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D0, SzPt10ParamDat$Pred_D0, 3)

#============================================  D25  =====================================================

#Running non-linear regression for D25
D25_Mod3<-nls(D25~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt10ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D25_Mod3)

#Predicting Values
SzPt10ParamDat$Pred_D25<-predict(D25_Mod3)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D25, SzPt10ParamDat$Pred_D25, 4)

#============================================  D50  =====================================================

#Running non-linear regression for D50
D50_Mod3<-nls(D50~b0*DomHtM^b1*TPH^b2,
              
              data=SzPt10ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25))

#Generating summary of results
summary(D50_Mod3)

#Predicting Values
SzPt10ParamDat$Pred_D50<-predict(D50_Mod3)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D50, SzPt10ParamDat$Pred_D50, 3)

#============================================  D93  =====================================================

#Running non-linear regression for D93
D93_Mod3<-nls(D93~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt10ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D93_Mod3)

#Predicting Values
SzPt10ParamDat$Pred_D93<-predict(D93_Mod3)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D93, SzPt10ParamDat$Pred_D93, 4)

#============================================  D95  =====================================================

#Running non-linear regression for D95
D95_Mod3<-nls(D95~b0*DomHtM^b1*TPH^b2*exp(b3*AgeN),
              
              data=SzPt10ParamDat,
              
              start=list(b0=2, b1=0.7, b2=-0.25, b3=-0.4))

#Generating summary of results
summary(D95_Mod3)

#Predicting Values
SzPt10ParamDat$Pred_D95<-predict(D95_Mod3)

#Calculating RMSE
RMSE(SzPt10ParamDat, SzPt10ParamDat$D95, SzPt10ParamDat$Pred_D95, 4)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#========================================================================================

#Separating Plot Data for Further Analysis

#Get column subsets
M3_PlotSz10<-SzPt10ParamDat[,c("MeasmtObs","Age","DomHtM","TPH","RS",
                               "Pred_AMD", "Pred_QMD","Pred_D0","Pred_D25","Pred_D50","Pred_D93",
                               "Pred_D95")]



save(M3_PlotSz10,file="K:/csabatia/GraduateStudentWork/JoshBankston/Data Analysis/Distribution Recovery Analyses/Model 3_BF87/M3_PlotLevelAnalysis10.RData")
