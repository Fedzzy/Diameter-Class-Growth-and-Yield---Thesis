#Non-Linear Regression and Prediction for Model 2 - SzPt25

#Loading in required functions
source(file="Compiled Analysis Functions.R")

#Loading in Required Parameter Data
load(file="Plot Level Parameter Data.RData")

#Adding in log() columns
SzPt25ParamDat$LTPH<-log(SzPt25ParamDat$TPH)
SzPt25ParamDat$LHt<-log(SzPt25ParamDat$DomHtM)

#============================================  AMD  =====================================================

#Running non-linear regression for AMD
AMD_Mod2<-nls(AMDat~exp(b1+b2*RS+b3*LTPH+b4*LHt),
              
              data = SzPt25ParamDat,
              
              start=list(b1=4.71, b2=-0.2, b3=-0.3, b4=0.2))

#Generating summary of results
summary(AMD_Mod2)

#Predicting Values
SzPt25ParamDat$Pred_AMD<-predict(AMD_Mod2)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$AMDat, SzPt25ParamDat$Pred_AMD, 4)

#============================================  QMD  =====================================================

#Running non-linear regression for QMD
QMD_Mod2<-nls(QMDat~exp(b1+b2*RS+b3*LTPH+b4*LHt),
              
              data = SzPt25ParamDat,
              
              start=list(b1=4.3, b2=-0.2, b3=-0.3, b4=0.2))

#Generating summary of results
summary(QMD_Mod2)

#Predicting Values
SzPt25ParamDat$Pred_QMD<-predict(QMD_Mod2)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$QMDat, SzPt25ParamDat$Pred_QMD, 4)

#============================================  D0  =====================================================

#Running non-linear regression for D0
D0_Mod2<-nls(D0~exp(b1+b2*RS+b3*LTPH+(b5/Age)),
             
             data = SzPt25ParamDat,
             
             start=list(b1=5.08, b2=-0.3, b3=-0.7, b5=-1))

#Generating summary of results
summary(D0_Mod2)  #RMSE 1.883

#Predicting Values
SzPt25ParamDat$Pred_D0<-predict(D0_Mod2)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D0, SzPt25ParamDat$Pred_D0, 4)

#============================================  D25  =====================================================

#Running non-linear regression for D25
D25_Mod2<-nls(D25~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
              
              data = SzPt25ParamDat,
              
              start=list(b1=5.4, b2=-0.3, b3=-0.4, b4=0.1, b5=.07))

#Generating summary of results
summary(D25_Mod2)

#Predicting Values
SzPt25ParamDat$Pred_D25<-predict(D25_Mod2)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D25, SzPt25ParamDat$Pred_D25, 5)

#============================================  D50  =====================================================

#Running non-linear regression for D50
D50_Mod2<-nls(D50~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
              
              data = SzPt25ParamDat,
              
              start=list(b1=4.72, b2=-0.2, b3=-0.3, b4=0.2, b5=-.2))

#Generating summary of results
summary(D50_Mod2)

#Predicting Values
SzPt25ParamDat$Pred_D50<-predict(D50_Mod2)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D50, SzPt25ParamDat$Pred_D50, 5)

#============================================  D93  =====================================================

#Running non-linear regression for D93
D93_Mod2<-nls(D93~exp(b1+b2*RS+b3*LTPH+b4*LHt),
              
              data = SzPt25ParamDat,
              
              start=list(b1=3.78, b2=-0.1, b3=-0.2, b4=0.3))

#Generating summary of results
summary(D93_Mod2)

#Predicting Values
SzPt25ParamDat$Pred_D93<-predict(D93_Mod2)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D93, SzPt25ParamDat$Pred_D93, 4)

#============================================  D95  =====================================================

#Running non-linear regression for D95
D95_Mod2<-nls(D95~exp(b1+b2*RS+b3*LTPH+b4*LHt),
              
              data = SzPt25ParamDat,
              
              start=list(b1=3.61, b2=-0.1, b3=-0.2, b4=0.4))

#Generating summary of results
summary(D95_Mod2)

#Predicting Values
SzPt25ParamDat$Pred_D95<-predict(D95_Mod2)

#Calculating RMSE
RMSE(SzPt25ParamDat, SzPt25ParamDat$D95, SzPt25ParamDat$Pred_D95, 4)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#========================================================================================

#Separating Plot Data for Further Analysis

#Get column subsets
M2_PlotSz25<-SzPt25ParamDat[,c("MeasmtObs","Age","DomHtM","TPH","RS",
                               "Pred_AMD", "Pred_QMD","Pred_D0","Pred_D25","Pred_D50","Pred_D93",
                               "Pred_D95")]



save(M2_PlotSz25,file="K:/csabatia/GraduateStudentWork/JoshBankston/Data Analysis/Distribution Recovery Analyses/Model 2_MF1992/M2_PlotLevelAnalysis25.RData")
