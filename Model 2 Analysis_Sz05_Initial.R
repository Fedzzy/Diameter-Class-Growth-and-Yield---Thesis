#Non-Linear Regression and Prediction for Model 2 - SzPt05

#Loading in required functions
source(file="K:/csabatia/GraduateStudentWork/JoshBankston/Regression Analysis/Compiled Analysis Functions.R")

#Loading in Required Parameter Data
load(file="K:/csabatia/GraduateStudentWork/JoshBankston/Regression Analysis/Plot Level Parameter Data.RData")

#Adding in log() columns
SzPt05ParamDat$LTPH<-log(SzPt05ParamDat$TPH)
SzPt05ParamDat$LHt<-log(SzPt05ParamDat$DomHtM)

#============================================  AMD  =====================================================

#Running non-linear regression for AMD
AMD_Mod2<-nls(AMDat~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
              
              data = SzPt05ParamDat,
              
              start=list(b1=4.97, b2=-0.2, b3=-0.3, b4=0.2, b5=-.5))

#Generating summary of results
summary(AMD_Mod2)

#Predicting Values
SzPt05ParamDat$Pred_AMD<-predict(AMD_Mod2)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$AMDat, SzPt05ParamDat$Pred_AMD, 5)

#============================================  QMD  =====================================================

#Running non-linear regression for QMD
QMD_Mod2<-nls(QMDat~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
              
              data = SzPt05ParamDat,
              
              start=list(b1=4.26, b2=-0.2, b3=-0.3, b4=0.2, b5=-.5))

#Generating summary of results
summary(QMD_Mod2)

#Predicting Values
SzPt05ParamDat$Pred_QMD<-predict(QMD_Mod2)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$QMDat, SzPt05ParamDat$Pred_QMD, 5)

#============================================  D0  =====================================================

#Running non-linear regression for D0
D0_Mod2<-nls(D0~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
              
              data = SzPt05ParamDat,
              
              start=list(b1=5.08, b2=-0.3, b3=-0.7, b4=0.3, b5=-1))

#Generating summary of results
summary(D0_Mod2)

#Predicting Values
SzPt05ParamDat$Pred_D0<-predict(D0_Mod2)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D0, SzPt05ParamDat$Pred_D0, 5)

#============================================  D25  =====================================================

#Running non-linear regression for D25
D25_Mod2<-nls(D25~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
             
             data = SzPt05ParamDat,
             
             start=list(b1=5.39, b2=-0.3, b3=-0.4, b4=0.1, b5=.07))

#Generating summary of results
summary(D25_Mod2)

#Predicting Values
SzPt05ParamDat$Pred_D25<-predict(D25_Mod2)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D25, SzPt05ParamDat$Pred_D25, 5)

#============================================  D50  =====================================================

#Running non-linear regression for D50
D50_Mod2<-nls(D50~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
             
             data = SzPt05ParamDat,
             
             start=list(b1=4.72, b2=-0.2, b3=-0.3, b4=0.2, b5=-.2))

#Generating summary of results
summary(D50_Mod2)

#Predicting Values
SzPt05ParamDat$Pred_D50<-predict(D50_Mod2)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D50, SzPt05ParamDat$Pred_D50, 5)

#============================================  D93  =====================================================

#Running non-linear regression for D93
D93_Mod2<-nls(D93~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
              
              data = SzPt05ParamDat,
              
              start=list(b1=3.77, b2=-0.1, b3=-0.2, b4=0.3, b5=-1))

#Generating summary of results
summary(D93_Mod2)

#Predicting Values
SzPt05ParamDat$Pred_D93<-predict(D93_Mod2)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D93, SzPt05ParamDat$Pred_D93, 5)

#============================================  D95  =====================================================

#Running non-linear regression for D95
D95_Mod2<-nls(D95~exp(b1+b2*RS+b3*LTPH+b4*LHt+(b5/Age)),
              
              data = SzPt05ParamDat,
              
              start=list(b1=3.61, b2=-0.1, b3=-0.2, b4=0.4, b5=-1.1))

#Generating summary of results
summary(D95_Mod2)

#Predicting Values
SzPt05ParamDat$Pred_D95<-predict(D95_Mod2)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D95, SzPt05ParamDat$Pred_D95, 5)