
#THIS ANALYSIS IS A REFINEMENT OF THE MODELS FROM FILE ..... WITH INSIGNIFICANT VARIABLES DROPPED
#======================Regression Analysis of Different Varibles - Model 1 - SzPt05==================

#FITTING THE MODELS & USING THE MODELS TO PREDICT MOMENTS OR PERCENTILES

#Loading in Sourced Functions
source(file="Compiled Analysis Functions.R")

#Load in Parameter Datasets
load(file="Plot Level Parameter Data.RData")

#========================================Nonlinear Regression for AMD - Model 1

AMD_Mod1<-nls(AMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))

summary(AMD_Mod1)

#Prediction
SzPt05ParamDat$AMD_Mod1<-predict(AMD_Mod1)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$AMDat, SzPt05ParamDat$AMD_Mod1, 4)


#========================================Nonlinear Regression for QMD - Model 1
#Refined model
QMD_Mod1<-nls(QMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10))


summary(QMD_Mod1)

#Prediction
SzPt05ParamDat$QMD_Mod1<-predict(QMD_Mod1)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$QMDat, SzPt05ParamDat$QMD_Mod1, 3)
#---------------------------------------------------------------------
#Original Model
QMD_Mod1_Or<-nls(QMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(QMD_Mod1_Or)

#Prediction
SzPt05ParamDat$QMD_Mod1_Or<-predict(QMD_Mod1_Or)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$QMDat, SzPt05ParamDat$QMD_Mod1_Or, 4)

#==================
#Is dropping of age from QMD model QMD_Mod1_Or significant?
#Carry out a " likelihood ratio test" on the two model fits
anova(QMD_Mod1,QMD_Mod1_Or)

#Pr(>F) is greater than 0.05. So, dropping age from the model has no significant effect on model precision

#Try this for the AMD model, where age was significant part of the model
#AMD Model without age
AMD_Mod1_Reduced<-nls(AMDat~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10)
              )

summary(AMD_Mod1_Reduced)

#Carry out a " likelihood ratio test" on the two AMD model fits
anova(AMD_Mod1_Reduced,AMD_Mod1)

#Pr(>F) is less than 0.05. So, dropping age from the model has significant effect on model precicion

#=============================================================================================================



#========================================Nonlinear Regression for D0 - Model 1
#Taking out due to alterntive 1 being a better predictor of dbhMin
D0_Mod1<-nls(D0~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))

#Obtaining summary of results
summary(D0_Mod1)  #RMSE found to be 2.161

#Prediction
SzPt05ParamDat$D0_Mod1<-predict(D0_Mod1)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D0, SzPt05ParamDat$D0_Mod1, 4)


#========================================Nonlinear Regression for D25 - Model 1

D25_Mod1<-nls(D25~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
             
             data=(SzPt05ParamDat),
             
             start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D25_Mod1)

#Prediction
SzPt05ParamDat$D25_Mod1<-predict(D25_Mod1)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D25, SzPt05ParamDat$D25_Mod1, 4)

#========================================Nonlinear Regression for D50 - Model 1

D50_Mod1<-nls(D50~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))+b3*Age),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10, b3=.006))


summary(D50_Mod1)

#Prediction
SzPt05ParamDat$D50_Mod1<-predict(D50_Mod1)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D50, SzPt05ParamDat$D50_Mod1, 4)

#========================================Nonlinear Regression for D93 - Model 1

D93_Mod1<-nls(D93~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10))


summary(D93_Mod1)

#Prediction
SzPt05ParamDat$D93_Mod1<-predict(D93_Mod1)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D93, SzPt05ParamDat$D93_Mod1, 3)

#========================================Nonlinear Regression for D95 - Model 1

D95_Mod1<-nls(D95~b0*(TPH^b1)*exp((b2/sqrt(DomHtM))),
              
              data=(SzPt05ParamDat),
              
              start=list(b0=105, b1=-0.2, b2=-10))


summary(D95_Mod1)

#Prediction
SzPt05ParamDat$D95_Mod1<-predict(D95_Mod1)

#Calculating RMSE
RMSE(SzPt05ParamDat, SzPt05ParamDat$D95, SzPt05ParamDat$D95_Mod1, 3)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#========================================================================================

#Separating Plot Data for Further Analysis

#Get column subsets
M1_PlotSz05<-SzPt05ParamDat[,c("MeasmtObs","Age","DomHtM","TPH","RS",
                            "AMD_Mod1", "QMD_Mod1","D0_Mod1","D25_Mod1","D50_Mod1","D93_Mod1",
                            "D95_Mod1")]


#SAVING THE PREDICTED MOMENTS & PERCENTILES INTO AN .RDATA FILE
save(M1_PlotSz05,file="K:/csabatia/GraduateStudentWork/JoshBankston/Data Analysis/Distribution Recovery Analyses/Model 1_Cao2004/PlotLevelAnalysis05.RData")




