#Error calculation for Model 3 across all plot sizes


#Load Model 1 predicted dbh distributions
load(file="PredictedDbhDistbn_Mod3.RData")

#Load observed dbh distributions
load(file="ObservedDbhDistbn.RData")

#Loading in needed dominant height and age information
load(file="Dominant Height and Age Dataset_Model 3.RData")

#========== PLOT SIZE 0.05-ACRE =========================================================================
#Combine predicted & observed distribution data
#Make dbhClass & MeasmtObs factor variables
standTableAllPlots_05$dbhClass<-as.factor(standTableAllPlots_05$dbhClass)
standTableAllPlots_05$MeasmtObs<-as.factor(standTableAllPlots_05$MeasmtObs)

#Merge the datasets
Plot05<-merge(Obs05,standTableAllPlots_05,all=T)
Plot05$plotSize<-NULL

#Convert NA TPAs to zero
Plot05$tpaObs<-ifelse(is.na(Plot05$tpaObs),0,Plot05$tpaObs)
Plot05$tpaPredMom<-ifelse(is.na(Plot05$tpaPredMom),0,Plot05$tpaPredMom)
Plot05$tpaPredPct<-ifelse(is.na(Plot05$tpaPredPct),0,Plot05$tpaPredPct)
Plot05$tpaPredHyb<-ifelse(is.na(Plot05$tpaPredHyb),0,Plot05$tpaPredHyb)

Plot05Final<-subset(Plot05,!(tpaObs==0&tpaPredMom==0&tpaPredPct==0&tpaPredHyb==0))

#========== PLOT SIZE 0.10-ACRE =========================================================================
#Combine predicted & observed distribution data
#Make dbhClass & MeasmtObs factor variables
standTableAllPlots_10$dbhClass<-as.factor(standTableAllPlots_10$dbhClass)
standTableAllPlots_10$MeasmtObs<-as.factor(standTableAllPlots_10$MeasmtObs)

#Merge the datasets
Plot10<-merge(Obs10,standTableAllPlots_10,all=T)
Plot10$plotSize<-NULL

#Convert NA TPAs to zero
Plot10$tpaObs<-ifelse(is.na(Plot10$tpaObs),0,Plot10$tpaObs)
Plot10$tpaPredMom<-ifelse(is.na(Plot10$tpaPredMom),0,Plot10$tpaPredMom)
Plot10$tpaPredPct<-ifelse(is.na(Plot10$tpaPredPct),0,Plot10$tpaPredPct)
Plot10$tpaPredHyb<-ifelse(is.na(Plot10$tpaPredHyb),0,Plot10$tpaPredHyb)

Plot10Final<-subset(Plot10,!(tpaObs==0&tpaPredMom==0&tpaPredPct==0&tpaPredHyb==0))

#========== PLOT SIZE 0.15-ACRE =========================================================================
#Combine predicted & observed distribution data
#Make dbhClass & MeasmtObs factor variables
standTableAllPlots_15$dbhClass<-as.factor(standTableAllPlots_15$dbhClass)
standTableAllPlots_15$MeasmtObs<-as.factor(standTableAllPlots_15$MeasmtObs)

#Merge the datasets
Plot15<-merge(Obs15,standTableAllPlots_10,all=T)
Plot15$plotSize<-NULL

#Convert NA TPAs to zero
Plot15$tpaObs<-ifelse(is.na(Plot15$tpaObs),0,Plot15$tpaObs)
Plot15$tpaPredMom<-ifelse(is.na(Plot15$tpaPredMom),0,Plot15$tpaPredMom)
Plot15$tpaPredPct<-ifelse(is.na(Plot15$tpaPredPct),0,Plot15$tpaPredPct)
Plot15$tpaPredHyb<-ifelse(is.na(Plot15$tpaPredHyb),0,Plot15$tpaPredHyb)

Plot15Final<-subset(Plot15,!(tpaObs==0&tpaPredMom==0&tpaPredPct==0&tpaPredHyb==0))

#========== PLOT SIZE 0.20-ACRE =========================================================================
#Combine predicted & observed distribution data
#Make dbhClass & MeasmtObs factor variables
standTableAllPlots_20$dbhClass<-as.factor(standTableAllPlots_20$dbhClass)
standTableAllPlots_20$MeasmtObs<-as.factor(standTableAllPlots_20$MeasmtObs)

#Merge the datasets
Plot20<-merge(Obs20,standTableAllPlots_20,all=T)
Plot20$plotSize<-NULL

#Convert NA TPAs to zero
Plot20$tpaObs<-ifelse(is.na(Plot20$tpaObs),0,Plot20$tpaObs)
Plot20$tpaPredMom<-ifelse(is.na(Plot20$tpaPredMom),0,Plot20$tpaPredMom)
Plot20$tpaPredPct<-ifelse(is.na(Plot20$tpaPredPct),0,Plot20$tpaPredPct)
Plot20$tpaPredHyb<-ifelse(is.na(Plot20$tpaPredHyb),0,Plot20$tpaPredHyb)

Plot20Final<-subset(Plot20,!(tpaObs==0&tpaPredMom==0&tpaPredPct==0&tpaPredHyb==0))

#========== PLOT SIZE 0.25-ACRE =========================================================================
#Combine predicted & observed distribution data
#Make dbhClass & MeasmtObs factor variables
standTableAllPlots_25$dbhClass<-as.factor(standTableAllPlots_25$dbhClass)
standTableAllPlots_25$MeasmtObs<-as.factor(standTableAllPlots_25$MeasmtObs)

#Merge the datasets
Plot25<-merge(Obs25,standTableAllPlots_25,all=T)
Plot25$plotSize<-NULL

#Convert NA TPAs to zero
Plot25$tpaObs<-ifelse(is.na(Plot25$tpaObs),0,Plot25$tpaObs)
Plot25$tpaPredMom<-ifelse(is.na(Plot25$tpaPredMom),0,Plot25$tpaPredMom)
Plot25$tpaPredPct<-ifelse(is.na(Plot25$tpaPredPct),0,Plot25$tpaPredPct)
Plot25$tpaPredHyb<-ifelse(is.na(Plot25$tpaPredHyb),0,Plot25$tpaPredHyb)

Plot25Final<-subset(Plot25,!(tpaObs==0&tpaPredMom==0&tpaPredPct==0&tpaPredHyb==0))

#==============================================

#Merging Error tables with Dominant height and Age dataset
FinalErr_05<-merge(Plot05Final, HtDat_05)
FinalErr_10<-merge(Plot10Final, HtDat_10)
FinalErr_15<-merge(Plot15Final, HtDat_15)
FinalErr_20<-merge(Plot20Final, HtDat_20)
FinalErr_25<-merge(Plot25Final, HtDat_25)

#Saving final tables into RData object
save(FinalErr_05, FinalErr_10, FinalErr_15, FinalErr_20, FinalErr_25, file="Error Tables_Model 3.RData")

