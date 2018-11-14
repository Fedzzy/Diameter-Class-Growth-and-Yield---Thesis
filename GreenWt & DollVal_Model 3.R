#Calculating GreenWeight for Each DBH Class - Model 3

#Sourcing in needed GreenWt function
source(file="GreenWt Function.R")
source(file="DolVal Function.R")
source(file="QMDgroupedData.R")

#Loading in needed datasets for Model 1
load(file="Final Error Tables_Model 3.RData")

#=======================================Beginning Analysis for Plot Size .05 - Model 3=============================

#Creating non-factored dbhClass column
FinalTable_05$dbh<-as.numeric(paste(FinalTable_05$dbhClass))

#Creating QMD columns for each of the 3 prediciton methods
FinalTable_05$QMD_M<-QMDgroup(FinalTable_05$dbh, FinalTable_05$tpaPredMom)

FinalTable_05$QMD_Pct<-QMDgroup(FinalTable_05$dbh, FinalTable_05$tpaPredPct)

FinalTable_05$QMD_Hyb<-QMDgroup(FinalTable_05$dbh, FinalTable_05$tpaPredHyb)

#Creating column for height in feet
FinalTable_05$Ht_Obs<-(5.602*(FinalTable_05$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_05$QMDobs/FinalTable_05$dbh)^.25))

FinalTable_05$Ht_M<-(5.602*(FinalTable_05$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_05$QMD_M/FinalTable_05$dbh)^.25))

FinalTable_05$Ht_Pct<-(5.602*(FinalTable_05$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_05$QMD_Pct/FinalTable_05$dbh)^.25))

FinalTable_05$Ht_Hyb<-(5.602*(FinalTable_05$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_05$QMD_Hyb/FinalTable_05$dbh)^.25))

#Calculating Green Weight by dbhClass
FinalTable_05$GreenWt_Obs<-GrnWt(FinalTable_05$dbh, FinalTable_05$Ht_Obs)*FinalTable_05$tpaObs

FinalTable_05$GreenWt_M<-GrnWt(FinalTable_05$dbh, FinalTable_05$Ht_M)*FinalTable_05$tpaPredMom

FinalTable_05$GreenWt_Pct<-GrnWt(FinalTable_05$dbh, FinalTable_05$Ht_Pct)*FinalTable_05$tpaPredPct

FinalTable_05$GreenWt_Hyb<-GrnWt(FinalTable_05$dbh, FinalTable_05$Ht_Hyb)*FinalTable_05$tpaPredHyb

#Assigning Dollar Value
FinalTable_05$DollVal_obs<-DollVal(FinalTable_05$dbh, FinalTable_05$GreenWt_Obs)

FinalTable_05$DollVal_M<-DollVal(FinalTable_05$dbh, FinalTable_05$GreenWt_M)

FinalTable_05$DollVal_Pct<-DollVal(FinalTable_05$dbh, FinalTable_05$GreenWt_Pct)

FinalTable_05$DollVal_Hyb<-DollVal(FinalTable_05$dbh, FinalTable_05$GreenWt_Hyb)


#=======================================Beginning Analysis for Plot Size .10 - Model 3=============================

#Creating non-factored dbhClass column
FinalTable_10$dbh<-as.numeric(paste(FinalTable_10$dbhClass))

#Creating QMD columns for each of the 3 prediciton methods
FinalTable_10$QMD_M<-QMDgroup(FinalTable_10$dbh, FinalTable_10$tpaPredMom)

FinalTable_10$QMD_Pct<-QMDgroup(FinalTable_10$dbh, FinalTable_10$tpaPredPct)

FinalTable_10$QMD_Hyb<-QMDgroup(FinalTable_10$dbh, FinalTable_10$tpaPredHyb)

#Creating column for height in feet
FinalTable_10$Ht_Obs<-(5.602*(FinalTable_10$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_10$QMDobs/FinalTable_10$dbh)^.25))

FinalTable_10$Ht_M<-(5.602*(FinalTable_10$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_10$QMD_M/FinalTable_10$dbh)^.25))

FinalTable_10$Ht_Pct<-(5.602*(FinalTable_10$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_10$QMD_Pct/FinalTable_10$dbh)^.25))

FinalTable_10$Ht_Hyb<-(5.602*(FinalTable_10$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_10$QMD_Hyb/FinalTable_10$dbh)^.25))

#Calculating Green Weight by dbhClass
FinalTable_10$GreenWt_Obs<-GrnWt(FinalTable_10$dbh, FinalTable_10$Ht_Obs)*FinalTable_10$tpaObs

FinalTable_10$GreenWt_M<-GrnWt(FinalTable_10$dbh, FinalTable_10$Ht_M)*FinalTable_10$tpaPredMom

FinalTable_10$GreenWt_Pct<-GrnWt(FinalTable_10$dbh, FinalTable_10$Ht_Pct)*FinalTable_10$tpaPredPct

FinalTable_10$GreenWt_Hyb<-GrnWt(FinalTable_10$dbh, FinalTable_10$Ht_Hyb)*FinalTable_10$tpaPredHyb

#Assigning Dollar Value
FinalTable_10$DollVal_obs<-DollVal(FinalTable_10$dbh, FinalTable_10$GreenWt_Obs)

FinalTable_10$DollVal_M<-DollVal(FinalTable_10$dbh, FinalTable_10$GreenWt_M)

FinalTable_10$DollVal_Pct<-DollVal(FinalTable_10$dbh, FinalTable_10$GreenWt_Pct)

FinalTable_10$DollVal_Hyb<-DollVal(FinalTable_10$dbh, FinalTable_10$GreenWt_Hyb)


#=======================================Beginning Analysis for Plot Size .15 - Model 3=============================

#Creating non-factored dbhClass column
FinalTable_15$dbh<-as.numeric(paste(FinalTable_15$dbhClass))

#Creating QMD columns for each of the 3 prediciton methods
FinalTable_15$QMD_M<-QMDgroup(FinalTable_15$dbh, FinalTable_15$tpaPredMom)

FinalTable_15$QMD_Pct<-QMDgroup(FinalTable_15$dbh, FinalTable_15$tpaPredPct)

FinalTable_15$QMD_Hyb<-QMDgroup(FinalTable_15$dbh, FinalTable_15$tpaPredHyb)

#Creating column for height in feet
FinalTable_15$Ht_Obs<-(5.602*(FinalTable_15$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_15$QMDobs/FinalTable_15$dbh)^.25))

FinalTable_15$Ht_M<-(5.602*(FinalTable_15$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_15$QMD_M/FinalTable_15$dbh)^.25))

FinalTable_15$Ht_Pct<-(5.602*(FinalTable_15$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_15$QMD_Pct/FinalTable_15$dbh)^.25))

FinalTable_15$Ht_Hyb<-(5.602*(FinalTable_15$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_15$QMD_Hyb/FinalTable_15$dbh)^.25))

#Calculating Green Weight by dbhClass
FinalTable_15$GreenWt_Obs<-GrnWt(FinalTable_15$dbh, FinalTable_15$Ht_Obs)*FinalTable_15$tpaObs

FinalTable_15$GreenWt_M<-GrnWt(FinalTable_15$dbh, FinalTable_15$Ht_M)*FinalTable_15$tpaPredMom

FinalTable_15$GreenWt_Pct<-GrnWt(FinalTable_15$dbh, FinalTable_15$Ht_Pct)*FinalTable_15$tpaPredPct

FinalTable_15$GreenWt_Hyb<-GrnWt(FinalTable_15$dbh, FinalTable_15$Ht_Hyb)*FinalTable_15$tpaPredHyb

#Assigning Dollar Value
FinalTable_15$DollVal_obs<-DollVal(FinalTable_15$dbh, FinalTable_15$GreenWt_Obs)

FinalTable_15$DollVal_M<-DollVal(FinalTable_15$dbh, FinalTable_15$GreenWt_M)

FinalTable_15$DollVal_Pct<-DollVal(FinalTable_15$dbh, FinalTable_15$GreenWt_Pct)

FinalTable_15$DollVal_Hyb<-DollVal(FinalTable_15$dbh, FinalTable_15$GreenWt_Hyb)

#=======================================Beginning Analysis for Plot Size .20 - Model 3=============================

#Creating non-factored dbhClass column
FinalTable_20$dbh<-as.numeric(paste(FinalTable_20$dbhClass))

#Creating QMD columns for each of the 3 prediciton methods
FinalTable_20$QMD_M<-QMDgroup(FinalTable_20$dbh, FinalTable_20$tpaPredMom)

FinalTable_20$QMD_Pct<-QMDgroup(FinalTable_20$dbh, FinalTable_20$tpaPredPct)

FinalTable_20$QMD_Hyb<-QMDgroup(FinalTable_20$dbh, FinalTable_20$tpaPredHyb)

#Creating column for height in feet
FinalTable_20$Ht_Obs<-(5.602*(FinalTable_20$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_20$QMDobs/FinalTable_20$dbh)^.25))

FinalTable_20$Ht_M<-(5.602*(FinalTable_20$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_20$QMD_M/FinalTable_20$dbh)^.25))

FinalTable_20$Ht_Pct<-(5.602*(FinalTable_20$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_20$QMD_Pct/FinalTable_20$dbh)^.25))

FinalTable_20$Ht_Hyb<-(5.602*(FinalTable_20$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_20$QMD_Hyb/FinalTable_20$dbh)^.25))

#Calculating Green Weight by dbhClass
FinalTable_20$GreenWt_Obs<-GrnWt(FinalTable_20$dbh, FinalTable_20$Ht_Obs)*FinalTable_20$tpaObs

FinalTable_20$GreenWt_M<-GrnWt(FinalTable_20$dbh, FinalTable_20$Ht_M)*FinalTable_20$tpaPredMom

FinalTable_20$GreenWt_Pct<-GrnWt(FinalTable_20$dbh, FinalTable_20$Ht_Pct)*FinalTable_20$tpaPredPct

FinalTable_20$GreenWt_Hyb<-GrnWt(FinalTable_20$dbh, FinalTable_20$Ht_Hyb)*FinalTable_20$tpaPredHyb

#Assigning Dollar Value
FinalTable_20$DollVal_obs<-DollVal(FinalTable_20$dbh, FinalTable_20$GreenWt_Obs)

FinalTable_20$DollVal_M<-DollVal(FinalTable_20$dbh, FinalTable_20$GreenWt_M)

FinalTable_20$DollVal_Pct<-DollVal(FinalTable_20$dbh, FinalTable_20$GreenWt_Pct)

FinalTable_20$DollVal_Hyb<-DollVal(FinalTable_20$dbh, FinalTable_20$GreenWt_Hyb)

#=======================================Beginning Analysis for Plot Size .25 - Model 3=============================

#Creating non-factored dbhClass column
FinalTable_25$dbh<-as.numeric(paste(FinalTable_25$dbhClass))

#Creating QMD columns for each of the 3 prediciton methods
FinalTable_25$QMD_M<-QMDgroup(FinalTable_25$dbh, FinalTable_25$tpaPredMom)

FinalTable_25$QMD_Pct<-QMDgroup(FinalTable_25$dbh, FinalTable_25$tpaPredPct)

FinalTable_25$QMD_Hyb<-QMDgroup(FinalTable_25$dbh, FinalTable_25$tpaPredHyb)

#Creating column for height in feet
FinalTable_25$Ht_Obs<-(5.602*(FinalTable_25$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_25$QMDobs/FinalTable_25$dbh)^.25))

FinalTable_25$Ht_M<-(5.602*(FinalTable_25$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_25$QMD_M/FinalTable_25$dbh)^.25))

FinalTable_25$Ht_Pct<-(5.602*(FinalTable_25$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_25$QMD_Pct/FinalTable_25$dbh)^.25))

FinalTable_25$Ht_Hyb<-(5.602*(FinalTable_25$DomHtM*3.28084)^1.0021*exp(-1.76969*(FinalTable_25$QMD_Hyb/FinalTable_25$dbh)^.25))

#Calculating Green Weight by dbhClass
FinalTable_25$GreenWt_Obs<-GrnWt(FinalTable_25$dbh, FinalTable_25$Ht_Obs)*FinalTable_25$tpaObs

FinalTable_25$GreenWt_M<-GrnWt(FinalTable_25$dbh, FinalTable_25$Ht_M)*FinalTable_25$tpaPredMom

FinalTable_25$GreenWt_Pct<-GrnWt(FinalTable_25$dbh, FinalTable_25$Ht_Pct)*FinalTable_25$tpaPredPct

FinalTable_25$GreenWt_Hyb<-GrnWt(FinalTable_25$dbh, FinalTable_25$Ht_Hyb)*FinalTable_25$tpaPredHyb

#Assigning Dollar Value
FinalTable_25$DollVal_obs<-DollVal(FinalTable_25$dbh, FinalTable_25$GreenWt_Obs)

FinalTable_25$DollVal_M<-DollVal(FinalTable_25$dbh, FinalTable_25$GreenWt_M)

FinalTable_25$DollVal_Pct<-DollVal(FinalTable_25$dbh, FinalTable_25$GreenWt_Pct)

FinalTable_25$DollVal_Hyb<-DollVal(FinalTable_25$dbh, FinalTable_25$GreenWt_Hyb)

#=======================================Saving Objects into completed Dataset
save(FinalTable_05, FinalTable_10, FinalTable_15, FinalTable_20, FinalTable_25, file="GreenWt & DollVal_Model 3.Rdata")
