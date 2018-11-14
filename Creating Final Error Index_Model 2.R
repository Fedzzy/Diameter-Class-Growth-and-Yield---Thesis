#Creating Final Error Index Table for Model 1

#Loading in Data
load(file="ErrorIndex_M2.Rdata")
load(file="ThinData.RData")

#Merging Data into Final Dataset
FEI_M2<-merge(CombinedErrors_M2, ThinData) #FEI stands for Final Error Index

#Saving FEI_M1 as object in directory
save(FEI_M2, file="Final Error Index_Model 2.RData")
