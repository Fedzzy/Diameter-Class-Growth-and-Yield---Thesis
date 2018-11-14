#Creating Final Error Index Table for Model 3

#Loading in Data
load(file="ErrorIndex_M3.Rdata")
load(file="ThinData.RData")

#Merging Data into Final Dataset
FEI_M3<-merge(CombinedError_M3, ThinData) #FEI stands for Final Error Index

#Saving FEI_M1 as object in directory
save(FEI_M3, file="Final Error Index_Model 3.RData")
