#Error index calculations for model 3

#loading in necessary files
load(file="GreenWt & DollVal_Model 3.Rdata")

#Loading in plyr from library
library(plyr)


#Running error index calculations for plot size .05 using ddply
ErrorIndex_05_M3<-ddply(FinalTable_05, .(MeasmtObs), summarize,
                        
                        age=min(Age),
                      
                        MEIgw=round(sum(abs(GreenWt_Obs - GreenWt_M)), digits=4),
                        
                        HEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Hyb)), digits=4),
                        
                        PEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Pct)), digits=4),
                        
                        MEIdol=round(sum(abs(DollVal_obs - DollVal_M)), digits=4),
                        
                        HEIdol=round(sum(abs(DollVal_obs - DollVal_Hyb)), digits=4),
                        
                        PEIdol=round(sum(abs(DollVal_obs - DollVal_Pct)), digits=4),
                        
                        Model="Model 3",
                        
                        PlotSize=0.05)


#Running error index calculations for plot size .10 using ddply
ErrorIndex_10_M3<-ddply(FinalTable_10, .(MeasmtObs), summarize,
                        
                        age=min(Age),
                        
                        MEIgw=round(sum(abs(GreenWt_Obs - GreenWt_M)), digits=4),
                        
                        HEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Hyb)), digits=4),
                        
                        PEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Pct)), digits=4),
                        
                        MEIdol=round(sum(abs(DollVal_obs - DollVal_M)), digits=4),
                        
                        HEIdol=round(sum(abs(DollVal_obs - DollVal_Hyb)), digits=4),
                        
                        PEIdol=round(sum(abs(DollVal_obs - DollVal_Pct)), digits=4),
                        
                        Model="Model 3",
                        
                        PlotSize=0.10)


#Running error index calculations for plot size .15 using ddply
ErrorIndex_15_M3<-ddply(FinalTable_15, .(MeasmtObs), summarize,
                        
                        age=min(Age),
                        
                        MEIgw=round(sum(abs(GreenWt_Obs - GreenWt_M)), digits=4),
                        
                        HEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Hyb)), digits=4),
                        
                        PEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Pct)), digits=4),
                        
                        MEIdol=round(sum(abs(DollVal_obs - DollVal_M)), digits=4),
                        
                        HEIdol=round(sum(abs(DollVal_obs - DollVal_Hyb)), digits=4),
                        
                        PEIdol=round(sum(abs(DollVal_obs - DollVal_Pct)), digits=4),
                        
                        Model="Model 3",
                        
                        PlotSize=0.15)


#Running error index calculations for plot size .20 using ddply
ErrorIndex_20_M3<-ddply(FinalTable_20, .(MeasmtObs), summarize,
                        
                        age=min(Age),
                        
                        MEIgw=round(sum(abs(GreenWt_Obs - GreenWt_M)), digits=4),
                        
                        HEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Hyb)), digits=4),
                        
                        PEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Pct)), digits=4),
                        
                        MEIdol=round(sum(abs(DollVal_obs - DollVal_M)), digits=4),
                        
                        HEIdol=round(sum(abs(DollVal_obs - DollVal_Hyb)), digits=4),
                        
                        PEIdol=round(sum(abs(DollVal_obs - DollVal_Pct)), digits=4),
                        
                        Model="Model 3",
                        
                        PlotSize=0.20)


#Running error index calculations for plot size .25 using ddply
ErrorIndex_25_M3<-ddply(FinalTable_25, .(MeasmtObs), summarize,
                        
                        age=min(Age),
                        
                        MEIgw=round(sum(abs(GreenWt_Obs - GreenWt_M)), digits=4),
                        
                        HEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Hyb)), digits=4),
                        
                        PEIgw=round(sum(abs(GreenWt_Obs - GreenWt_Pct)), digits=4),
                        
                        MEIdol=round(sum(abs(DollVal_obs - DollVal_M)), digits=4),
                        
                        HEIdol=round(sum(abs(DollVal_obs - DollVal_Hyb)), digits=4),
                        
                        PEIdol=round(sum(abs(DollVal_obs - DollVal_Pct)), digits=4),
                        
                        Model="Model 3",
                        
                        PlotSize=0.25)

#Combining all error tables into one object
CombinedError_M3<-rbind(ErrorIndex_05_M3, ErrorIndex_10_M3, ErrorIndex_15_M3, ErrorIndex_20_M3, ErrorIndex_25_M3)

#Savings datasets into collective .Rdata file
save(ErrorIndex_05_M3, ErrorIndex_10_M3, ErrorIndex_15_M3, ErrorIndex_20_M3,
     
     ErrorIndex_25_M3, CombinedError_M3, file="ErrorIndex_M3.Rdata")
