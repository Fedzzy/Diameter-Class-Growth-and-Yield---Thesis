#=== COMPUTING OVERAL ERROR INDICES =======

#loading in necessary files
load(file="Final Error Index_model 1.RData")
load(file="Final Error Index_model 2.RData")
load(file="Final Error Index_model 3.RData")

#loading in plyr for ddply
library(plyr)

#Combining all three datasets into one
AllDat<-rbind(FEI_M1, FEI_M2, FEI_M3) #FEI = Final Error Index

#Rename Model 1, 2, & 3 to the easy to recognise names
AllDat$Model<-as.factor(revalue(AllDat$Model, c("Model 1"="Matney and Farrar (1992) Model",
                                                      "Model 2"="Cao (2004) Model",
                                                      "Model 3"="Baldwin and Fedducia (1987) Model")
                                  )
                       )

#====== EI ACROSS ALL DATA ================================================
#Obtaining across all data averages for gw and dollval by plot size
EIall<-ddply(AllDat, .(Model, PlotSize), summarize,
           
           AvgM=mean(MEIgw),
           
           AvgH=mean(HEIgw),
           
           AvgP=mean(PEIgw),
           
           AvgMdol=mean(MEIdol),
           
           AvgHdol=mean(HEIdol),
           
           AvgPdol=mean(PEIdol)
           )

#Separate green weight from dollar errors
EIallTon<-EIall[,c("Model","PlotSize","AvgM","AvgH","AvgP")]
names(EIallTon)[3:5]<-c("Method of Moments Recovery Approach","Hybrid Recovery Approach",
                        "Percentiles Recovery Approach")
EIallDol<-EIall[,c("Model","PlotSize","AvgMdol","AvgHdol","AvgPdol")]
names(EIallDol)[3:5]<-c("Method of Moments Recovery Approach","Hybrid Recovery Approach",
                        "Percentiles Recovery Approach")

#Reshape the green tons errors data to the long form
EIallTon2<-reshape(EIallTon,varying=list(3:5),
                      times=names(EIallTon)[3:5],
                      v.names="ErrorIndex",direction="long")
row.names(EIallTon2)<-NULL
EIallTon2$id<-NULL

names(EIallTon2)[names(EIallTon2)%in%"time"]<-"RecoveryMethod"

#Reshape the dollar errors data to the long form
EIallDol2<-reshape(EIallDol,varying=list(3:5),
                   times=names(EIallDol)[3:5],
                   v.names="ErrorIndex",direction="long")
row.names(EIallDol2)<-NULL
EIallDol2$id<-NULL
names(EIallDol2)[names(EIallDol2)%in%"time"]<-"RecoveryMethod"



EIallTon2$RecoveryMethod<-as.factor(EIallTon2$RecoveryMethod)
EIallDol2$RecoveryMethod<-as.factor(EIallDol2$RecoveryMethod)


#Plot Green Weight Errors
library(lattice)
color<-c("black","blue","red")
EItonPlot<-xyplot(ErrorIndex~PlotSize|Model,
                 par.strip.text=list(cex=1.3),
                 #par.settings=list(strip.background=list(col="lightgrey")),
                 data=EIallTon2,
                 groups=RecoveryMethod,
                 type=c("b","g"),lwd=2,
                 layout=c(3,1),index.cond=list(c(1,3,2)),
                 #skip=c(0,0,0,0,0,0,0,1),
                 pch=c(1,16,17),col=color,cex=2,
                 scales=list(x=list(alternating=1),y=list(alternating=3),cex=1.2),
                 ylim=c(12,26),  
                 key=list(text=c(list(levels(EIallTon2$RecoveryMethod)),col="black"),
                          #space="right", 
                          corner=c(0.99,0.93) ,
                          title="Legend",
                          cex=1,
                          lines=list(type= "b",lty=1,pch=c(1,16,17),cex=1.5,col=color,lwd=2),
                          divide=1,border=T,cex.title=1.2,padding.text=2
                          ),
                 
                 xlab=list("Plot Size (acre)",cex=1.5),
                 ylab=list("Mean Error Index (Green tons/acre)",cex=1.5)
                 )
print(EItonPlot)   



#Plot Dollar Value Errors
library(lattice)
color<-c("black","blue","red")
EIdolPlot<-xyplot(ErrorIndex~PlotSize|Model,
                  par.strip.text=list(cex=1.3),
                  #par.settings=list(strip.background=list(col="lightgrey")),
                  data=EIallDol2,
                  groups=RecoveryMethod,
                  type=c("b","g"),lwd=2,
                  layout=c(3,1),index.cond=list(c(1,3,2)),
                  #skip=c(0,0,0,0,0,0,0,1),
                  pch=c(1,16,17),col=color,cex=2,
                  scales=list(x=list(alternating=1),y=list(alternating=3),cex=1.2),
                  
                  key=list(text=c(list(levels(EIallDol2$RecoveryMethod)),col="black"),
                           #space="right", 
                           corner=c(0.99,0.93) ,
                           title="Legend",
                           cex=1,
                           lines=list(type= "b",lty=1,pch=c(1,16,17),cex=1.5,col=color,lwd=2),
                           divide=1,border=T,cex.title=1.2,padding.text=2
                  ),
                  
                  xlab=list("Plot Size (acre)",cex=1.5),
                  ylab=list("Mean Error Index (Dollars/acre)",cex=1.5)
                )
print(EIdolPlot)   
#=================================================================================
#### EI FOR UNTHINNED STANDS ===========================

#subsetting thinned/unthinned stands(I believe 0=unthinned?)
UnThinDat<-subset(AllDat, Thin==0)
row.names(UnThinDat)<-NULL
ThinDat<-subset(AllDat, Thin==1)
row.names(ThinDat)<-NULL

EIunthin<-ddply(UnThinDat, .(Model, PlotSize), summarize,
             
             AvgM=mean(MEIgw),
             
             AvgH=mean(HEIgw),
             
             AvgP=mean(PEIgw),
             
             AvgMdol=mean(MEIdol),
             
             AvgHdol=mean(HEIdol),
             
             AvgPdol=mean(PEIdol)
           )

#Separate green weight from dollar errors
EIunthinTon<-EIunthin[,c("Model","PlotSize","AvgM","AvgH","AvgP")]
names(EIunthinTon)[3:5]<-c("Method of Moments Recovery Approach","Hybrid Recovery Approach",
                           "Percentiles Recovery Approach")
EIunthinDol<-EIunthin[,c("Model","PlotSize","AvgMdol","AvgHdol","AvgPdol")]
names(EIunthinDol)[3:5]<-c("Method of Moments Recovery Approach","Hybrid Recovery Approach",
                        "Percentiles Recovery Approach")

#Reshape the green tons errors data to the long form
EIunthinTon2<-reshape(EIunthinTon,varying=list(3:5),
                   times=names(EIunthinTon)[3:5],
                   v.names="ErrorIndex",direction="long")
row.names(EIunthinTon2)<-NULL
EIunthinTon2$id<-NULL

names(EIunthinTon2)[names(EIunthinTon2)%in%"time"]<-"RecoveryMethod"

#Reshape the dollar errors data to the long form
EIunthinDol2<-reshape(EIunthinDol,varying=list(3:5),
                   times=names(EIunthinDol)[3:5],
                   v.names="ErrorIndex",direction="long")
row.names(EIunthinDol2)<-NULL
EIunthinDol2$id<-NULL
names(EIunthinDol2)[names(EIunthinDol2)%in%"time"]<-"RecoveryMethod"

EIunthinTon2$RecoveryMethod<-as.factor(EIunthinTon2$RecoveryMethod)
EIunthinDol2$RecoveryMethod<-as.factor(EIunthinDol2$RecoveryMethod)


#Plot Green Weight Errors
library(lattice)
color<-c("black","blue","red")
EItonPlotUn<-xyplot(ErrorIndex~PlotSize|Model,
                  par.strip.text=list(cex=1.3),
                  #par.settings=list(strip.background=list(col="lightgrey")),
                  data=EIunthinTon2,
                  groups=RecoveryMethod,
                  type=c("b","g"),lwd=2,
                  layout=c(3,1),index.cond=list(c(1,3,2)),
                  #skip=c(0,0,0,0,0,0,0,1),
                  pch=c(1,16,17),col=color,cex=2,
                  scales=list(x=list(alternating=1),y=list(alternating=3),cex=1.2),
                  ylim=c(12,26),                  
                  key=list(text=c(list(levels(EIunthinTon2$RecoveryMethod)),col="black"),
                           #space="right", 
                           corner=c(0.99,0.93) ,
                           title="Legend",
                           cex=1,
                           lines=list(type= "b",lty=1,pch=c(1,16,17),cex=1.5,col=color,lwd=2),
                           divide=1,border=T,cex.title=1.2,padding.text=2
                  ),
                                   
                  xlab=list("Plot Size (acre)",cex=1.5),
                  ylab=list("Mean Error Index (Green tons/acre)",cex=1.5)
                )
print(EItonPlotUn)   



#Plot Dollar Value Errors
library(lattice)
color<-c("black","blue","red")
EIdolPlotUn<-xyplot(ErrorIndex~PlotSize|Model,
                  par.strip.text=list(cex=1.3),
                  #par.settings=list(strip.background=list(col="lightgrey")),
                  data=EIunthinDol2,
                  groups=RecoveryMethod,
                  type=c("b","g"),lwd=2,
                  layout=c(3,1),index.cond=list(c(1,3,2)),
                  #skip=c(0,0,0,0,0,0,0,1),
                  pch=c(1,16,17),col=color,cex=2,
                  scales=list(x=list(alternating=1),y=list(alternating=3),cex=1.2),
                  #ylim=c(110,350),  
                  key=list(text=c(list(levels(EIunthinDol2$RecoveryMethod)),col="black"),
                           #space="right", 
                           corner=c(0.99,0.93) ,
                           title="Legend",
                           cex=1,
                           lines=list(type= "b",lty=1,pch=c(1,16,17),cex=1.5,col=color,lwd=2),
                           divide=1,border=T,cex.title=1.2,padding.text=2
                  ),
                  
                  xlab=list("Plot Size (acre)",cex=1.5),
                  ylab=list("Mean Error Index (Dollars/acre)",cex=1.5)
                )
print(EIdolPlotUn)   

#=================================================================================
#### EI FOR THINNED STANDS ===========================

EIthin<-ddply(ThinDat, .(Model, PlotSize), summarize,
                
                AvgM=mean(MEIgw),
                
                AvgH=mean(HEIgw),
                
                AvgP=mean(PEIgw),
                
                AvgMdol=mean(MEIdol),
                
                AvgHdol=mean(HEIdol),
                
                AvgPdol=mean(PEIdol)
             )

#Separate green weight from dollar errors
EIthinTon<-EIthin[,c("Model","PlotSize","AvgM","AvgH","AvgP")]
names(EIthinTon)[3:5]<-c("Method of Moments Recovery Approach","Hybrid Recovery Approach",
                         "Percentiles Recovery Approach")
EIthinDol<-EIthin[,c("Model","PlotSize","AvgMdol","AvgHdol","AvgPdol")]
names(EIthinDol)[3:5]<-c("Method of Moments Recovery Approach","Hybrid Recovery Approach",
                         "Percentiles Recovery Approach")

#Reshape the green tons errors data to the long form
EIthinTon2<-reshape(EIthinTon,varying=list(3:5),
                      times=names(EIthinTon)[3:5],
                      v.names="ErrorIndex",direction="long")
row.names(EIthinTon2)<-NULL
EIthinTon2$id<-NULL

names(EIthinTon2)[names(EIthinTon2)%in%"time"]<-"RecoveryMethod"

#Reshape the dollar errors data to the long form
EIthinDol2<-reshape(EIthinDol,varying=list(3:5),
                      times=names(EIthinDol)[3:5],
                      v.names="ErrorIndex",direction="long")
row.names(EIthinDol2)<-NULL
EIthinDol2$id<-NULL
names(EIthinDol2)[names(EIthinDol2)%in%"time"]<-"RecoveryMethod"

EIthinTon2$RecoveryMethod<-as.factor(EIthinTon2$RecoveryMethod)
EIthinDol2$RecoveryMethod<-as.factor(EIthinDol2$RecoveryMethod)


#Plot Green Weight Errors
library(lattice)
color<-c("black","blue","red")
EItonPlotTh<-xyplot(ErrorIndex~PlotSize|Model,
                    par.strip.text=list(cex=1.3),
                    #par.settings=list(strip.background=list(col="lightgrey")),
                    data=EIthinTon2,
                    groups=RecoveryMethod,
                    type=c("b","g"),lwd=2,
                    layout=c(3,1),index.cond=list(c(1,3,2)),
                    #skip=c(0,0,0,0,0,0,0,1),
                    pch=c(1,16,17),col=color,cex=2,
                    scales=list(x=list(alternating=1),y=list(alternating=3),cex=1.2),
                    ylim=c(12,26),  
                    key=list(text=c(list(levels(EIthinTon2$RecoveryMethod)),col="black"),
                             #space="right", 
                             corner=c(0.99,0.93) ,
                             title="Legend",
                             cex=1,
                             lines=list(type= "b",lty=1,pch=c(1,16,17),cex=1.5,col=color,lwd=2),
                             divide=1,border=T,cex.title=1.2,padding.text=2
                    ),
                    
                    xlab=list("Plot Size (acre)",cex=1.5),
                    ylab=list("Mean Error Index (Green tons/acre)",cex=1.5)
                 )
print(EItonPlotTh)   



#Plot Dollar Value Errors
library(lattice)
color<-c("black","blue","red")
EIdolPlotTh<-xyplot(ErrorIndex~PlotSize|Model,
                    par.strip.text=list(cex=1.3),
                    #par.settings=list(strip.background=list(col="lightgrey")),
                    data=EIthinDol2,
                    groups=RecoveryMethod,
                    type=c("b","g"),lwd=2,
                    layout=c(3,1),index.cond=list(c(1,3,2)),
                    #skip=c(0,0,0,0,0,0,0,1),
                    pch=c(1,16,17),col=color,cex=2,
                    scales=list(x=list(alternating=1),y=list(alternating=3),cex=1.2),
                    #ylim=c(110,350),  
                    key=list(text=c(list(levels(EIthinDol2$RecoveryMethod)),col="black"),
                             #space="right", 
                             corner=c(0.99,0.93) ,
                             title="Legend",
                             cex=1,
                             lines=list(type= "b",lty=1,pch=c(1,16,17),cex=1.5,col=color,lwd=2),
                             divide=1,border=T,cex.title=1.2,padding.text=2
                    ),
                    
                    xlab=list("Plot Size (acre)",cex=1.5),
                    ylab=list("Mean Error Index (Dollars/acre)",cex=1.5)
                  )
print(EIdolPlotTh)  
#=================================================================================================
         