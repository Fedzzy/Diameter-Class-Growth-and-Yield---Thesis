
#Load Model 1 Error Tables
load(file="Error Tables_Model 3.RData")

#Create QMD data for the different plot sizes
library(plyr)

#==========Coding in Function which will calculate QMD by group==================================================

QMDgroup<-function(DBHclass,Freq)
            {
              qmd=sqrt(sum(DBHclass^2*Freq,na.rm=T)/sum(Freq,na.rm=T))
              return(qmd)
}

#==================================Creation of QMD column and Merging Data sets for .05 Acre=====================

#Get numeric dbh values
FinalErr_05$dbh<-as.numeric(paste(FinalErr_05$dbhClass))

#Calculaton of QMD using ddply()
QMDat05<-ddply(FinalErr_05,.(MeasmtObs),summarize,
               
                QMDobs=round(QMDgroup(dbh,tpaObs),1))

#Merging .05 acre data tables
FinalTable_05<-merge(FinalErr_05, QMDat05)

#Dropping dbh column
FinalTable_05$dbh<-NULL

#==================================Creation of QMD column and Merging Data sets for .10 Acre=====================

#Get numeric dbh values
FinalErr_10$dbh<-as.numeric(paste(FinalErr_10$dbhClass))

#Calculaton of QMD using ddply()
QMDat10<-ddply(FinalErr_10,.(MeasmtObs),summarize,
               
               QMDobs=round(QMDgroup(dbh,tpaObs),1))

#Merging .05 acre data tables
FinalTable_10<-merge(FinalErr_10, QMDat10)

#Dropping dbh column
FinalTable_10$dbh<-NULL

#==================================Creation of QMD column and Merging Data sets for .15 Acre=====================

#Get numeric dbh values
FinalErr_15$dbh<-as.numeric(paste(FinalErr_15$dbhClass))

#Calculaton of QMD using ddply()
QMDat15<-ddply(FinalErr_15,.(MeasmtObs),summarize,
               
               QMDobs=round(QMDgroup(dbh,tpaObs),1))

#Merging .05 acre data tables
FinalTable_15<-merge(FinalErr_15, QMDat15)

#Dropping dbh column
FinalTable_15$dbh<-NULL

#==================================Creation of QMD column and Merging Data sets for .20 Acre=====================

#Get numeric dbh values
FinalErr_20$dbh<-as.numeric(paste(FinalErr_20$dbhClass))

#Calculaton of QMD using ddply()
QMDat20<-ddply(FinalErr_20,.(MeasmtObs),summarize,
               
               QMDobs=round(QMDgroup(dbh,tpaObs),1))

#Merging .05 acre data tables
FinalTable_20<-merge(FinalErr_20, QMDat20)

#Dropping dbh column
FinalTable_20$dbh<-NULL

#==================================Creation of QMD column and Merging Data sets for .20 Acre=====================

#Get numeric dbh values
FinalErr_25$dbh<-as.numeric(paste(FinalErr_25$dbhClass))

#Calculaton of QMD using ddply()
QMDat25<-ddply(FinalErr_25,.(MeasmtObs),summarize,
               
               QMDobs=round(QMDgroup(dbh,tpaObs),1))

#Merging .05 acre data tables
FinalTable_25<-merge(FinalErr_25, QMDat25)

#Dropping dbh column
FinalTable_25$dbh<-NULL

#==============Saving Final Tables to an .RData file================
save(FinalTable_05, FinalTable_10, FinalTable_15, FinalTable_20, FinalTable_25, file="Final Error Tables_Model 3.RData")
