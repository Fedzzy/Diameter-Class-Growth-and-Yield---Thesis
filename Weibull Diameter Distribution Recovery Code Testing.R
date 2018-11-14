source(file="WeibullProportions.R")
a=1.958439
b=11.839672
C=3.373858
#Inialization
treesTotal=636
treesRemain<-636 #tpaAll is the "storage" for age 35 trees
minDbh35Class<-1
i=minDbh35Class-0.5 #Start at the bottom of the minimum dbh class
currentDbh=minDbh35Class
dbhClasses<-NULL #Initialize data vector for dbh classes at age 35
tpaInClass<-NULL # Initialize data vector for trees per acre in the age 35 dbh classes

#Allocate trees to each dbh class
while(treesRemain>=1)
{
  dbhClasses<-append(dbhClasses,currentDbh)
    if(a*0.393701>i)
      {
       numTrees<-getWeiProp(a,(i+1)*2.54,a,b,C)*treesTotal
      }
     else
      {
       numTrees<-getWeiProp(i*2.54,(i+1)*2.54,a,b,C)*treesTotal
      }
  treesRemain<-treesRemain-numTrees
  tpaInClass<-append(tpaInClass,numTrees)
  i=i+1
  currentDbh=currentDbh+1
  if (treesRemain<1)    #If less than 1 tree remain, 
     {
     dbhClasses<-append(dbhClasses,currentDbh)
     tpaInClass<-append(tpaInClass,treesRemain)
     }
}
#Combine the dbh class and TPA vectors and coerce in to a data frame
standTable<-as.data.frame(cbind(dbhClasses, tpaInClass))
names(standTable)<-c("dbhClass","tpaPred")
standTable$MeasmtObs<-1
standTable$tpaPred<-floor(standTable$tpaPred+0.44444)












