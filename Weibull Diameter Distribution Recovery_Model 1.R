#RECOVER THE TREE DISTRIBUTION FOR EACH MEASMT OBS BASED ON THE WEIBULL PARAMETERS RECOVERED FROM
# THE PREDICTED MOMENTS

#Load the data
load("Model 1 Predicted Weibull Parameter Data.Rdata")

#Loading in plyr() from library
library(plyr)

#Loading in needed functions
source(file="WeibullParametersEstimation.R")
source(file="WeibullProportions.R")
source(file="DbhOneUnitClassCreator.R")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#===========================================================================================================
#=============  PLOT SIZE 0.05-ACRE MODELS DISTRIBUTION RECOVERY ====================

#Get a listing of all Plots
Plots<-as.list(levels(PWM1_05$MeasmtObs))
#Initialize Stand Table for All Plots

standTableAllPlots_05<-NULL
#Checking that class boundary of the plot's lowest dbh class is greater or equal to the Weibull a parameter
#Finding: Some plots have A parameter larger than lower boundary of smallest dbh class. Build in condition to resolve this. DONE

for(k in 1:length(Plots))
{
  measObs<-Plots[[k]]
  print(paste("Now processing measurement observation number",measObs))
  aM=PWM1_05[k,"PWM_A"]
  bM=PWM1_05[k,"PWM_B"]
  cM=PWM1_05[k,"PWM_C"]
  
  aP=PWM1_05[k,"PWP_A"]
  bP=PWM1_05[k,"PWP_B"]
  cP=PWM1_05[k,"PWP_C"]
  
  aH=PWM1_05[k,"PWH_A"] 
  bH=PWM1_05[k,"PWH_B"]
  cH=PWM1_05[k,"PWH_C"]
  #Inialization
  treesTotal=PWM1_05[k,"NumTrees"]/PWM1_05[k,"PS"]
  treesRemainM<-treesTotal
  treesRemainP<-treesTotal
  treesRemainH<-treesTotal
  minDbhClass<-getDbhClass((PWM1_05[k,"dbhMin"])*0.393701)
  i<-minDbhClass-0.5 #Start at the bottom of the minimum dbh class
  currentDbh<-minDbhClass
  counter=1
  dbhClasses<-NULL 
  tpaInClassM<-NULL 
  tpaInClassP<-NULL
  tpaInClassH<-NULL

#Allocate trees to each dbh class in the kth plot
      while(treesRemainM>0|treesRemainP>0|treesRemainH>0)
      {
        dbhClasses<-append(dbhClasses,currentDbh)
        if(aM*0.393701>i)
        {
          numTreesM<-getWeiProp(aM,(i+1)*2.54,aM,bM,cM)*treesTotal
        }
        else
        {
          if(treesRemainM>=1)
          {
            numTreesM<-getWeiProp(i*2.54,(i+1)*2.54,aM,bM,cM)*treesTotal
          }
          else
          {
            numTreesM=treesRemainM
          }
        }
        tpaInClassM<-append(tpaInClassM,numTreesM)
        treesRemainM<-treesRemainM-numTreesM
        
        if(aP*0.393701>i)
        {
          numTreesP<-getWeiProp(aP,(i+1)*2.54,aP,bP,cP)*treesTotal
        }
        else
        {
          if(treesRemainP>=1)
          {
            numTreesP<-getWeiProp(i*2.54,(i+1)*2.54,aP,bP,cP)*treesTotal
          }
          else 
          {
            numTreesP=treesRemainP
          }
        }
          tpaInClassP<-append(tpaInClassP,numTreesP)
        treesRemainP<-treesRemainP-numTreesP
        
        if(aH*0.393701>i)
        {
          numTreesH<-getWeiProp(aH,(i+1)*2.54,aH,bH,cH)*treesTotal
        }
        else
        {
          if(treesRemainH>=1)
          {
            numTreesH<-getWeiProp(i*2.54,(i+1)*2.54,aH,bH,cH)*treesTotal
          }
          else 
          {
            numTreesH=treesRemainH
          }
        }
         tpaInClassH<-append(tpaInClassH,numTreesH)
         treesRemainH<-treesRemainH-numTreesH
         if (counter>10 & (numTreesM<0.1 & numTreesP<0.1 & numTreesH<0.1)){break}
        i=i+1
        currentDbh=currentDbh+1
        counter=counter+1
      }
  #Combine the dbh class and TPA vectors and coerce in to a data frame
  standTable<-as.data.frame(cbind(dbhClasses, tpaInClassM,tpaInClassP,tpaInClassH))
  names(standTable)<-c("dbhClass","tpaPredMom","tpaPredPct","tpaPredHyb")
  standTable$MeasmtObs<-measObs
  standTable$tpaPredMom<-floor(standTable$tpaPredMom+0.44444)
  standTable$tpaPredPct<-floor(standTable$tpaPredPct+0.44444)
  standTable$tpaPredHyb<-floor(standTable$tpaPredHyb+0.44444)
  
  standTableAllPlots_05<-rbind(standTableAllPlots_05,standTable)
} #END for loop

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#===========================================================================================================
#=============  PLOT SIZE 0.1-ACRE MODELS DISTRIBUTION RECOVERY ====================

#Get a listing of all Plots
Plots<-as.list(levels(PWM1_10$MeasmtObs))
#Initialize Stand Table for All Plots

standTableAllPlots_10<-NULL
#Checking that class boundary of the plot's lowest dbh class is greater or equal to the Weibull a parameter
#Finding: Some plots have A parameter larger than lower boundary of smallest dbh class. Build in condition to resolve this. DONE

for(k in 1:length(Plots))
{
  measObs<-Plots[[k]]
  print(paste("Now processing measurement observation number",measObs))
  aM=PWM1_10[k,"PWM_A"]
  bM=PWM1_10[k,"PWM_B"]
  cM=PWM1_10[k,"PWM_C"]
  
  aP=PWM1_10[k,"PWP_A"]
  bP=PWM1_10[k,"PWP_B"]
  cP=PWM1_10[k,"PWP_C"]
  
  aH=PWM1_10[k,"PWH_A"] 
  bH=PWM1_10[k,"PWH_B"]
  cH=PWM1_10[k,"PWH_C"]
  #Inialization
  treesTotal=PWM1_10[k,"NumTrees"]/PWM1_10[k,"PS"]
  treesRemainM<-treesTotal
  treesRemainP<-treesTotal
  treesRemainH<-treesTotal
  minDbhClass<-getDbhClass((PWM1_10[k,"dbhMin"])*0.393701)
  i<-minDbhClass-0.5 #Start at the bottom of the minimum dbh class
  currentDbh<-minDbhClass
  counter=1
  dbhClasses<-NULL 
  tpaInClassM<-NULL 
  tpaInClassP<-NULL
  tpaInClassH<-NULL
  
  #Allocate trees to each dbh class in the kth plot
  while(treesRemainM>0|treesRemainP>0|treesRemainH>0)
  {
    dbhClasses<-append(dbhClasses,currentDbh)
    if(aM*0.393701>i)
    {
      numTreesM<-getWeiProp(aM,(i+1)*2.54,aM,bM,cM)*treesTotal
    }
    else
    {
      if(treesRemainM>=1)
      {
        numTreesM<-getWeiProp(i*2.54,(i+1)*2.54,aM,bM,cM)*treesTotal
      }
      else
      {
        numTreesM=treesRemainM
      }
    }
    tpaInClassM<-append(tpaInClassM,numTreesM)
    treesRemainM<-treesRemainM-numTreesM
    
    if(aP*0.393701>i)
    {
      numTreesP<-getWeiProp(aP,(i+1)*2.54,aP,bP,cP)*treesTotal
    }
    else
    {
      if(treesRemainP>=1)
      {
        numTreesP<-getWeiProp(i*2.54,(i+1)*2.54,aP,bP,cP)*treesTotal
      }
      else 
      {
        numTreesP=treesRemainP
      }
    }
    tpaInClassP<-append(tpaInClassP,numTreesP)
    treesRemainP<-treesRemainP-numTreesP
    
    if(aH*0.393701>i)
    {
      numTreesH<-getWeiProp(aH,(i+1)*2.54,aH,bH,cH)*treesTotal
    }
    else
    {
      if(treesRemainH>=1)
      {
        numTreesH<-getWeiProp(i*2.54,(i+1)*2.54,aH,bH,cH)*treesTotal
      }
      else 
      {
        numTreesH=treesRemainH
      }
    }
    tpaInClassH<-append(tpaInClassH,numTreesH)
    treesRemainH<-treesRemainH-numTreesH
    if (counter>10 & (numTreesM<0.1 & numTreesP<0.1 & numTreesH<0.1)){break}
    i=i+1
    currentDbh=currentDbh+1
    counter=counter+1
  }
  #Combine the dbh class and TPA vectors and coerce in to a data frame
  standTable<-as.data.frame(cbind(dbhClasses, tpaInClassM,tpaInClassP,tpaInClassH))
  names(standTable)<-c("dbhClass","tpaPredMom","tpaPredPct","tpaPredHyb")
  standTable$MeasmtObs<-measObs
  standTable$tpaPredMom<-floor(standTable$tpaPredMom+0.44444)
  standTable$tpaPredPct<-floor(standTable$tpaPredPct+0.44444)
  standTable$tpaPredHyb<-floor(standTable$tpaPredHyb+0.44444)
  
  standTableAllPlots_10<-rbind(standTableAllPlots_10,standTable)
} #END for loop

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#===========================================================================================================
#=============  PLOT SIZE 0.15-ACRE MODELS DISTRIBUTION RECOVERY ====================

#Get a listing of all Plots
Plots<-as.list(levels(PWM1_15$MeasmtObs))
#Initialize Stand Table for All Plots

standTableAllPlots_15<-NULL
#Checking that class boundary of the plot's lowest dbh class is greater or equal to the Weibull a parameter
#Finding: Some plots have A parameter larger than lower boundary of smallest dbh class. Build in condition to resolve this. DONE

for(k in 1:length(Plots))
{
  measObs<-Plots[[k]]
  print(paste("Now processing measurement observation number",measObs))
  aM=PWM1_15[k,"PWM_A"]
  bM=PWM1_15[k,"PWM_B"]
  cM=PWM1_15[k,"PWM_C"]
  
  aP=PWM1_15[k,"PWP_A"]
  bP=PWM1_15[k,"PWP_B"]
  cP=PWM1_15[k,"PWP_C"]
  
  aH=PWM1_15[k,"PWH_A"] 
  bH=PWM1_15[k,"PWH_B"]
  cH=PWM1_15[k,"PWH_C"]
  #Inialization
  treesTotal=PWM1_15[k,"NumTrees"]/PWM1_15[k,"PS"]
  treesRemainM<-treesTotal
  treesRemainP<-treesTotal
  treesRemainH<-treesTotal
  minDbhClass<-getDbhClass((PWM1_15[k,"dbhMin"])*0.393701)
  i<-minDbhClass-0.5 #Start at the bottom of the minimum dbh class
  currentDbh<-minDbhClass
  counter=1
  dbhClasses<-NULL 
  tpaInClassM<-NULL 
  tpaInClassP<-NULL
  tpaInClassH<-NULL
  
  #Allocate trees to each dbh class in the kth plot
  while(treesRemainM>0|treesRemainP>0|treesRemainH>0)
  {
    dbhClasses<-append(dbhClasses,currentDbh)
    if(aM*0.393701>i)
    {
      numTreesM<-getWeiProp(aM,(i+1)*2.54,aM,bM,cM)*treesTotal
    }
    else
    {
      if(treesRemainM>=1)
      {
        numTreesM<-getWeiProp(i*2.54,(i+1)*2.54,aM,bM,cM)*treesTotal
      }
      else
      {
        numTreesM=treesRemainM
      }
    }
    tpaInClassM<-append(tpaInClassM,numTreesM)
    treesRemainM<-treesRemainM-numTreesM
    
    if(aP*0.393701>i)
    {
      numTreesP<-getWeiProp(aP,(i+1)*2.54,aP,bP,cP)*treesTotal
    }
    else
    {
      if(treesRemainP>=1)
      {
        numTreesP<-getWeiProp(i*2.54,(i+1)*2.54,aP,bP,cP)*treesTotal
      }
      else 
      {
        numTreesP=treesRemainP
      }
    }
    tpaInClassP<-append(tpaInClassP,numTreesP)
    treesRemainP<-treesRemainP-numTreesP
    
    if(aH*0.393701>i)
    {
      numTreesH<-getWeiProp(aH,(i+1)*2.54,aH,bH,cH)*treesTotal
    }
    else
    {
      if(treesRemainH>=1)
      {
        numTreesH<-getWeiProp(i*2.54,(i+1)*2.54,aH,bH,cH)*treesTotal
      }
      else 
      {
        numTreesH=treesRemainH
      }
    }
    tpaInClassH<-append(tpaInClassH,numTreesH)
    treesRemainH<-treesRemainH-numTreesH
    if (counter>10 & (numTreesM<0.1 & numTreesP<0.1 & numTreesH<0.1)){break}
    i=i+1
    currentDbh=currentDbh+1
    counter=counter+1
  }
  #Combine the dbh class and TPA vectors and coerce in to a data frame
  standTable<-as.data.frame(cbind(dbhClasses, tpaInClassM,tpaInClassP,tpaInClassH))
  names(standTable)<-c("dbhClass","tpaPredMom","tpaPredPct","tpaPredHyb")
  standTable$MeasmtObs<-measObs
  standTable$tpaPredMom<-floor(standTable$tpaPredMom+0.44444)
  standTable$tpaPredPct<-floor(standTable$tpaPredPct+0.44444)
  standTable$tpaPredHyb<-floor(standTable$tpaPredHyb+0.44444)
  
  standTableAllPlots_15<-rbind(standTableAllPlots_15,standTable)
} #END for loop


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#===========================================================================================================
#=============  PLOT SIZE 0.20-ACRE MODELS DISTRIBUTION RECOVERY ====================

#Get a listing of all Plots
Plots<-as.list(levels(PWM1_20$MeasmtObs))
#Initialize Stand Table for All Plots

standTableAllPlots_20<-NULL
#Checking that class boundary of the plot's lowest dbh class is greater or equal to the Weibull a parameter
#Finding: Some plots have A parameter larger than lower boundary of smallest dbh class. Build in condition to resolve this. DONE

for(k in 1:length(Plots))
{
  measObs<-Plots[[k]]
  print(paste("Now processing measurement observation number",measObs))
  aM=PWM1_20[k,"PWM_A"]
  bM=PWM1_20[k,"PWM_B"]
  cM=PWM1_20[k,"PWM_C"]
  
  aP=PWM1_20[k,"PWP_A"]
  bP=PWM1_20[k,"PWP_B"]
  cP=PWM1_20[k,"PWP_C"]
  
  aH=PWM1_20[k,"PWH_A"] 
  bH=PWM1_20[k,"PWH_B"]
  cH=PWM1_20[k,"PWH_C"]
  #Inialization
  treesTotal=PWM1_20[k,"NumTrees"]/PWM1_20[k,"PS"]
  treesRemainM<-treesTotal
  treesRemainP<-treesTotal
  treesRemainH<-treesTotal
  minDbhClass<-getDbhClass((PWM1_20[k,"dbhMin"])*0.393701)
  i<-minDbhClass-0.5 #Start at the bottom of the minimum dbh class
  currentDbh<-minDbhClass
  counter=1
  dbhClasses<-NULL 
  tpaInClassM<-NULL 
  tpaInClassP<-NULL
  tpaInClassH<-NULL
  
  #Allocate trees to each dbh class in the kth plot
  while(treesRemainM>0|treesRemainP>0|treesRemainH>0)
  {
    dbhClasses<-append(dbhClasses,currentDbh)
    if(aM*0.393701>i)
    {
      numTreesM<-getWeiProp(aM,(i+1)*2.54,aM,bM,cM)*treesTotal
    }
    else
    {
      if(treesRemainM>=1)
      {
        numTreesM<-getWeiProp(i*2.54,(i+1)*2.54,aM,bM,cM)*treesTotal
      }
      else
      {
        numTreesM=treesRemainM
      }
    }
    tpaInClassM<-append(tpaInClassM,numTreesM)
    treesRemainM<-treesRemainM-numTreesM
    
    if(aP*0.393701>i)
    {
      numTreesP<-getWeiProp(aP,(i+1)*2.54,aP,bP,cP)*treesTotal
    }
    else
    {
      if(treesRemainP>=1)
      {
        numTreesP<-getWeiProp(i*2.54,(i+1)*2.54,aP,bP,cP)*treesTotal
      }
      else 
      {
        numTreesP=treesRemainP
      }
    }
    tpaInClassP<-append(tpaInClassP,numTreesP)
    treesRemainP<-treesRemainP-numTreesP
    
    if(aH*0.393701>i)
    {
      numTreesH<-getWeiProp(aH,(i+1)*2.54,aH,bH,cH)*treesTotal
    }
    else
    {
      if(treesRemainH>=1)
      {
        numTreesH<-getWeiProp(i*2.54,(i+1)*2.54,aH,bH,cH)*treesTotal
      }
      else 
      {
        numTreesH=treesRemainH
      }
    }
    tpaInClassH<-append(tpaInClassH,numTreesH)
    treesRemainH<-treesRemainH-numTreesH
    if (counter>10 & (numTreesM<0.1 & numTreesP<0.1 & numTreesH<0.1)){break}
    i=i+1
    currentDbh=currentDbh+1
    counter=counter+1
  }
  #Combine the dbh class and TPA vectors and coerce in to a data frame
  standTable<-as.data.frame(cbind(dbhClasses, tpaInClassM,tpaInClassP,tpaInClassH))
  names(standTable)<-c("dbhClass","tpaPredMom","tpaPredPct","tpaPredHyb")
  standTable$MeasmtObs<-measObs
  standTable$tpaPredMom<-floor(standTable$tpaPredMom+0.44444)
  standTable$tpaPredPct<-floor(standTable$tpaPredPct+0.44444)
  standTable$tpaPredHyb<-floor(standTable$tpaPredHyb+0.44444)
  
  standTableAllPlots_20<-rbind(standTableAllPlots_20,standTable)
} #END for loop


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#===========================================================================================================
#=============  PLOT SIZE 0.25-ACRE MODELS DISTRIBUTION RECOVERY ====================

#Get a listing of all Plots
Plots<-as.list(levels(PWM1_25$MeasmtObs))
#Initialize Stand Table for All Plots

standTableAllPlots_25<-NULL
#Checking that class boundary of the plot's lowest dbh class is greater or equal to the Weibull a parameter
#Finding: Some plots have A parameter larger than lower boundary of smallest dbh class. Build in condition to resolve this. DONE

for(k in 1:length(Plots))
{
  measObs<-Plots[[k]]
  print(paste("Now processing measurement observation number",measObs))
  aM= PWM1_25[k,"PWM_A"]
  bM=PWM1_25[k,"PWM_B"]
  cM=PWM1_25[k,"PWM_C"]
  
  aP=PWM1_25[k,"PWP_A"]
  bP=PWM1_25[k,"PWP_B"]
  cP=PWM1_25[k,"PWP_C"]
  
  aH=PWM1_25[k,"PWH_A"] 
  bH=PWM1_25[k,"PWH_B"]
  cH=PWM1_25[k,"PWH_C"]
  #Inialization
  treesTotal=PWM1_25[k,"NumTrees"]/PWM1_25[k,"PS"]
  treesRemainM<-treesTotal
  treesRemainP<-treesTotal
  treesRemainH<-treesTotal
  minDbhClass<-getDbhClass((PWM1_25[k,"dbhMin"])*0.393701)
  i<-minDbhClass-0.5 #Start at the bottom of the minimum dbh class
  currentDbh<-minDbhClass
  counter=1
  dbhClasses<-NULL 
  tpaInClassM<-NULL 
  tpaInClassP<-NULL
  tpaInClassH<-NULL
  
  #Allocate trees to each dbh class in the kth plot
  while(treesRemainM>0|treesRemainP>0|treesRemainH>0)
  {
    dbhClasses<-append(dbhClasses,currentDbh)
    if(aM*0.393701>i)
    {
      numTreesM<-getWeiProp(aM,(i+1)*2.54,aM,bM,cM)*treesTotal
    }
    else
    {
      if(treesRemainM>=1)
      {
        numTreesM<-getWeiProp(i*2.54,(i+1)*2.54,aM,bM,cM)*treesTotal
      }
      else
      {
        numTreesM=treesRemainM
      }
    }
    tpaInClassM<-append(tpaInClassM,numTreesM)
    treesRemainM<-treesRemainM-numTreesM
    
    if(aP*0.393701>i)
    {
      numTreesP<-getWeiProp(aP,(i+1)*2.54,aP,bP,cP)*treesTotal
    }
    else
    {
      if(treesRemainP>=1)
      {
        numTreesP<-getWeiProp(i*2.54,(i+1)*2.54,aP,bP,cP)*treesTotal
      }
      else 
      {
        numTreesP=treesRemainP
      }
    }
    tpaInClassP<-append(tpaInClassP,numTreesP)
    treesRemainP<-treesRemainP-numTreesP
    
    if(aH*0.393701>i)
    {
      numTreesH<-getWeiProp(aH,(i+1)*2.54,aH,bH,cH)*treesTotal
    }
    else
    {
      if(treesRemainH>=1)
      {
        numTreesH<-getWeiProp(i*2.54,(i+1)*2.54,aH,bH,cH)*treesTotal
      }
      else 
      {
        numTreesH=treesRemainH
      }
    }
    tpaInClassH<-append(tpaInClassH,numTreesH)
    treesRemainH<-treesRemainH-numTreesH
    if (counter>10 & (numTreesM<0.1 & numTreesP<0.1 & numTreesH<0.1)){break}
    i=i+1
    currentDbh=currentDbh+1
    counter=counter+1
  }
  #Combine the dbh class and TPA vectors and coerce in to a data frame
  standTable<-as.data.frame(cbind(dbhClasses, tpaInClassM,tpaInClassP,tpaInClassH))
  names(standTable)<-c("dbhClass","tpaPredMom","tpaPredPct","tpaPredHyb")
  standTable$MeasmtObs<-measObs
  standTable$tpaPredMom<-floor(standTable$tpaPredMom+0.44444)
  standTable$tpaPredPct<-floor(standTable$tpaPredPct+0.44444)
  standTable$tpaPredHyb<-floor(standTable$tpaPredHyb+0.44444)
  
  standTableAllPlots_25<-rbind(standTableAllPlots_25,standTable)
} #END for loop


#Saving dbh tables
save(standTableAllPlots_05, standTableAllPlots_10, standTableAllPlots_15, standTableAllPlots_20, standTableAllPlots_25,
     
     file="PredictedDbhDistbn_Mod1.RData")




                       


