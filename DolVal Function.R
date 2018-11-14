#Function used to determine Volume of trees according to dbh class

DollVal<-function(dbhClass, GreenWt)
  
{
  
  if(dbhClass < 6)
    
  {
    val=GreenWt*1
    return(val)
  }
  
  else if (dbhClass >= 6 & dbhClass <= 8)
    
  {
    val=GreenWt*8
    return(val)
  }
  
  else if(dbhClass >= 9 & dbhClass <= 11)
    
  {
    val=GreenWt*15
    return(val)
  }
  
  else if(dbhClass >= 12)
    
  {
    val=GreenWt*24
    return(val)
  } 
}

#Vectorizing function
DollVal<-Vectorize(DollVal, vectorize.args=c("dbhClass", "GreenWt"))