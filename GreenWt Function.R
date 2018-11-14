#Function used to determine Green Weight of trees according to dbh class

GrnWt<-function(dbhClass, Ht)
              
{
  
  if(dbhClass < 6)
    
  {
      wt=-4.3238 + 0.1397*dbhClass^2*Ht
      if(wt<0){return(0)}
      else
      {return(wt/2000)}
  }
    
  else if (dbhClass >= 6 & dbhClass <= 8)
      
      {
      wt=(-4.3238 + 0.1397*dbhClass^2*Ht)*(exp(-0.9468*(2^4.3397)/(dbhClass^4.1697)))
      if(wt<0){return(0)}
      else
      {return(wt/2000)}
      }

  else if(dbhClass >= 9 & dbhClass <= 11)
    
      {
        wt=(-4.3238 + 0.1397*dbhClass^2*Ht)*(exp(-0.9468*(4^4.3397)/(dbhClass^4.1697)))
      if(wt<0){return(0)}
      else
      {return(wt/2000)}
      }

  else if(dbhClass >= 12)
    
      {
        wt=(-4.3238 + 0.1397*dbhClass^2*Ht)*(exp(-0.9468*(6^4.3397)/(dbhClass^4.1697)))
      if(wt<0){return(0)}
      else
      {return(wt/2000)}
      } 
}

#Vectorizing function
GrnWt<-Vectorize(GrnWt, vectorize.args=c("dbhClass", "Ht"))
