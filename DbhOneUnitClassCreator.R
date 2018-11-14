#This function detrmines the 1-ich dbh class from the given tree dbh in inches.
getDbhClass<-function(dbh)
 {         
   dbhClass=floor(dbh+0.4)     
   return(dbhClass)
 }
 
 getDbhClass<-Vectorize(getDbhClass,vectorize.args=c("dbh"))
