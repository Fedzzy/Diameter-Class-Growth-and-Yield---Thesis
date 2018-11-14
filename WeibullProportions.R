
#==============================================================================================
#Function to compute proportion of trees between 2 diameters from Weibull cumulative distribution function
#dbhL is the lower dbh limit, dbhU the upper dbh limit, and a, b, and C are Weibull distribution parameters 
getWeiProp<-function(dbhL,dbhU,a,b,C)
               {
               prop<-(exp(-((dbhL-a)/b)^C)-exp(-((dbhU-a)/b)^C))
               return(prop)
               }
                      