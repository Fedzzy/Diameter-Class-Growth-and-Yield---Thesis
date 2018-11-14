#These functions compute the Parameters of a stand's Weibull diameterdistribution from stand characteristics
#=============================================================
# A.   Hyink (1980)'s Moment Recovery Approach functions==============

library(stats) #Provides bisection algorithm called uniroot
# 'a' Parameter
WeiMomentA<-function(dbhMin)       #dbhMin - Minimum DBH
                  {
                    a=0.5*dbhMin
                    return(a)
                  }
                  

# 'b' Parameter                    
WeiMomentB<-function(dbhMin,AMD,QMD) #AMD - Arithmetic Mean DBH
          {                          #QMD - Quadratic Mean DBH
           a=WeiMomentA(dbhMin)
           bFun<-function(a,C,AMD)
             {
              return((AMD-a)/gamma(1+1/C))
             }
           cFun<- function(C)
             {
              return(QMD^2-(bFun(a,C,AMD))^2*gamma(1+2/C)-2*a*AMD+a^2)          }
           C=uniroot(cFun,c(0.1,20))$root
           b=(AMD-a)/gamma(1+1/C)
          return(b)
          }       
WeiMomentB<-Vectorize(WeiMomentB,vectorize.args=c("dbhMin","AMD","QMD")) 

# 'c' Parameter                    
WeiMomentC<-function(dbhMin,AMD,QMD)
          {
           a=WeiMomentA(dbhMin)
           bFun<-function(a,C,AMD)
             {
              return((AMD-a)/gamma(1+1/C))
             }
           cFun<- function(C)
             {
              return(QMD^2-(bFun(a,C,AMD))^2*gamma(1+2/C)-2*a*AMD+a^2)          
             }
           C=uniroot(cFun,c(0.1,20))$root
          return(C)
          }       
WeiMomentC<-Vectorize(WeiMomentC,vectorize.args=c("dbhMin","AMD","QMD"))       

#==================================================================================================
            
# Bailey et al. (1989) Percentiles Approach
# 'a' Parameter
WeiPctleA<-function(numTrees,D0,D50) #D0 - 0th Percentile
           {                         #D50 - 50th Percentile
           a=(numTrees^(1/3)*D0-D50)/(numTrees^(1/3)-1)
           if (a<0) 
             {
             a=0 # 'a' parameter set to zero if calculated 'a' is less than zero
             return(a)
             }
           return(a)
           }
WeiPctleA<-Vectorize(WeiPctleA,vectorize.args=c("numTrees","D0","D50"))

WeiPctleC<-function(numTrees,D0,D25,D50,D95)#D25 - 25th Percentile
           {                                #D95 - 95th Percentile
           a=WeiPctleA(numTrees,D0,D50)
           C=2.343088/(log(D95-a)-log(D25-a))
           return(C)
           }

WeiPctleC<-Vectorize(WeiPctleC,vectorize.args=c("numTrees","D0","D25","D50","D95"))


WeiPctleB<-function(numTrees,D0,D25,D50,D95,QMD)
           {
           a=WeiPctleA(numTrees,D0,D50)
           C=2.343088/(log(D95-a)-log(D25-a))
           gamma1=gamma(1+1/C)
           gamma2=gamma(1+2/C)
           rhs=(a/gamma2)^2*(gamma1^2-gamma2)+QMD^2/gamma2
           b=-(a*gamma1/gamma2)+sqrt(rhs)
           return(b)
           }

WeiPctleB<-Vectorize(WeiPctleB,vectorize.args=c("numTrees","D0","D25","D50","D95","QMD"))



#==================================================================================================
# Baldwin and Feduccia (1987) Moment-Percentile Hybrid Approach of Weibull parameter recovery
# 'a' Parameter

WeiHyA<-function(dbhMin)       #dbhMin - Minimum DBH
                  {
                    a=0.5*dbhMin
                    return(a)
                  }
# b parameter
WeiHyB<-function(dbhMin,D93,QMD)  #D93 - 93rd percentile diameter, QMD - quadratic mean dbh
          {
             a=WeiHyA(dbhMin)
             cFun<- function(C)
              {
              return(a^2-QMD^2+((2*a*(D93-a)*(gamma(1+1/C)))/(2.65926^(1/C)))+
                               (((D93-a)^2*(gamma(1+2/C)))/(2.65926^(2/C)))
                     )           
              }
             C=uniroot(cFun,c(1,20))$root
             b=(D93-a)/(2.65926^(1/C))
             return(b)
          }
             
WeiHyB<-Vectorize(WeiHyB,vectorize.args=c("dbhMin","D93","QMD"))    

#c parameter
WeiHyC<-function(dbhMin,D93,QMD)
          {
             a=WeiHyA(dbhMin)
             cFun<- function(C)
              {
              return(a^2-QMD^2+((2*a*(D93-a)*(gamma(1+1/C)))/(2.65926^(1/C)))+
                               (((D93-a)^2*(gamma(1+2/C)))/(2.65926^(2/C)))
                     )           
              }
             C=uniroot(cFun,c(1,20))$root
             return(C)
          }  

WeiHyC<-Vectorize(WeiHyC,vectorize.args=c("dbhMin","D93","QMD")) 

