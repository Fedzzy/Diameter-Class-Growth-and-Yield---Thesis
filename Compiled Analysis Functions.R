#Function to calculate quadratic mean dbh
QMD <- function(dbh)
{
  qmd<-sqrt(mean(dbh^2,na.rm=T))
  return(qmd)
}

#Functions
#Fit Statistics Functions
FI<-function(y,y.pred)
{
  R2<-1-sum((y-y.pred)^2,na.rm=T)/sum((y-mean(y,na.rm=T))^2,na.rm=T)
  return(R2)
}
RMSE<-function(dat,y,y.pred,p)
{
  rmse<-sqrt(sum((y-y.pred)^2,na.rm=T)/(nrow(dat)-p))
  return(rmse)
}
BIAS<-function(y,y.pred)
{
  bias<-mean((y.pred-y),na.rm=T)
  return(bias)
}
MAB<-function(y,y.pred)
{
  mab<-mean(abs(y-y.pred),na.rm=T)
  return(mab)
}

#Observations generally miss in some cruise plots and the relevant zero values are normally not
#available. It is necessary to specify number of plots in mean and standard error formula

meanCruise<-function(x,n)
{
  cruiseMean<-(sum(x,na.rm=T))/n  #n is number of plots in cruise
  return(cruiseMean)
}

sdCruise<-function(x,n)
{
  sHat<-sqrt(((sum(x^2,na.rm = T))-(((sum(x,na.rm = T))^2)/n))/(n-1))
  return(sHat)
} 


#95% Confidence Limits Function
cl95Cruise<-function(x,n,lim=c("upper","lower"))
{
  Xbar<-meanCruise(x,n)    #Mean
  se<-sdCruise(x,n)/sqrt(n) #Std Error of the Mean
  tVal<-qt(0.025,n-1,lower.tail=F)  #Find t-value
  if (lim=="upper") {cl=Xbar+tVal*se}
  else if (lim=="lower") {cl=Xbar-tVal*se}
  else stop("Must specify whether upper or lower limit")
  return(cl)
}

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
    return(QMD^2-(bFun(a,C,AMD))^2*gamma(1+2/C)-2*a*AMD+a^2)         
  }
  C=uniroot(cFun,c(0.5,20))$root
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
  C=uniroot(cFun,c(0.5,20))$root
  return(C)
}       
WeiMomentC<-Vectorize(WeiMomentC,vectorize.args=c("dbhMin","AMD","QMD"))       

#====================================================================

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


#==============================================================================================
#Function to compute proportion of trees between 2 diameters from Weibull cumulative distribution function
#dbhL is the lower dbh limit, dbhL the upper dbh limit, and a, b, and C are Weibull distribution parameters 
getWeiProp<-function(dbhL,dbhU,a,b,C)
{
  prop<-(exp(-((dbhL-a)/b)^C)-exp(-((dbhU-a)/b)^C))
  return(prop)
}

#Function to calculate dominant height where dominant/codominant trees are
# those with dbh >= stand quadratic mean dbh
HDnoCC<-function(dbh,ht)
{
  hts<-0
  count<-0
  qmd<-sqrt(mean(dbh^2,na.rm=TRUE))
  for(i in 1: length(dbh))
  {
    if ((is.finite(dbh[i])& dbh[i]>=qmd)  & is.finite(ht[i]))
    {
      hts<-hts+ht[i]
      count<-count+1
    }
  }
  hd<-hts/count
  return (hd)
}

#Function to calculate dominant height in a data set where dominant and codominant trees are identified by crown class  
HDwithCC<-function(ht,crownClass) #Where crownClass 1 or 2 represents dominants or codominants
{
  if (!(is.numeric(crownClass)))
  {
    stop("Error: crown class must be numeric")
  }
  hts<-0
  count<-0
  for(i in 1: length(ht))
  {
    if ((is.finite(crownClass[i])& crownClass[i]<=2) & is.finite(ht[i]))
    {
      hts<-hts+ht[i]
      count<-count+1
    }
  }
  hd<-hts/count
  return (hd)
}

 #Function to compute proportion of trees between 2 diameters from Weibull cumulative distribution function
#dbhL is the lower dbh limit, dbhL the upper dbh limit, and a, b, and C are Weibull distribution parameters 
getWeiProp<-function(dbhL,dbhU,a,b,C)
               {
               prop<-(exp(-((dbhL-a)/b)^C)-exp(-((dbhU-a)/b)^C))
               return(prop)
               }
        