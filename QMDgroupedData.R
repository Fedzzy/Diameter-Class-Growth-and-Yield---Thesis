#This function detrmines the 1-ich dbh class from the given tree dbh in inches.
QMDgroup<-function(DBHclass,Freq)
            {
              qmd=sqrt(sum(DBHclass^2*Freq,na.rm=T)/sum(Freq,na.rm=T))
              return(qmd)
            }

