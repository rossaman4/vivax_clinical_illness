
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cumLifetimeInf.r
# called from setSimulInputs.r (which is called by simul.r)
# to get expected numbers of cumulative primary infections by age and season
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# extend seasonal Pf EIR pattern for cumulative infections since birth up to >4.25 years (mid-pt of oldest age-group)
pfseas_extC<-c(rep(pfseas_ext[1:6],3),pfseas_ext)
numExtendedIntervals<-18
seasEIRC<-rep(pfseas_extC/(60/timeUnit),each=(60/timeUnit))
# multiply to get Pv EIR per genotype
seasEIRC<-seasEIRC*beta1*114
maxtimeC<-length(seasEIRC)+1
intervalNumC<-seq(1:maxtimeC)
 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# bsaRatioC: calculated per age group per interval to align BSA and seasonality
# C=extended version for cumulative infections
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getBsaRatioC<-function(midIntervalAgeYrs,endInterval,timeUnit) {
  bsaRatio<-rep(1,maxtimeC)
  bsaAge<-rep(1,maxtimeC)
  midIntervalTime<-((endInterval+numExtendedIntervals)-0.5)*(60/timeUnit)
  midIntervalAgeMonths<-trunc(midIntervalAgeYrs*365/30)
  bsaAge[midIntervalTime]<-midIntervalAgeMonths 
  for (i in ((midIntervalTime+1):maxtimeC)) {
      bsaAge[i]<-round(midIntervalAgeMonths + ((i-midIntervalTime)/(30/timeUnit)) )
  }
  for (i in 1:midIntervalTime) {
     bsaAge[i]<-trunc(midIntervalAgeMonths - (midIntervalTime-i)/(30/timeUnit) )
  }
  for (i in 1:maxtimeC) {
      if (bsaAge[i]>0) {
          bsaRatio[i]<-sqrt(girlsHt[bsaAge[i]+1]*girlsWei[bsaAge[i]+1]/3600)/0.606998
       } 
      # include first month of life
      else if (bsaAge[i]==0 & (midIntervalAgeMonths+((i-midIntervalTime)/(30/timeUnit)))>0) {
          bsaRatio[i]<-sqrt(girlsHt[1]*girlsWei[1]/3600)/0.606998
      }
      else {
         bsaRatio[i]<-0
       }
   }  
 return(bsaRatio)
}



getCumPrimI<-function(endIntervalC) {
  midIntervalTime<-trunc(((endIntervalC+numExtendedIntervals)-0.5)*(60/timeUnit))
  numPrimI<-rep(0,maxtimeC)
  for (b in 1:midIntervalTime) {     
      numPrimI[b]<-rpois(1,seasEIRC[b]*bsaRatioC[b])
  }     
  return(sum(numPrimI))
}


getManySims<-function(endIntervalC) {
  # get 1000 simulations and take mean
  repCum<-1000
  cumStore<-rep(0,repCum)
  for (i in 1:repCum) {
    cumStore[i]<-getCumPrimI(endIntervalC)
  }
  return(mean(cumStore))
}



# get for all age groups
endIntervalList<-seq(1:16)
midIntervalAgeYrsList<-c(1.25,1.75,2.25,2.75,3.25,3.75,4.25)
cumIntAgeStore<-array(0, dim=c(16,7))
for (j in 1:16) {
  for (k in 1:7) {
    bsaRatioC<-getBsaRatioC(midIntervalAgeYrsList[k],endIntervalList[j],timeUnit) 
    cumIntAgeStore[j,k]<-getManySims(endIntervalList[j])
  }
}
cumIntAgeStore


write.table(cumIntAgeStore, file="cumInf_byIntAgegp_noitn_novill_new.txt", sep=" ")




