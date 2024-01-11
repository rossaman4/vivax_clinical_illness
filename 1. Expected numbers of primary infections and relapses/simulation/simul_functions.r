# ~~~~~~~~~~~~~~~~~~~~~~~~
# simul_functions.r
# ~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# addRelapseToInfStore
# adds relapses to InfStore, the temporary store for each individual's simulation results in days
# codes:
# hypnozoites and duration (if an "effective relapse" classification B =2, if during prophylactic period =-4, during imm period or when BS infection by same brood = 4)
# BS infection (by prim i or relapse) = 3, imm period = -8 (treated takes precedence over imm period)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


addRelapsesToInfStore<-function(i) {
 for (j in 1:length(primI)) {
       hypnoTime<-getRelapses()
       if (length(hypnoTime)>0) {
          for (h in 1:length(hypnoTime)) { 
             rTime<-primI[j]+incubPeriod+hypnoTime[h]
             if (rTime<=maxtime) { 
                 if (infStore[rTime,j]!=0) {
                    if (infStore[rTime,j]==-6) {
                        infStore[rTime,j]<--4
                          }
                    if (infStore[rTime,j]>0) {
                        infStore[rTime,j]<-4
                     }
                    if (infStore[rTime,j]==-8){
                       infStore[rTime,j]<-4
                    }
                 }
                if (infStore[rTime,j]==0) {
                  infStore[rTime,j]<-2
                  durationBS<-getDurationBS(h)
                  relapseClear<-0
                  for (d in 2:durationBS) {          
                    if ((rTime+d-1)<=maxtime) {
                      if (infStore[rTime+d-1,j]==-6 | infStore[rTime+d-1,j]==-8) {
                          relapseClear<-1
                      }
                      if (infStore[rTime+d-1,j]!=-6 & infStore[rTime+d-1,j]!=-8) {
                          if (relapseClear==0) {
                             infStore[rTime+d-1,j]<-3
                          }
                      }
                   }
                  } # close for d loop
                 for (w in 1:immunePeriodDays) {
                  if (rTime+durationBS+w-1<=maxtime) {
                    if (infStore[rTime+durationBS+w-1,j]==-8 | infStore[rTime+durationBS+w-1,j]==-6){
                       relapseClear<-1
                    }
                   if (rTime+durationBS+w-1<=maxtime & relapseClear==0) {
                       infStore[rTime+durationBS+w-1,j]<--8
                   }
                } # w wastage close
             }  
           } 
         }
      } # for h loop
    } # close if (hypnoTime>0) loop
   }  # for j in primI loop
   return(infStore)
}





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# addToNonwastedCount
# summary counts of relapse (relapse classification B)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
addToNonwastedCount<-function() {
   for (r in 1:length(primI)) {
     nonwastedNum<-0
     for (b in 1:maxtime) {        
       if (infStore[b,r]==2) {
           if (nonwastedNum<5) {nonwastedNum<-nonwastedNum+1}
           nonwastedCount[b,nonwastedNum]<-nonwastedCount[b,nonwastedNum]+1
       }
    }
  }
  return(nonwastedCount)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# addToPrimCount 
# summary counts of simulated primary infections in infStore 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
addToPrimCount<-function() {
  for (r in 1:length(primI)) {
     for (b in 1:maxtime){
        if (infStore[b,r]==1) {
            primCount[b]<-primCount[b]+1
        } 
     }
  }
  return(primCount)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# addToRelapseCount
# summary counts of all simulated relapses in infStore (relapse classification A)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
addToRelapseCount<-function() {
   for (r in 1:length(primI)) {
     relNum<-0
     for (b in 1:maxtime) {        
       if (infStore[b,r]==2 | infStore[b,r]==4) {
           if (relNum<5) {relNum<-relNum+1}
           relapseCount[b,relNum]<-relapseCount[b,relNum]+1
       }
    }
  }
  return(relapseCount)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getAgecatLong
# age categories over time (used for treatment)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getAgecatLong<-function(midIntervalAgeYrs,endInterval,timeUnit) {
  bsaAge<-rep(1,maxtime)
  midIntervalTime<-(endInterval-0.5)*(60/timeUnit)
  midIntervalAgeMonths<-trunc(midIntervalAgeYrs*365/30)
  bsaAge[midIntervalTime]<-midIntervalAgeMonths 
  for (b in ((midIntervalTime+1):maxtime)) {
      bsaAge[b]<-round(midIntervalAgeMonths + ((b-midIntervalTime)/(30/timeUnit)) )
  }
  for (b in 1:midIntervalTime) {
     bsaAge[b]<-round(midIntervalAgeMonths - (midIntervalTime-b)/(30/timeUnit) )
  }
  agecatLong<-bsaAge
  agecatLong[agecatLong<12]<--9
  agecatLong[agecatLong>=12 & agecatLong<18]<-0
  agecatLong[agecatLong>=18 & agecatLong<24]<-1
  agecatLong[agecatLong>=24 & agecatLong<30]<-2
  agecatLong[agecatLong>=30 & agecatLong<36]<-3
  agecatLong[agecatLong>=36 & agecatLong<42]<-4
  agecatLong[agecatLong>=42 & agecatLong<48]<-5
  agecatLong[agecatLong>=48]<-6
  return(agecatLong)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bsaRatio
# calculates body surface area per age group per interval to align BSA and seasonality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getBsaRatio<-function(midIntervalAgeYrs,endInterval,timeUnit) {
  bsaRatio<-rep(1,maxtime)
  bsaAge<-rep(1,maxtime)
  midIntervalTime<-(endInterval-0.5)*(60/timeUnit)
  midIntervalAgeMonths<-trunc(midIntervalAgeYrs*365/30)
  bsaAge[midIntervalTime]<-midIntervalAgeMonths 
  for (b in ((midIntervalTime+1):maxtime)) {
      bsaAge[b]<-round(midIntervalAgeMonths + ((b-midIntervalTime)/(30/timeUnit)) )
  }
  for (b in 1:midIntervalTime) {
     bsaAge[b]<-round(midIntervalAgeMonths - (midIntervalTime-b)/(30/timeUnit) )
  }
  for (b in 1:midIntervalTime) {
      if (bsaAge[b]>0) {
          bsaRatio[b]<-sqrt(girlsHt[bsaAge[b]+1]*girlsWei[bsaAge[b]+1]/3600)/0.606998
       } 
      # include first month of life
      else if (bsaAge[b]==0 & (midIntervalAgeMonths+((b-midIntervalTime)/(30/timeUnit)))>0) {
          bsaRatio[b]<-sqrt(girlsHt[1]*girlsWei[1]/3600)/0.606998
      }
      else {
         bsaRatio[b]<-0
       }
   }  
 return(bsaRatio)
}





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getDurationBS
# samples the duration of blood-stage infection (in days)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getDurationBS<-function(j){
   # for rr=0.8 4p3
     dura<-round(rweibull(1,3.184713,17.28778)*(5/timeUnit),digit=0)
   # for rr=2.0 5p9
   if (paramSetID==1) {dura<-round(rweibull(1,3.184713,17.28778)*(5/timeUnit)*(30/80),digit=0)}
   return(dura)
}
# nb for daily parameters directly: a=3.184713, b=86.48751





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getPrimI 
# get timing of simulated primary infections   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getPrimI<-function(i,maxtime) {
  numPrimI<-rep(0,maxtime)
  for (b in 1:maxtime) {     
     numPrimI[b]<-rpois(1,seasEIR[b]*bsaRatio[b])
  }     
  # primI: list of primary infection times
  primI<--9     
  for (b in 1:maxtime) {
    if (numPrimI[b]!=0) {primI<-c(primI,rep(b,numPrimI[b]))}
  }  
  primI<-primI[primI!=-9]
  return(primI)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~
# getRelapses
# get timing of relapses
# ~~~~~~~~~~~~~~~~~~~~~~~~
getRelapses<-function() {
     numHypno<-getNumHypno()
     hypnoTime<-round((14/timeUnit) + exp(rnorm(numHypno,mean=2.92,sd=0.956))*(7/timeUnit),digits=0)
     if (numHypno>0) {
        for (nh in 1:numHypno) {   # in case very long relapse times sampled
           if (hypnoTime[nh]>500) {hypnoTime[nh]<-500}
         }
      hypnoTime<-sort(hypnoTime)
     }
 return(hypnoTime)
}
# 14d is time from primary to first relapse week




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getTreatedLong
# get stochastic treatments from treatment rates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getTreatedLong<-function(treatRate){
        # turns treatRate into whether treatment of simulated individual or not
        treated<-rep(0,length(treatRate))
        for (b in 1:length(treatRate)) { 
           treated[b]<-rpois(1,treatRate[b])
        }
        return(treated)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getTreatRateLong
# gets the observed treatment rates for the covariate category
# allows the same treatment rates before the study period
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getTreatRateLong<-function(midIntervalAgeYrs,endInterval,timeUnit) {
    	treated<-rep(0,length(seasEIR))
      treatRate<-rep(0,length(seasEIR))
      treatments$rate<-(treatments$numTrts/treatments$nchildren)/(60/timeUnit)
      # get treatment rate for age and interval
      for (b in 1:length(treated)) {
           if (agecatLong[b]>-9 & endIntervalTreatLong[b]>=9) {
            if (length(treatments$rate[treatments$agecat==agecatLong[b] & treatments$endInterval==endIntervalTreatLong[b]])>0) {
                treatRate[b]<-treatments$rate[treatments$agecat==agecatLong[b] & treatments$endInterval==endIntervalTreatLong[b]]
            }
          }
         # some categories have no children, or children are <1y
         if (treatRate[b]==0) {treatRate[b]<-0.0005}
      }
    return(treatRate)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getNumHypno
# get number of relapses for primary infection
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getNumHypno function
getNumHypno<-function() {
   numHypnoList<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
   # parameter set 2 (4p3)
   cumProbNumHypno<-c(0.1658090,0.3060006,0.4245326,0.5247513,0.6094863,0.6811297,0.7417042,0.7929200,0.8362229,0.8728355,0.9037914,0.9299647,0.9520942,0.9708047,0.9866244,1.0)
   # parameter set 1 (5p9)
   if (paramSetID==1) {cumProbNumHypno<-c(0.1048926,0.2019707,0.2918164,0.3749687,0.4519261,0.5231502,0.5890681,0.6500751,0.7065370,0.7587926,0.8071551,0.8519147,0.8933396,0.9316784,0.9671609,1.0000000)}
   rand<-runif(1)
   numHypno<-0
   for (i in 1:(length(numHypnoList)-1)) {
      if (rand>cumProbNumHypno[i]) {numHypno<-numHypnoList[i+1]}
   }
  return(numHypno)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# setUpPrimInfStore
# sets up InfStore to temporarily store simulation for one individual in days
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setUpPrimInfStore<-function(i) {
   infStore<-array(0,dim=c(maxtime,length(primI)))
   # add treatment proph period as -6
    for (b in 1:maxtime) {
        if (treatedLong[b]>0) {
           for (tt in 1:proph) {
              if ((b+tt)<=maxtime) {
                  infStore[(b+tt),]<-rep(-6,length(primI)) 
              }
           } 
        }   
    }
   # primary infections and duration
   for (j in 1:length(primI)) {
      # add primary infection as 1, if during proph period then as -1
      treatClear<-0
      durationBS<-getDurationBS(j)
      if (primI[j]+incubPeriod<maxtime) { 
         if (infStore[primI[j]+incubPeriod,j]==0){
             infStore[primI[j]+incubPeriod,j]<-1
         } 
         if (infStore[primI[j]+incubPeriod,j]==-6){
           infStore[primI[j]+incubPeriod,j]<--1
           treatClear<-1
         }
      }
      for (d in 2:durationBS) {
         dTime<-primI[j]+incubPeriod-1+d
         if (dTime<=maxtime) {
            if (infStore[dTime,j]>=0 & treatClear==0) {
               infStore[dTime,j]<-3
            }
            if (infStore[dTime,j]==-6 | infStore[dTime,j]==-1) {
               treatClear <- 1
            }
         }
      }
      # wastage period indicator (as -8)
      for (w in 1:20) {
          dTime<-primI[j]+incubPeriod+durationBS-1+w
          if (dTime<=maxtime) {
            if (infStore[dTime,j]==-6){
              treatClear<-1
            }
          }
          if (dTime<=maxtime & treatClear==0) {
            infStore[dTime,j]<--8
          }
      }
   } # close j 
   return(infStore)
}









