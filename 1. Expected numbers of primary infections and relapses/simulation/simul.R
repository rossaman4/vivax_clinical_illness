# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simul.r
# simulate vivax model to get age- and interval-specific numbers for the Ilaita cohort 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# called by runSimul.r

# SET INPUTS FOR SIMULATION AND GET NUMBER OF CUMULATIVE INFECTIONS
source("setSimulInputs.r")


# RUN SIMULATION FOR EXPECTED NUMBERS OF PRIMARY INFECTIONS AND RELAPSES
# read in functions needed for simulation
source("simul_functions.r")

# number of simulated individuals per agegroup
numIndiv<-10
#numIndiv<-1000


# Get results for each 2-month interval (endInterval) and age-group (midIntervalAgeYrs) in a loop

# make stores for results
prevStore<-array(-9,dim=c(numStudyInterval,numAgeGp))
primStore<-array(-9,dim=c(numStudyInterval,numAgeGp))
nonwasted1Store<-array(-9,dim=c(numStudyInterval,numAgeGp))
nonwasted2pStore<-array(-9,dim=c(numStudyInterval,numAgeGp))
totRelapseStore<-array(-9,dim=c(numStudyInterval,numAgeGp))
totRelapse12Store<-array(-9,dim=c(numStudyInterval,numAgeGp))
totRelapse3pStore<-array(-9,dim=c(numStudyInterval,numAgeGp))


# run simulation
for (a in 1:length(ageGroupList)) {
  for (d in 1:length(endIntervalList)) {    
    midIntervalAgeYrs<-ageGroupList[a]
    endInterval<-endIntervalList[d]
    bsaRatio<-getBsaRatio(midIntervalAgeYrs,endInterval,timeUnit)
    agecatLong<-getAgecatLong(midIntervalAgeYrs,endInterval,timeUnit)
    treatRate<-getTreatRateLong(midIntervalAgeYrs,endInterval,timeUnit)
    source("primInfRelapse.r")
    # store results
    primStore[d,a]<-primAgeInt
    nonwasted1Store[d,a]<-nonwastedAgeInt1
    nonwasted2pStore[d,a]<-nonwastedAgeInt2p
    totRelapseStore[d,a]<-totRelapseAgeInt
    totRelapse12Store[d,a]<-totRelapseAgeInt12
    totRelapse3pStore[d,a]<-totRelapseAgeInt3p 
  }
print(a)
}


# GATHER AND FORMAT DATA 
# write data neatly to a file

# unfurl the simul arrays
ageGpl<-rep(seq(0,6),each=numStudyInterval)
endIntervall<-rep(seq(9,16), numAgeGp)
prim_ppp2m<-c(primStore)/numIndiv
relnonwB2j_1<-c(nonwasted1Store)/numIndiv
relnonwB2j_2p<-c(nonwasted2pStore)/numIndiv
reltotA1j_all<-c(totRelapseStore)/numIndiv
reltotA2j_12<-c(totRelapse12Store)/numIndiv
reltotA2j_3p<-c(totRelapse3pStore)/numIndiv




# get observed number of children and fevers per age- and time- category (for each separate run of itn/village combinations)
df <- as.data.frame(cbind(ageGpl, endIntervall))
df2 <- merge(df, r03data, by=c("ageGpl", "endIntervall"), all=TRUE)
df2 <- df2[,c('ageGpl', 'endIntervall', 'nchildrenL', 'fever500')]
df2$nchildrenL[is.na(df2$nchildrenL)]<-0
df2 <- df2[order(df2$ageGpl, df2$endIntervall),]



# get cumulative numbers of infections
cumInfl<-unlist(cumIntAgeStore[9:16,], use.names=FALSE)
                


# collect required variables together
expected_infections<-cbind(prim_ppp2m,relnonwB2j_1,relnonwB2j_2p,cumInfl,reltotA1j_all,reltotA2j_12,reltotA2j_3p, df2)
expected_infections<-data.frame(expected_infections)





