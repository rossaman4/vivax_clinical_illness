
# ----------------------
# primInfRelapse.r
# gets numbers of primary infections and relapses per age-group and per interval
# called by simul.r
# -----------------------


# GET COUNTS
# get simulated counts of primary infections, relapse & nonwasted relapses for n=numIndiv simulated individuals in age-group/interval 
# infStore is an array with one row per day and one column per inoculation (one infStore per person)
# codes for simulated events that occur:
# patent prim inf = 1, patent BS infection from primInf=3, treated proph period = -6, prim inf during proph=-1, immunePeriod=-8
# relapse 2 = relapse nonwasted (relapse classification B), 3 = patent BS infection from relapse, 4=relapse wasted due to BS infection or immuneP, -4=relapse wasted due to treatment  
# first make stores
nonwastedCount<-array(0,dim=c(maxtime,5))
relapseCount<-array(0,dim=c(maxtime,5))
primCount<-rep(0,maxtime)
prim5Count<-rep(0,max5time)

# simulate and store (daily)
for (i in 1:numIndiv) {
   treatedLong<-getTreatedLong(treatRate)
   primI<-getPrimI(i,maxtime)
   # primI is list of the times for primary infections
   if (length(primI)>0) {                 # for each primary infection    
      infStore<-setUpPrimInfStore(i)      # make a column for each prim inf    
      infStore<-addRelapsesToInfStore(i)  # add relapses 
      primCount<-addToPrimCount()
      nonwastedCount<-addToNonwastedCount()
      relapseCount<-addToRelapseCount()
   }     
}



# aggregate results for two-month (60-day) intervals 
# for primary infections (primCount),  relapse classification B (nonwastedCount) and relapse classfication A (relapseCount)
Nrelapse<-rep(9,maxtime)
Nnonwasted<-rep(9,maxtime)
for (b in 1:maxtime) {
  Nrelapse[b]<-sum(relapseCount[b,1:5])
  Nnonwasted[b]<-sum(nonwastedCount[b,1:5])
}

groupNonwasted<-rep(0,numIntervals)
groupRelapse<-rep(0,numIntervals)
groupPrim<-rep(0,numIntervals)
for (g in 1:numIntervals) { 
  start<-((g-1)*60)+1
  end<-start+59
  groupNonwasted[g]<-sum(Nnonwasted[start:end])
  groupRelapse[g]<-sum(Nrelapse[start:end])
  groupPrim[g]<-sum(primCount[start:end])
}

# Get also the nonwastedCount for ordered categories of relapses
groupNonwastedOrder<-array(0,dim=c(numIntervals,5))
groupRelapseOrder<-array(0,dim=c(numIntervals,5))
for (i in 1:5) {
  for (g in 1:numIntervals) { 
    start<-((g-1)*60)+1
    end<-start+59
    groupNonwastedOrder[g,i]<-sum(nonwastedCount[start:end,i])
    groupRelapseOrder[g,i]<-sum(relapseCount[start:end,i])
  }
}


# Final results for specific interval and age-group
primAgeInt<-groupPrim[endInterval]
nonwastedAgeInt1<-groupNonwastedOrder[endInterval,1]
nonwastedAgeInt2p<-sum(groupNonwastedOrder[endInterval,2:5])
totRelapseAgeInt<-groupRelapse[endInterval]
totRelapseAgeInt12<-sum(groupRelapseOrder[endInterval,1:2])
totRelapseAgeInt3p<-sum(groupRelapseOrder[endInterval,3:5])










