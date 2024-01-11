# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# setSimulInputs.r
# called by simul.r
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# force of primary infection
beta1<-0.01538
if (itnvillID==1) {beta1<-0.01538}
if (itnvillID==2) {beta1<-0.01538*0.7}
if (itnvillID==3) {beta1<-0.01538*1.16}
if (itnvillID==4) {beta1<-0.01538*0.7*1.16}

# if using parameter set 5p9 instead of 4p3 
if (paramSetID==1) {beta1 <- beta1 * (0.01778/0.01538)}


# time unit (daily=1, only daily possible)
timeUnit<-1

# SEASONALITY
pfseas_ext<-c(1.8576323, 1.2779529, 0.8645956, 0.6902913, 0.6234370, 1.1312040, 1.8576323, 1.2779529, 
          0.8645956, 0.6902913, 0.6234370, 1.1312040, 1.8576323, 1.2779529, 0.8645956, 0.6902913)
pfseas_ext <- pfseas_ext * (11.5/11.3)



# interval_ext needed for treatments pre-study period
interval_ext<-c(seq(3,6),seq(1,8))
interval_ext<-c(interval_ext[3:6], interval_ext) + 8


# get daily Pf EIR (60d per 2-month interval)
seasEIR<-rep(pfseas_ext/(60/timeUnit),each=(60/timeUnit))
endIntervalLong<-rep(seq(1:16),each=(60/timeUnit))
# endInterval with repeated EIR for calculating treatment rates
endIntervalTreatLong<-rep(interval_ext, each=(60/timeUnit))
# get Pv EIR per genotype (114 MS16 genotypes)
seasEIR<-seasEIR*beta1*114 
maxtime<-length(seasEIR)
max5time<-maxtime/5 

# Body surface area (month 0 to 61)
girlsWei<-c(3.2322,4.2075,5.1315,5.8602,6.4280,6.9100,7.3016,7.6416,7.9534,8.2259,8.4850,8.7207,8.9536,9.1722,9.3861,9.6038,9.8124,10.0265,10.2324,10.4372,10.6481,10.8521,11.0633,11.2684,11.4809,11.6868,11.8922,12.1028,12.3042,12.5093,12.7047,12.9033,13.0930,13.2809,13.4739,13.6599,13.8518,14.0373,14.2226,14.4136,14.5979,14.7877,14.9704,15.1584,15.3395,15.5199,15.7056,15.8849,16.0697,16.2485,16.4270,16.6114,16.7897,16.9737,17.1514,17.3344,17.5107,17.6861,17.8664,18.0398,18.2179,18.3890)
girlsHt<-c(49.8477,54.4571,57.7796,60.5589,62.8071,64.7781,66.4510,67.9842,69.4732,70.8463,72.2088,73.4788,74.7451,75.9297,77.0770,78.2258,79.3055,80.3904,81.4121,82.4080,83.4116,84.3595,85.3154,86.2184,86.4299,87.2922,88.1358,88.9881,89.7938,90.6072,91.3765,92.1539,92.8906,93.6135,94.3473,95.0460,95.7572,96.4356,97.1049,97.7871,98.4385,99.1028,99.7369,100.3834,101.0007,101.6099,102.2312,102.8249,103.4312,104.0113,104.5854,105.1727,105.7354,106.3114,106.8634,107.4284,107.9698,108.5060,109.0547,109.5806,110.1189,110.6352)
ageStudy<-seq(0:61)/12 
bsaGirl<-sqrt(girlsHt*girlsWei/3600)/0.606998


# EXPECTED NUMBER OF CUMULATIVE INFECTIONS  
# get cumulative numbers of infections per age group and interval
# run simulation or read-in if previously run
# source("cumLifetimeInf.r")
# intervals are endIntervals, age is mid-point of interval
cumIntAgeStore<-read.table("cumInf_byIntAgegp_noitn_novill.txt",sep=" ")
cumIntAgeStore
# multiply to get correct values for the covariate and parameter set 
if (itnvillID==1) {cumIntAgeStore<-cumIntAgeStore}
if (itnvillID==2) {cumIntAgeStore<-cumIntAgeStore*0.7}
if (itnvillID==3) {cumIntAgeStore<-cumIntAgeStore*1.16}
if (itnvillID==4) {cumIntAgeStore<-cumIntAgeStore*0.7*1.16}
# if using parameter set 5p9 instead of 4p3 
if (paramSetID==1) {cumIntAgeStore <- cumIntAgeStore * (0.01778/0.01538)}
cumIntAgeStore




# maximum number of infections per child
numPotInf<-50 
# prohylactic period
proph<-14
# mid-point of age-group
ageGroupList<-c(1.25,1.75,2.25,2.75,3.25,3.75,4.25)
# endInterval is the end of the time-interval
endIntervalList<-c(9,10,11,12,13,14,15,16)


## treatments & number of children 
r03data<-read.table("probClinData2_combined.txt", header=TRUE)
r03data<-data.frame(r03data)
if (itnvillID==1) {r03data<-r03data[r03data$itnvillCombo==1,]}
if (itnvillID==2) {r03data<-r03data[r03data$itnvillCombo==2,]}
if (itnvillID==3) {r03data<-r03data[r03data$itnvillCombo==3,]}
if (itnvillID==4) {r03data<-r03data[r03data$itnvillCombo==4,]}
treatments<-cbind(r03data$nchildren,r03data$numTrt,r03data$agecat,r03data$endInterval)
colnames(treatments)<-c("nchildren","numTrts","agecat","endInterval")
treatments<-data.frame(treatments)

names(r03data)[names(r03data) == "agecat"] <- "ageGpl"
names(r03data)[names(r03data) == "endInterval"] <- "endIntervall"
names(r03data)[names(r03data) == "nchildren"] <- "nchildrenL"
names(r03data)[names(r03data) == "pvfever500"] <- "fever500"


# number of age-groups
numAgeGp<-7
# number of 2-month intervals in the study period
numStudyInterval<-8

# incubation period = 12 days (omitted since Pf incub period is around 12d and seasonality is from Pf)
incubPeriod<-0
# number of intervals in both pre and study periods 
numIntervals<-16

# immune period in days (for relapses classification B)
immunePeriodDays <- 20


