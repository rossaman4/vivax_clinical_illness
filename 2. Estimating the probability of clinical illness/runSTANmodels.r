
rm(list=ls())

library(dplyr)
library(rstan)
rstan_options("required" = FALSE)


# set working directory 
setwd("...")


# read in data and expected numbers of primary infections and relapses
# the read-in txt files are created by scripts in the Simulation folder
# (but take some time to run so also saved here)


# 5p9 (parameter set 1)
itnvill<-read.table("input files/itnvill_5p9param1.txt", sep=" ", header=TRUE)
itnnovill<-read.table("input files/itnnovill_5p9param1.txt", sep=" ", header=TRUE)
noitnvill<-read.table("input files/noitnvill_5p9param1.txt", sep=" ", header=TRUE)
noitnnovill<-read.table("input files/noitnnovill_5p9param1.txt", sep=" ", header=TRUE)
xtemp5p9<-rbind(noitnvill,itnnovill,noitnnovill,itnvill)
# take out zero denominators
xtemp5p9<-xtemp5p9[xtemp5p9$nchildrenL>0,]



# 4p3 (parameter set 2)
itnvill<-read.table("input files/itnvill_4p3param2.txt", sep=" ", header=TRUE)
itnnovill<-read.table("input files/itnnovill_4p3param2.txt", sep=" ", header=TRUE)
noitnvill<-read.table("input files/noitnvill_4p3param2.txt", sep=" ", header=TRUE)
noitnnovill<-read.table("input files/noitnnovill_4p3param2.txt", sep=" ", header=TRUE)
xtemp4p3<-rbind(noitnvill,itnnovill,noitnnovill,itnvill)
# take out zero denominators
xtemp4p3<-xtemp4p3[xtemp4p3$nchildrenL>0,]



# one record per covariate category and two-month interval
# prim_ppp2m = number of primary infections: per child per two-month interval
# relnonwB2j_1 = number of first relapses ppp2mo (classification B)
# relnonwB2j_2p = number of second or later relapses ppp2mo (classification B)
# ageGpl = age-group
# endIntervall = interval
# cumInfl = mean cumulative number of infections per child 
# fever500 = number of P vivax fevers
# nchildrenL = number of children
# reltotA1j_all = number of relapses: per child per two-month interval (classification A)
# reltotA2j_12 = number of first or second relapses ppp2mo (classification A)
# reltotA2j_3p = number of third or later relapses ppp2mo (classification A)




# get noitnnovill 3-3.5yo rows to use for seasonal contribution of primary infections and relapses (figure)
noitnnovill<-read.table("input files/noitnnovill_5p9param1.txt", sep=" ", header=TRUE)
xtemp5p9_seas <- noitnnovill[noitnnovill$ageGpl==4,]
noitnnovill<-read.table("input files/noitnnovill_4p3param2.txt", sep=" ", header=TRUE)
xtemp4p3_seas <- noitnnovill[noitnnovill$ageGpl==4,]




# SELECT ONE PARAMETER SET TO RUN AT A TIME AND RUN MODELS
# paramSet = 1 (5p9); paramSet = 2 (4p3)
paramSet <- 1
source("calledByRunStanModels.r")

paramSet <- 2
source("calledByRunStanModels.r")




