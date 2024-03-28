
# ~~ fig2_table3.r ~~

#setwd("...")

# from /input files (expected numbers of primary infections and relapses)
# 5p9 (parameter set 1)
itnvill<-read.table("input files/itnvill_5p9param1.txt", sep=" ", header=TRUE)
itnnovill<-read.table("input files/itnnovill_5p9param1.txt", sep=" ", header=TRUE)
noitnvill<-read.table("input files/noitnvill_5p9param1.txt", sep=" ", header=TRUE)
noitnnovill<-read.table("input files/noitnnovill_5p9param1.txt", sep=" ", header=TRUE)
xtemp5p9<-rbind(noitnvill,itnnovill,noitnnovill,itnvill)

# 4p3 (parameter set 2)
itnvill<-read.table("input files/itnvill_4p3param2.txt", sep=" ", header=TRUE)
itnnovill<-read.table("input files/itnnovill_4p3param2.txt", sep=" ", header=TRUE)
noitnvill<-read.table("input files/noitnvill_4p3param2.txt", sep=" ", header=TRUE)
noitnnovill<-read.table("input files/noitnnovill_4p3param2.txt", sep=" ", header=TRUE)
xtemp4p3<-rbind(noitnvill,itnnovill,noitnnovill,itnvill)





# ---- get estimated parameter values -----
# from Stan model for the probability of clinical illness
# read in saved results for each model, extract parameter estimates, put in an array


# 4p3
B2_param2<-read.table("output files/out1_B2_4p3param2.txt", header=TRUE)
B1_param2<-read.table("output files/out1_B1_4p3param2.txt", header=TRUE)
A2_param2<-read.table("output files/out1_A2_4p3param2.txt", header=TRUE)
A1_param2<-read.table("output files/out1_A1_4p3param2.txt", header=TRUE)

mp4p3<-array(-9, dim=c(4,5))
colnames(mp4p3)<-c("p1","p2","p3","cumhstar","gamma")
mp4p3[1,]<-A1_param2[c(1,2,2,3,4),1]
# values for p3 are same as p2 for models which do not estimate p3 separately
mp4p3[2,]<-A2_param2[1:5,1]
mp4p3[3,]<-B1_param2[c(1,2,2,3,4),1]
mp4p3[4,]<-B2_param2[1:5,1]
mp4p3<-as.data.frame(mp4p3)
# order
mp4p3$model <- c("A1", "A2", "B1", "B2")
# (for table 3)
mp4p3


# 5p9
B2_param2<-read.table("out1_B2_5p9param1.txt", header=TRUE)
B1_param2<-read.table("out1_B1_5p9param1.txt", header=TRUE)
A2_param2<-read.table("out1_A2_5p9param1.txt", header=TRUE)
A1_param2<-read.table("out1_A1_5p9param1.txt", header=TRUE)

mp5p9<-array(-9, dim=c(4,5))
colnames(mp5p9)<-c("p1","p2","p3","cumhstar","gamma")
mp5p9[1,]<-A1_param2[c(1,2,2,3,4),1]
mp5p9[2,]<-A2_param2[1:5,1]
mp5p9[3,]<-B1_param2[c(1,2,2,3,4),1]
mp5p9[4,]<-B2_param2[1:5,1]
mp5p9<-as.data.frame(mp5p9)
# order
mp5p9$model <- c("A1", "A2", "B1", "B2")
# (for table 3)
mp5p9




# --- calculate predicted values ---- 

nmodels<-4

getPreds <- function(xtemp, mp) {
     for (i in 1:nmodels) {      
         Dh1 <- exp( -xtemp$cumInfl * mp$cumhstar[i])

         p1adj <- mp$p1[i] * Dh1
         p2adj <- mp$p2[i] * Dh1
         p3adj <- mp$p3[i] * Dh1

        if (mp$model[i]=="B1" | mp$model[i]=="B2") {      
             xtemp$predPrimIll <- xtemp$prim_ppp2m*p1adj
             xtemp$predRel1 <- xtemp$relnonwB2j_1*p2adj
             xtemp$predRel2p <- xtemp$relnonwB2j_2p*p3adj
             pred <- (xtemp$predPrimIll + xtemp$predRel1 + xtemp$predRel2p) * xtemp$nchildrenL
             # save predicted values to correct model 
             if (mp$model[i]=="B1") { xtemp$predB1 <- pred }
             if (mp$model[i]=="B2") { xtemp$predB2 <- pred }  
        } 

        if (mp$model[i]=="A1" | mp$model[i]=="A2") {      
             xtemp$predPrimIll <- xtemp$prim_ppp2m*p1adj
             xtemp$predRel1 <- xtemp$reltotA2j_12*p2adj
             xtemp$predRel2p <- xtemp$reltotA2j_3p*p3adj
             pred <- (xtemp$predPrimIll + xtemp$predRel1 + xtemp$predRel2p) * xtemp$nchildrenL
             # save predicted values to correct model 
             if (mp$model[i]=="A1") { xtemp$predA1 <- pred }
             if (mp$model[i]=="A2") { xtemp$predA2 <- pred }  
        } 
     }
  return(xtemp)
}


xtemp4p3 <- getPreds(xtemp4p3, mp4p3)
xtemp5p9 <- getPreds(xtemp5p9, mp5p9)


xtemp4p3$fevperchild <- xtemp4p3$fever500/xtemp4p3$nchildrenL
xtemp5p9$fevperchild <- xtemp5p9$fever500/xtemp5p9$nchildrenL





# --- (a) PLOT BY AGE-GROUP ---


getAgeIncid <- function(xtemp){
    # sum to get pattern by age-group
    agegStore<-array(0, dim=c(7,7))
    agegStore<-as.data.frame(agegStore)
    agegStore[,1]<-c(0,1,2,3,4,5,6)
    vars <- c("nchildrenL", "fever500", "predB1", "predB2", "predA1", "predA2")
    nvars=6
    for (v in 1:nvars) {
        variable = paste0("xtemp$",vars[v]) 
        for (ageg in 1:7) {
              agegStore[ageg,v+1] <- sum(eval(parse(text=variable))[xtemp$ageGpl==(ageg-1)], na.rm=TRUE) 
        }
    }
    colnames(agegStore) <- c("agegp","nch2mo","obsfever","predB1","predB2","predA1","predA2")
    # scale to per child per year
    for (v in 3:(nvars+1)) {
         agegStore[,v] <- agegStore[,v]*6 / (agegStore[,2])
    }
   return(agegStore)
}

agegStore4p3 <- getAgeIncid(xtemp4p3)
agegStore5p9 <- getAgeIncid(xtemp5p9)


agecatList<-c(1.25,1.75,2.25,2.75,3.25,3.75,4.25)




# set graphical parameters
par(oma=c(3,2.5,0,0))
par(mar=c(0.2,0.2,0.2,0.2))
par(xpd=NA)
# b,l,t,r
# length of tick marks
par(tcl=-0.3)
# where labels are placed
par(mgp=c(1.2,0.4,0))   


par(mfrow=c(1,2))



plot(agecatList, agegStore4p3$obsfever, type="n", las=1, xlab="age-group", ylab="cases per child per year", ylim=c(0,4))

# CI come from fig 1
agecatList<-c(1.25,1.75,2.25,2.75,3.25,3.75,4.25)
# not excluding prophylactic periods
myIncid500<-c(2.036,1.86,1.186,1.295,0.710,0.556,0.161)
cilower<-c(1.52,1.47,0.918, 0.979,0.486,0.317,0.022)
ciupper<-c(2.73,2.35,1.53,1.714,1.038,0.977,1.169)

points(agecatList, myIncid500, col="darkseagreen4", pch=15)
segments(agecatList, cilower, agecatList, ciupper, col="darkseagreen4")

# add predictions
points(agecatList,agegStore4p3$predB1, col="mediumorchid2", pch=16)
points(agecatList,agegStore4p3$predB2, col="mediumorchid2", pch=16)
points(agecatList,agegStore4p3$predA1, col="mediumorchid2", pch=16)
points(agecatList,agegStore4p3$predA2, col="mediumorchid2", pch=16)

points(agecatList,agegStore5p9$predB1, col="mediumorchid2", pch=16)
points(agecatList,agegStore5p9$predB2, col="mediumorchid2", pch=16)
points(agecatList,agegStore5p9$predA1, col="mediumorchid2", pch=16)
points(agecatList,agegStore5p9$predA2, col="mediumorchid2", pch=16)

text(4,3.8,"a")




# -- (b) PLOT BY 2-MONTH TIME INTERVAL ---


getIntervalIncid <- function(xtemp){
    # sum to get pattern by age-group
    timeStore<-array(0, dim=c(8,7))
    timeStore<-as.data.frame(timeStore)
    timeStore[,1]<-c(9,10,11,12,13,14,15,16)
    vars <- c("nchildrenL", "fever500", "predB1", "predB2", "predA1", "predA2")
    nvars=6
    for (v in 1:nvars) {
        variable = paste0("xtemp$",vars[v]) 
        for (t in 1:8) {
              timeStore[t,v+1] <- sum(eval(parse(text=variable))[xtemp$endIntervall==(t+8)], na.rm=TRUE) 
        }
    }
    colnames(timeStore) <- c("interval","nch2mo","obsfever","predB1","predB2","predA1","predA2")
    # scale to per child per year
    for (v in 3:(nvars+1)) {
         timeStore[,v] <- timeStore[,v]*6 / (timeStore[,2])
    }
   return(timeStore)
}

timeStore4p3 <- getIntervalIncid(xtemp4p3)
timeStore5p9 <- getIntervalIncid(xtemp5p9)


intervalList <- timeStore4p3$interval 


plot(intervalList, timeStore4p3$obsfever, col="black", type="n", las=1, xlab="two-month interval", ylab="", ylim=c(0,4), xaxt="n", yaxt="n")
roundLabel<-c("Jun06","Aug06","Oct06","Dec06","Feb07","Apr07","Jun07","Aug07")
axis(1, at=seq(9,16), labels=roundLabel)
axis(2, at=c(0,1,2,3,4), labels=NA)

# CI from fig 1
# time/round (not removing the prophylatic period)
timeIncid500<-c(1.450,1.228,1.392,1.904,1.436,1.684,1.016,0.34)
cilower<-c(1.054,0.877,1.033,1.460,1.079,1.273,0.729,0.195)
ciupper<-c(1.993,1.721,1.874,2.483,1.910,2.227,1.416,0.594)

points(seq(9,16), timeIncid500, col="darkseagreen4", pch=15)
segments(seq(9,16), cilower, seq(9,16), ciupper, col="darkseagreen4")

# add predictions
points(intervalList,timeStore4p3$predB1, col="mediumorchid2", pch=16)
points(intervalList,timeStore4p3$predB2, col="mediumorchid2", pch=16)
points(intervalList,timeStore4p3$predA1, col="mediumorchid2", pch=16)
points(intervalList,timeStore4p3$predA2, col="mediumorchid2", pch=16)

points(intervalList,timeStore5p9$predB1, col="mediumorchid2", pch=16)
points(intervalList,timeStore5p9$predB2, col="mediumorchid2", pch=16)
points(intervalList,timeStore5p9$predA1, col="mediumorchid2", pch=16)
points(intervalList,timeStore5p9$predA2, col="mediumorchid2", pch=16)

text(15.5,3.8,"b")










