

sim0<-read.table("output0.txt")
sim0p1<-read.table("output0p1.txt")
sim0p2<-read.table("output0p2.txt")
sim0p3<-read.table("output0p3.txt")
sim0p4<-read.table("output0p4.txt")
sim0p5<-read.table("output0p5.txt")
sim0p6<-read.table("output0p6.txt")
sim0p7<-read.table("output0p7.txt")
sim0p8<-read.table("output0p8.txt")
sim0p9<-read.table("output0p9.txt")

colnames(sim0)<-c("survey","agegp","measure","value")
colnames(sim0p1)<-c("survey","agegp","measure","value")
colnames(sim0p2)<-c("survey","agegp","measure","value")
colnames(sim0p3)<-c("survey","agegp","measure","value")
colnames(sim0p4)<-c("survey","agegp","measure","value")
colnames(sim0p5)<-c("survey","agegp","measure","value")
colnames(sim0p6)<-c("survey","agegp","measure","value")
colnames(sim0p7)<-c("survey","agegp","measure","value")
colnames(sim0p8)<-c("survey","agegp","measure","value")
colnames(sim0p9)<-c("survey","agegp","measure","value")


# get age-incid curves
numAgegp<-30
ageGps<-c(0.125,0.375,0.625,0.825,1.25,1.75,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,11,13,15,17,19,22.5,
27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,75)
    

numTrtCov<-10
ageIncid<-array(-9,dim=c(numAgegp,numTrtCov))


# get incidence of nTreatments1 per age-group averaging over quarterly monitoring
getIndicPerSim<-function(simSet) {
   indic<-rep(-9,numAgegp)
   for (i in 1:numAgegp) { 
     numUnc<-sum(simSet$value[simSet$measure==11 & simSet$agegp==i])/(80/4)
     nHost<-mean(simSet$value[simSet$measure==0 & simSet$agegp==i])
     indic[i]<-(numUnc/nHost)
   }
return(indic)
}



ageIncid[,1]<-getIndicPerSim(sim0)
ageIncid[,2]<-getIndicPerSim(sim0p1)
ageIncid[,3]<-getIndicPerSim(sim0p2)
ageIncid[,4]<-getIndicPerSim(sim0p3)
ageIncid[,5]<-getIndicPerSim(sim0p4)
ageIncid[,6]<-getIndicPerSim(sim0p5)
ageIncid[,7]<-getIndicPerSim(sim0p6)
ageIncid[,8]<-getIndicPerSim(sim0p7)
ageIncid[,9]<-getIndicPerSim(sim0p8)
ageIncid[,10]<-getIndicPerSim(sim0p9)








