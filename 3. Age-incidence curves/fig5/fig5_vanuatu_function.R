


sim1<-read.table("output1.txt")
sim2<-read.table("output2.txt")
sim5<-read.table("output5.txt")
sim10<-read.table("output10.txt")
sim20<-read.table("output20.txt")
sim30<-read.table("output30.txt")
sim50<-read.table("output50.txt")


colnames(sim1)<-c("survey","agegp","measure","value")
colnames(sim2)<-c("survey","agegp","measure","value")
colnames(sim5)<-c("survey","agegp","measure","value")
colnames(sim10)<-c("survey","agegp","measure","value")
colnames(sim20)<-c("survey","agegp","measure","value")
colnames(sim30)<-c("survey","agegp","measure","value")
colnames(sim50)<-c("survey","agegp","measure","value")



# get age-incid curves
numAgegp<-30
ageGps<-c(0.125,0.375,0.625,0.825,1.25,1.75,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,11,13,15,17,19,22.5,
27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,75)
    

numEIR<-7
ageIncid<-array(-9,dim=c(numAgegp,numEIR))

# get incidence of nTreatments1 per age-group averaging over quarterly monitoring
getIndicPerSim<-function(simSet) {
   indic<-rep(-9,numAgegp)
   for (i in 1:numAgegp) { 
     numUnc<-sum(simSet$value[simSet$measure==14 & simSet$agegp==i])/(80/4)
     nHost<-mean(simSet$value[simSet$measure==0 & simSet$agegp==i])
     indic[i]<-(numUnc/nHost)
   }
return(indic)
}



ageIncid[,1]<-getIndicPerSim(sim1)
ageIncid[,2]<-getIndicPerSim(sim2)
ageIncid[,3]<-getIndicPerSim(sim5)
ageIncid[,4]<-getIndicPerSim(sim10)
ageIncid[,5]<-getIndicPerSim(sim20)
ageIncid[,6]<-getIndicPerSim(sim30)
ageIncid[,7]<-getIndicPerSim(sim50)









