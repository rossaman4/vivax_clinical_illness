
# FIGURE: NUMBER OF CUMULATIVE INFECTIONS ---
# S3 Figure

# set your directory
setwd("..")

cumIntAgeStore<-read.table("cumInf_IntAgegp_noitn_novill.txt", sep=" ")

# colour palette
library("RColorBrewer")
colblues=rev(brewer.pal(9,"Blues"))
coloranges=rev(brewer.pal(9,"Oranges"))
coloranges<-coloranges[9:1]
colblues<-colblues[9:1]

# order colours to give interval by time from start of transmission season
# two intervals have same shading since 8 intervals covers more than one year
cols1<-colblues[c(6,7,8,9,4,5,6,7)]
cols2<-coloranges[c(6,7,8,9,4,5,6,7)]



par(oma=c(0,0,0,0))
# where labels are placed
par(mgp=c(1.3,0.3,0))   
# length of tick marks  
par(tcl=-0.3)
par(mar=c(2.5,2,0.0,0.2))

ageGpList<-c(1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25)

plot(ageGpList,cumIntAgeStore[9,],type="p",ylim=c(-5,50),xlim=c(0.5,4.5),las=1,xlab="mid-point of age group in years",
     ylab="cumulative number of infections", pch=16,col=cols1[1])
for (i in 2:7) {
  lines(ageGpList,cumIntAgeStore[i+8,],type="p",pch=16,col=cols1[i])
}

# The input file was created assuming the force of infection of parameter set 2 - to adjust to the force to infection estimated for parameter set 1 multiply by 1.156
lines(ageGpList,cumIntAgeStore[9,]*1.156,type="p",pch=16,col=cols2[1])
for (i in 2:7) {
  lines(ageGpList,cumIntAgeStore[i+8,]*1.156,type="p",pch=16,col=cols2[i])
}










