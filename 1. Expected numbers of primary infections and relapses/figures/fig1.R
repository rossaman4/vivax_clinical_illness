# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fig1.r
# plot simulated patterns of primary infections and relapses
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list=ls())

# set your directory
setwd("..")


# colour palette
library("RColorBrewer")
colblues=rev(brewer.pal(9,"Blues"))
colgreens=rev(brewer.pal(9,"Greens"))
cols<-c(colgreens[1:4],colblues[4:1])


# get simulated primary infections and relapses
# Either read-in the simulations you have freshly run, or use those previously run  
# 4p3 (parameter set 2)
noitnnovill<-read.table("noitnnovill_4p3param2.txt", header=TRUE)
probClinData4p3<-noitnnovill
# 5p9 (parameter set 1)
noitnnovill<-read.table("noitnnovill_5p9param1.txt", header=TRUE)
probClinData5p9<-noitnnovill




# extract primary infections and relapses
prim4p3<-probClinData4p3$prim_ppp2m
nonw14p3<-probClinData4p3$relnonwB2j_1
nonw2p4p3<-probClinData4p3$relnonwB2j_2p
totRel4p3<-probClinData4p3$reltotA1j_all
totRel124p3<-probClinData4p3$reltotA2j_12
totRel3p4p3<-probClinData4p3$reltotA2j_3p
agecat<-probClinData4p3$ageGpl
endInterval<-probClinData4p3$endIntervall

prim5p9<-probClinData5p9$prim_ppp2m
nonw15p9<-probClinData5p9$relnonwB2j_1
nonw2p5p9<-probClinData5p9$relnonwB2j_2p
totRel5p9<-probClinData5p9$reltotA1j_all
totRel125p9<-probClinData5p9$reltotA2j_12
totRel3p5p9<-probClinData5p9$reltotA2j_3p

roundLabel<-c("Jun06","Aug06","Oct06","Dec06","Feb07","Apr07","Jun07","Aug07")
interval<-endInterval


# set graphical parameters
par(mfrow=c(2,5))
par(oma=c(1,0,0,0))
par(mar=c(1,2,0,0))
# b,l,t,r
# length of tick marks
par(tcl=-0.3)
# where labels are placed
par(mgp=c(0.7,0.3,0))  

 

# TOP ROW
plot(interval[agecat==0],prim5p9[agecat==0],type="p",lwd=1.75,xlim=c(8.5,16),axes=F,ylim=c(0,4),ylab="incidence/2 months/child",las=1,xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],prim5p9[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,4,"a",cex=1.2)


plot(interval[agecat==0],totRel125p9[agecat==0],type="p",lwd=1.75,ylim=c(0,8),xlim=c(8.5,16),axes=F,ylab="",xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],totRel125p9[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,8,"b",cex=1.2)


plot(interval[agecat==0],totRel3p5p9[agecat==0],type="p",lwd=1.75,ylim=c(0,12),xlim=c(8.5,16),axes=F,ylab="",xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],totRel3p5p9[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,12,"c",cex=1.2)


plot(interval[agecat==0],nonw15p9[agecat==0],type="p",lwd=1.75,xlim=c(8.5,16),ylim=c(0,4),ylab="",las=1,xlab="",axes=F,col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],nonw15p9[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,4,"d",cex=1.2)

plot(interval[agecat==0],nonw2p5p9[agecat==0],type="p",lwd=1.75,axes=F,xlim=c(8.5,16),ylim=c(0,6),ylab="",las=1,xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],nonw2p5p9[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,6,"e",cex=1.2)



# BOTTOM ROW
plot(interval[agecat==0],prim4p3[agecat==0],type="p",lwd=1.75,xlim=c(8.5,16),axes=F,ylim=c(0,4),ylab="incidence/2 months/child",las=1,xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],prim4p3[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,4,"f",cex=1.2)

plot(interval[agecat==0],totRel124p3[agecat==0],type="p",lwd=1.75,ylim=c(0,8),xlim=c(8.5,16),axes=F,ylab="",xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],totRel124p3[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,8,"g",cex=1.2)


plot(interval[agecat==0],totRel3p4p3[agecat==0],type="p",lwd=1.75,ylim=c(0,12),xlim=c(8.5,16),axes=F,ylab="",xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],totRel3p4p3[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,12,"h",cex=1.2)


plot(interval[agecat==0],nonw14p3[agecat==0],type="p",lwd=1.75,xlim=c(8.5,16),ylim=c(0,4),ylab="",las=1,xlab="",axes=F,col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],nonw14p3[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,4,"i",cex=1.2)

plot(interval[agecat==0],nonw2p4p3[agecat==0],type="p",lwd=1.75,axes=F,xlim=c(8.5,16),ylim=c(0,6),ylab="",las=1,xlab="",col=cols[1])
for (i in 1:6) {
   lines(interval[agecat==i],nonw2p4p3[agecat==i],type="p",col=cols[i],lwd=1.75)
}
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,6,"j",cex=1.2)












