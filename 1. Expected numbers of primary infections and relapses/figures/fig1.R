# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fig1.r
# plot expected patterns of primary infections and relapses
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# set working directory
setwd("...")



# install.packages("RColorBrewer")
# colour palette
library("RColorBrewer")
colblues=rev(brewer.pal(9,"Blues"))
colgreens=rev(brewer.pal(9,"Greens"))
cols<-c(colgreens[1:4],colblues[4:1])


# get simulated primary infections and relapses
# Either read-in the simulations you have freshly run, or use those previously run  
# 4p3 (parameter set 2)
noitnnovill<-read.table("noitnnovill_4p3param2.txt", header=TRUE)
pcdata4p3<-noitnnovill
# 5p9 (parameter set 1)
noitnnovill<-read.table("noitnnovill_5p9param1.txt", header=TRUE)
pcdata5p9<-noitnnovill



roundLabel<-c("Jun06","Aug06","Oct06","Dec06","Feb07","Apr07","Jun07","Aug07")
interval<-pcdata5p9$endInterval
agecat <- pcdata5p9$ageGpl




# -- functions to make graphs for each outcome (primary infection, relapse classifications) --

plotPrimInf <- function(xdata) {
   primInf <- xdata$prim_ppp2m
   plot(interval[agecat==0],primInf[agecat==0],type="p",lwd=1.75,xlim=c(8.5,16),axes=F,ylim=c(0,4),ylab="incidence/2 months/child",las=1,xlab="",col=cols[1])
   for (i in 1:6) {
      lines(interval[agecat==i],primInf[agecat==i],type="p",col=cols[i],lwd=1.75)
   }
}

plotTotRel12 <- function(xdata) {
  totRel12 <- xdata$reltotA2j_12
  plot(interval[agecat==0],totRel12[agecat==0],type="p",lwd=1.75,ylim=c(0,8),xlim=c(8.5,16),axes=F,ylab="",xlab="",col=cols[1])
  for (i in 1:6) {
     lines(interval[agecat==i],totRel12[agecat==i],type="p",col=cols[i],lwd=1.75)
  }
}


plotTotRel3p <- function(xdata) {
  totRel3p <- xdata$reltotA2j_3p
  plot(interval[agecat==0],totRel3p[agecat==0],type="p",lwd=1.75,ylim=c(0,12),xlim=c(8.5,16),axes=F,ylab="",xlab="",col=cols[1])
  for (i in 1:6) {
    lines(interval[agecat==i],totRel3p[agecat==i],type="p",col=cols[i],lwd=1.75)
  }
}


plotNonw1 <- function(xdata) {
  nonw1 <- xdata$relnonwB2j_1
  plot(interval[agecat==0],nonw1[agecat==0],type="p",lwd=1.75,xlim=c(8.5,16),ylim=c(0,4),ylab="",las=1,xlab="",axes=F,col=cols[1])
  for (i in 1:6) {
     lines(interval[agecat==i],nonw1[agecat==i],type="p",col=cols[i],lwd=1.75)
  }
}


plotNonw2p <- function(xdata) {
  nonw2p <- xdata$relnonwB2j_2p
   plot(interval[agecat==0],nonw2p[agecat==0],type="p",lwd=1.75,axes=F,xlim=c(8.5,16),ylim=c(0,6),ylab="",las=1,xlab="",col=cols[1])
   for (i in 1:6) {
     lines(interval[agecat==i],nonw2p[agecat==i],type="p",col=cols[i],lwd=1.75)
   }
}




# ~~~ plot graph ~~

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
plotPrimInf(pcdata5p9)
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,4,"a",cex=1.2)

plotTotRel12(pcdata5p9)
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,8,"b",cex=1.2)

plotTotRel3p(pcdata5p9)
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,12,"c",cex=1.2)

plotNonw1(pcdata5p9)
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,4,"d",cex=1.2)

plotNonw2p(pcdata5p9)
axis(1,at=seq(9,16),label=NA)
axis(2,las=1)
text(9,6,"e",cex=1.2)




# BOTTOM ROW

plotPrimInf(pcdata4p3)
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,4,"f",cex=1.2)

plotTotRel12(pcdata4p3)
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,8,"g",cex=1.2)

plotTotRel3p(pcdata4p3)
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,12,"h",cex=1.2)

plotNonw1(pcdata4p3)
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,4,"i",cex=1.2)

plotNonw2p(pcdata4p3)
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
text(9,6,"j",cex=1.2)












