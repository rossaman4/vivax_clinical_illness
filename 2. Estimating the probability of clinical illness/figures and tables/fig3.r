# ~~~~~~~~~~~~~~
# fig3.r
# ~~~~~~~~~~~~~~



rm(list=ls())

# specify your '/output files' folder path
output_files_path = "C:/..../"


# up to 51 cumulative primary infections
numPadjCols <- 51

# number of columns for models A2 and B2
numPrimOrRel_A2B2 <- 3
# number of columns for models A1 and B1
numPrimOrRel_A1B1 <- 2
# number of centile values extracted
numCentileCols <- 3




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getCL
# function to get credible limits from the out1 files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getCL<-function(xx, allchains){
   k<-0
   for (i in 1:dim(allchains)[1]) {
      # identify first row
      if (rownames(allchains)[i]==paste0(xx,"[1]")) {k<-i}
    }  
    # take 51 rows for the max 51 cumulative infections with CL estimated
    return( allchains[k:(k+50),c("X50.","X2.5.","X97.5.")] )
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function: collectCL_A2B2
# for two groups of relapses
# first 3 columns are for primary infections
# next 3 columns for relapses first group of relapses (j)
# last 3 columns for second group of relapses (j)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collectCL_A2B2<-function(allchains) {
   predStore<-cbind( getCL("ppadjpred", allchains), getCL("pr1adjpred", allchains), getCL("pr2adjpred", allchains) )
   colnames(predStore)<-c("ppadj50","ppadj2.5","ppadj97.5","pr1adj50","pr1adj2.5","pr1adj97.5","pr2adj50","pr2adj2.5","pr2adj97.5")
   rownames(predStore)<-NULL
   return(predStore)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function: collectCL_A1B1
# for one group of relapses
# first sheet is for primary infections
# 2nd= relapses (j) 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collectCL_A1B1<-function(allchains) {
   predStore<-cbind(getCL("ppadjpred", allchains), getCL("pradjpred", allchains)) 
   colnames(predStore)<-c("ppadj50","ppadj2.5","ppadj97.5","pradj50","pradj2.5","pradj97.5")
   rownames(predStore)<-NULL   
   return(predStore)
}






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function: getMinMaxPred
# for the two parameterizations
# get min lower CI bound
# and max upper bound
# for figures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getMinMaxPred<-function(pred4p3, pred5p9) {
     nrecno <- nrow(pred4p3)
     pred<-pred4p3
     for (i in 1:nrecno) {
           pred$X2.5.[i] <- min(pred4p3$X2.5.[i], pred5p9$X2.5.[i])
           pred$X97.5.[i] <- max(pred4p3$X97.5.[i], pred5p9$X97.5.[i])
     }
return(pred)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For each model, read in results for primary infections and relapses by cumulative number of infections
# the text files are output from the Stan model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



B2_param2<-read.table(paste0(output_files_path,"out1_B2_4p3param2.txt"), header=TRUE)
pred_B2_param2 <- collectCL_A2B2(B2_param2)

B2_param1<-read.table(paste0(output_files_path,"out1_B2_5p9param1.txt"), header=TRUE)
pred_B2_param1 <- collectCL_A2B2(B2_param1)

A2_param2<-read.table(paste0(output_files_path,"out1_A2_4p3param2.txt"), header=TRUE)
pred_A2_param2 <- collectCL_A2B2(A2_param2)

A2_param1<-read.table(paste0(output_files_path,"out1_A2_5p9param1.txt"), header=TRUE)
pred_A2_param1 <- collectCL_A2B2(A2_param1)

B1_param2<-read.table(paste0(output_files_path,"out1_B1_4p3param2.txt"), header=TRUE)
pred_B1_param2 <- collectCL_A1B1(B1_param2)

B1_param1<-read.table(paste0(output_files_path,"out1_B1_5p9param1.txt"), header=TRUE)
pred_B1_param1 <- collectCL_A1B1(B1_param1)

A1_param2<-read.table(paste0(output_files_path,"out1_A1_4p3param2.txt"), header=TRUE)
pred_A1_param2 <- collectCL_A1B1(A1_param2)

A1_param1<-read.table(paste0(output_files_path,"out1_A1_5p9param1.txt"), header=TRUE)
pred_A1_param1 <- collectCL_A1B1(A1_param1)




# get 4p3 and 5p9 together (only for CI of both for plotting (as two extremes))

pred_A1_paramb <- collectCL_A1B1( getMinMaxPred(A1_param1, A1_param2))

pred_A2_paramb <- collectCL_A2B2( getMinMaxPred(A2_param1, A2_param2))

pred_B1_paramb <- collectCL_A1B1( getMinMaxPred(B1_param1, B1_param2))

pred_B2_paramb <- collectCL_A2B2( getMinMaxPred(B2_param1, B2_param2))



# ~~~~~~~~~~~~~~~~~~~~
# Figure 3
# ~~~~~~~~~~~~~~~~~~~
  


# functions to make the polygon plots
# axis labels are added outside the function since they depend on the position of the graph in the panel


# for shaded area (shows range of cumh where there is no data - fewer cumulative infections (cumh) than the youngest age-group in the cohort)
# h = lowest cumh in the data 
h = 7+1
# Np = highest cumh in the data 
Np = 51


plotA1B1<-function(pred, col1, col2, ltr) {
     plot(cumh, pred[,1], col=0, ylim=c(0,1.0), type="l", yaxt="n", xaxt="n", xlab="", ylab="" )
     axis(2, at=c(0,0.2,0.4,0.6,0.8,1), las=1, labels=NA )
     axis(1, at=c(0,10,20,30,40,50), labels=NA)
     # split by whether cumh is in range of data or not, align with cumh
     cumh2<-cumh[(h+1):Np]-1
     cumh1<-cumh[2:(h+1)]-1
     pred1<-pred[2:(h+1),]
     pred2<-pred[(h+1):Np,]
     polygon(c(cumh2,rev(cumh2)), c(pred2[,c("ppadj2.5")],rev(pred2[,c("ppadj97.5")])), col=adjustcolor(col1,alpha.f=0.3), border=NA )
     polygon(c(cumh1,rev(cumh1)), c(pred1[,c("ppadj2.5")],rev(pred1[,c("ppadj97.5")])), col=adjustcolor(col1,alpha.f=0.07), border=NA )
     polygon(c(cumh2,rev(cumh2)), c(pred2[,c("pradj2.5")],rev(pred2[,c("pradj97.5")])), col=adjustcolor(col2,alpha.f=0.7), border=NA )
     polygon(c(cumh1,rev(cumh1)), c(pred1[,c("pradj2.5")],rev(pred1[,c("pradj97.5")])), col=adjustcolor(col2,alpha.f=0.2), border=NA )
     text(50,0.98,ltr,cex=1.6)
}


plotA2B2<-function(pred, col1, col2, col3, ltr) {
     plot(cumh, pred[,1], col=0, ylim=c(0,1.0), type="l", yaxt="n", xlab="",ylab="",xaxt="n" )
     axis(2, at=c(0,0.2,0.4,0.6,0.8,1), labels=NA)
     axis(1, at=c(0,10,20,30,40,50), labels=NA)
     # split by whether cumh is in the range of data or not, align with cumh
     cumh2<- cumh[(h+1):Np]-1
     cumh1<-cumh[2:(h+1)]-1
     pred1<-pred[2:(h+1),]
     pred2<-pred[(h+1):Np,]
     polygon(c(cumh2,rev(cumh2)), c(pred2[,c("pr2adj2.5")],rev(pred2[,c("pr2adj97.5")])), col=adjustcolor(col3,alpha.f=0.4), border=NA )
     polygon(c(cumh1,rev(cumh1)), c(pred1[,c("pr2adj2.5")],rev(pred1[,c("pr2adj97.5")])), col=adjustcolor(col3,alpha.f=0.2), border=NA )
     polygon(c(cumh2,rev(cumh2)), c(pred2[,c("pr1adj2.5")],rev(pred2[,c("pr1adj97.5")])), col=adjustcolor(col2,alpha.f=0.2), border=NA )
     polygon(c(cumh1,rev(cumh1)), c(pred1[,c("pr1adj2.5")],rev(pred1[,c("pr1adj97.5")])), col=adjustcolor(col2,alpha.f=0.1), border=NA )
     polygon(c(cumh2,rev(cumh2)), c(pred2[,c("ppadj2.5")], rev(pred2[,c("ppadj97.5")])), col=adjustcolor(col1,alpha.f=0.3), border=NA )
     polygon(c(cumh1,rev(cumh1)), c(pred1[,c("ppadj2.5")], rev(pred1[,c("ppadj97.5")])), col=adjustcolor(col1,alpha.f=0.07), border=NA )
     text(50,0.98,ltr, cex=1.6)
}




  
cumh<-seq(1,51,1)

# set graphical parameters
par(oma=c(3,3.3,0,0))
par(mar=c(0.2,0.2,0.2,0.2))
par(xpd=NA)
# b,l,t,r
# length of tick marks
par(tcl=-0.3)
# where labels are placed
par(mgp=c(1.2,0.6,0))   

par(mfrow=c(2,2))

par(cex.axis=1.6, cex.lab=1.6)


# PLOT - with 5p9 (param set 1) and 4p3 (param set 2) together (as two extremes)
  
plotA1B1(pred_A1_paramb,"red","cornflowerblue","A1")
axis(2, at=c(0,0.2,0.4,0.6,0.8,1), las=1 )
mtext("probability", side=2, line=2.5) 

plotA2B2(pred_A2_paramb,"red","blue","goldenrod","A2")


plotA1B1(pred_B1_paramb,"red","cornflowerblue","B1")
axis(2, at=c(0,0.2,0.4,0.6,0.8,1), las=1 )
mtext("probability", side=2, line=2.5) 
axis(1, at=c(0,10,20,30,40,50))


plotA2B2(pred_B2_paramb,"red","blue","goldenrod","B2")
axis(1, at=c(0,10,20,30,40,50))

mtext("cumulative number of primary infections", outer=TRUE, side=1, line=1.4)








