# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fig4.r
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


output_files_path = "C:/.../"

library("RColorBrewer")


# get mean and CL for each model & parameterization
# assemble estimates for mean, 2.5% and 97.5% for each model variant
# the last 8 records from each output file relate to the seasonal pattern (propnPrimI)



# 4p3 - parameter set 2
B2_param2<-read.table(paste0(output_files_path,"out1_B2_4p3param2.txt"), header=TRUE)
B1_param2<-read.table(paste0(output_files_path,"out1_B1_4p3param2.txt"), header=TRUE)
A2_param2<-read.table(paste0(output_files_path,"out1_A2_4p3param2.txt"), header=TRUE)
A1_param2<-read.table(paste0(output_files_path,"out1_A1_4p3param2.txt"), header=TRUE)

B2_param2 <- tail(B2_param2, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]
B2_param2
B1_param2 <- tail(B1_param2, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]
A2_param2 <- tail(A2_param2, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]
A1_param2 <- tail(A1_param2, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]



# 5p9 - parameter set 1
B2_param1<-read.table(paste0(output_files_path,"out1_B2_5p9param1.txt"), header=TRUE)
B1_param1<-read.table(paste0(output_files_path,"out1_B1_5p9param1.txt"), header=TRUE)
A2_param1<-read.table(paste0(output_files_path,"out1_A2_5p9param1.txt"), header=TRUE)
A1_param1<-read.table(paste0(output_files_path,"out1_A1_5p9param1.txt"), header=TRUE)

B2_param1 <- tail(B2_param1, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]
B1_param1 <- tail(B1_param1, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]
A2_param1 <- tail(A2_param1, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]
A1_param1 <- tail(A1_param1, n=8)[,c("mean","X50.","X2.5.", "X97.5.")]




# overall mean proportion - min and max
prop.df<-cbind(B2_param2$mean, B1_param2$mean,A2_param2$mean,A1_param2$mean,
            B2_param1$mean,B1_param2$mean,A2_param1$mean,A1_param1$mean)
# median can also be used
# prop.df<-cbind(B2_param2$X50., B1_param2$X50., A2_param2$X50., A1_param2$X50.,
#               B2_param1$X50., B1_param1$X50., A2_param1$X50., A1_param1$X50.)
prop.min <- apply(prop.df, 1, FUN=min)
prop.max <- apply(prop.df, 1, FUN=max)



# 2.5th centile - min 
proplb.df<-cbind(B2_param2$X2.5., B1_param2$X2.5.,A2_param2$X2.5., A1_param2$X2.5.,
               B2_param1$X2.5., B1_param1$X2.5., A2_param1$X2.5., A1_param1$X2.5.)
proplb.min <- apply(proplb.df, 1, FUN=min)

# 97.5th centile - max
propub.df<-cbind(B2_param2$X97.5., B1_param2$X97.5.,A2_param2$X97.5., A1_param2$X97.5.,
                 B2_param1$X97.5., B1_param1$X97.5., A2_param1$X97.5., A1_param1$X97.5.)
propub.max <- apply(propub.df, 1, FUN=max)





roundLabel<-c("Jun06","Aug06","Oct06","Dec06","Feb07","Apr07","Jun07","Aug07")
endInterval<-seq(8.5,16.5)



# extend variables for plotting which will give the 2-month blocks for the intervals
endIntervalb <- c(8.5, rep(seq(9.5,15.5,1),each=2), 16.5)
propMinCLb <- rep(proplb.min, each=2)
propMaxCLb <- rep(propub.max, each=2)
propMinb <- rep(prop.min, each=2)
propMaxb <- rep(prop.max, each=2)



cols<-brewer.pal(8,"PuOr")

par(mfrow=c(1,1))
par(oma=c(0.5,0,0.5,0.5))
par(mar=c(1,2.5,0,0))
# where labels are placed
par(mgp=c(1.5,0.3,0))   
# length of tick marks
par(tcl=-0.2)

plot(endIntervalb, propMaxCLb, ylim=c(0,1.0),type="n", ylab="proportion", xlab="", xaxt="n", yaxt="n")
axis(1,at=seq(9,16),label=roundLabel)
axis(2,las=1)
polygon(c(endIntervalb, rev(endIntervalb)), c(propMinCLb, rev(propMaxCLb)), border=NA, col="lightskyblue1")
polygon(c(endIntervalb ,rev(endIntervalb)), c(propMinb, rev(propMaxb)), border=NA, col="cornflowerblue")












