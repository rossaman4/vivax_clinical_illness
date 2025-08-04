
rm(list=ls())

require(ggplot2)
require(ggpubr)
library("RColorBrewer")


# set sub-directory for fig a output files
output_dirA<-"C:/out_dirA"

# set sub-directory for figs b and c output files
output_dirB<-"C:/output_dirB"


# sub-directory where the R scripts for the functions needed are
# figValidation_vanuatu_function.r
# figValidation_wosera_function.r
fncode_dir<-"C:/fig5/"




# -- Fig a --- 
  
# read in output files for each model in turn and extract results

subdir <- c("4p3long_A1", "4p3long_A2", "4p3long_B1", "4p3long_B2", "5p9short_A1", "5p9short_A2", "5p9short_B1", "5p9short_B2")
n_subdir <- length(subdir)  

for (s in 1:n_subdir) {
   tempdir <- file.path(output_dirA, paste0(subdir[s],"/output/") ) 
   setwd(tempdir)
   source(paste0(fncode_dir,"fig5_wosera_function.R"))
   df_name <- paste0("ageIncid", subdir[s])
   assign(df_name, ageIncid)
}




# Combine all model variants using a polygon (get min and max of predicted values)
numAgegps<-30
numTrtCov<-10
maxAgeIncid<-array(0, dim=c(numAgegps,numTrtCov))
minAgeIncid<-array(0, dim=c(numAgegps,numTrtCov))


for (i in 1:numTrtCov) {
  for (j in 1:numAgegps)  {
     maxAgeIncid[j,i]<-max(ageIncid5p9short_A1[j,i], ageIncid4p3long_A1[j,i],
                           ageIncid5p9short_A2[j,i], ageIncid4p3long_A2[j,i],
                           ageIncid5p9short_B2[j,i], ageIncid4p3long_B2[j,i],
                           ageIncid5p9short_B1[j,i], ageIncid4p3long_B1[j,i])

     minAgeIncid[j,i]<-min(ageIncid5p9short_A1[j,i], ageIncid4p3long_A1[j,i],
                           ageIncid5p9short_A2[j,i], ageIncid4p3long_A2[j,i],
                           ageIncid5p9short_B2[j,i], ageIncid4p3long_B2[j,i],
                           ageIncid5p9short_B1[j,i], ageIncid4p3long_B1[j,i])
     }
}


# observed incidence in the Wosera 
agecatW<-c(0.5,1.5,3,5,8,15)
incidPerYearW<-c(0.047058824,0.376470588,0.305882353,0.164705882,0.047058824,0.047058824)



# PLOT FIG a

# colour palette
cols=brewer.pal(11,"PiYG")[c(5,4,3,2,1,2,3,4,5)]



par(mfrow=c(1,3))

par(oma=c(0,0,0,0.5))
par(mar=c(2.5,2.5,0,0))
# where labels are placed
par(mgp=c(1.5,0.3,0))   
# length of tick marks
par(tcl=-0.2)
# b,l,t,r


minAgeIncidW<-minAgeIncid
maxAgeIncidW<-maxAgeIncid

p1 <- ggplot() + 
  xlim(0,10) +
  ylim(0,1.3) +
  # opaque ribbons for each EIR 
   geom_ribbon(aes(x=ageGps, ymin=minAgeIncidW[,7],ymax=maxAgeIncidW[,7]), fill=cols[7], alpha=4/10) +
   geom_ribbon(aes(x=ageGps, ymin=minAgeIncidW[,2],ymax=maxAgeIncidW[,2]), fill=cols[2], alpha=4/10) +
   geom_ribbon(aes(x=ageGps, ymin=minAgeIncidW[,6],ymax=maxAgeIncidW[,6]), fill=cols[6], alpha=4/10) +
   geom_ribbon(aes(x=ageGps, ymin=minAgeIncidW[,3],ymax=maxAgeIncidW[,3]), fill=cols[3], alpha=4/10) +
   geom_ribbon(aes(x=ageGps, ymin=minAgeIncidW[,5],ymax=maxAgeIncidW[,5]), fill=cols[5], alpha=4/10) +
   geom_ribbon(aes(x=ageGps, ymin=minAgeIncidW[,4],ymax=maxAgeIncidW[,4]), fill=cols[4], alpha=4/10) +
  # add Wosera observed data
  geom_point(aes(x=agecatW, y=incidPerYearW), col="blue") +
  theme_bw() +
  ylab("number of cases pppy") +
  xlab("age in years") +
  theme(plot.margin = margin(t=1, r=1, b=0, l=0))





# --- Fig b -----

# Vanuatu validation

eirList<-c(1,2,5,10,20,30,50)


# read in output files for each model in turn and extract results

subdir <- c("4p3long_A1", "4p3long_A2", "4p3long_B1", "4p3long_B2", "5p9short_A1", "5p9short_A2", "5p9short_B1", "5p9short_B2")
n_subdir <- length(subdir)  

for (s in 1:n_subdir) {
  tempdir <- file.path(output_dirB, paste0(subdir[s],"/output/") ) 
  setwd(tempdir)
  source(paste0(fncode_dir,"fig5_vanuatu_function.R"))
  df_name <- paste0("ageIncid", subdir[s])
  assign(df_name, ageIncid)
  print(s)
}



# get min and max prediction for each age-group
numAgegps<-30
numEIR<-7
maxAgeIncid<-array(0, dim=c(numAgegps,numEIR))
minAgeIncid<-array(0, dim=c(numAgegps,numEIR))


for (i in 1:7) {
  for (j in 1:numAgegps)  {
     maxAgeIncid[j,i]<-max(ageIncid5p9short_A1[j,i], ageIncid4p3long_A1[j,i],
                           ageIncid5p9short_A2[j,i], ageIncid4p3long_A2[j,i],
                           ageIncid5p9short_B2[j,i], ageIncid4p3long_B2[j,i],
                           ageIncid5p9short_B1[j,i], ageIncid4p3long_B1[j,i])

     minAgeIncid[j,i]<-min(ageIncid5p9short_A1[j,i], ageIncid4p3long_A1[j,i],
                           ageIncid5p9short_A2[j,i], ageIncid4p3long_A2[j,i],
                           ageIncid5p9short_B2[j,i], ageIncid4p3long_B2[j,i],
                           ageIncid5p9short_B1[j,i], ageIncid4p3long_B1[j,i])
     }
}




# observed incidence in Vanuatu 

# Vanuatu (Extracted from Maitland et al, 1996)
# weekly active surveillance, May1992-May1994
# If took antimalarial in previous 4 weeks, visit was excluded
# fieldworker holidays in wet season and dry season (August and April)


agecat92<-c(0.5,2,4,7.5)
agecat94<-c(0.55, 2.1, 4.1, 7.6)

incidV_92_93<-c(0.9,1.1,0.7,0.2)
incidV_93_94<-c(0.5,1.2,0.5,0.3)

# CI
# number of week-observations
n92 <- c(2096,4455,4532,4210)
pyn92<-n92/52
CI_92_lower <- incidV_92_93 - (1.96*sqrt(incidV_92_93/pyn92))
CI_92_upper <- incidV_92_93 + (1.96*sqrt(incidV_92_93/pyn92))

n94 <- c(2051, 4520, 5037, 10725)
pyn94<-n94/52
CI_94_lower <- incidV_93_94 - (1.96*sqrt(incidV_93_94/pyn94))
CI_94_upper <- incidV_93_94 + (1.96*sqrt(incidV_93_94/pyn94))


#-- PLOT FIG b --

# colour palette
library("RColorBrewer")
cols=c(brewer.pal(11,"PiYG")[c(7,8,9,10)], brewer.pal(11,"BrBG")[c(9,8,7)])


p2 <- ggplot() + 
       xlim(0,10) +
      # opaque ribbons for each EIR 
       geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,1],ymax=maxAgeIncid[,1]), fill=cols[1], alpha=5/10) +
       geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,7],ymax=maxAgeIncid[,7]), fill=cols[7], alpha=5/10) +
       geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,2],ymax=maxAgeIncid[,2]), fill=cols[2], alpha=5/10) +
       geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,6],ymax=maxAgeIncid[,6]), fill=cols[6], alpha=5/10) +
       geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,3],ymax=maxAgeIncid[,3]), fill=cols[3], alpha=5/10) +
       geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,5],ymax=maxAgeIncid[,5]), fill=cols[5], alpha=5/10) +
       geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,4],ymax=maxAgeIncid[,4]), fill=cols[4], alpha=5/10) +
       # add Vanuatu observed data
       geom_pointrange(aes(x=agecat92, y=incidV_92_93, ymin=CI_92_lower, ymax=CI_92_upper), col="blue", pch=17 ) +
       geom_pointrange(aes(x=agecat94, y=incidV_93_94, ymin=CI_94_lower, ymax=CI_94_upper), col="red") +
       theme_bw() +
       ylab("number of cases pppy") +
       xlab("age in years") +
       theme(plot.margin = margin(t=1, r=1, b=0, l=0))
p2  





# -- PLOT C ---

# IPTi control arm 3-15mo: observed incidence
month<-seq(1:13)+2+0.5
incidpv<-c(0.155,0.281,0.396,0.476,0.766,0.534,0.613,0.587,0.733,0.668,0.717,0.569,0.600)
cilo<-c(0.0646,0.160,0.246,0.307,0.541,0.351,0.414,0.394,0.512,0.458,0.498,0.378,0.402)
cihi<-c(0.373,0.496,0.638,0.738,1.082,0.809,0.906,0.876,1.048,0.974,1.031,0.856,0.895)



# colour palette
cols=c(brewer.pal(11,"PiYG")[c(7,8,9,10)], brewer.pal(11,"BrBG")[c(9,8,7)])


# plot C 

p3 <- ggplot() + 
  xlim(0,1.3) +
  # opaque ribbons for each EIR (need to code this better)
  geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,1],ymax=maxAgeIncid[,1]), fill=cols[1], alpha=5/10) +
  geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,7],ymax=maxAgeIncid[,7]), fill=cols[7], alpha=5/10) +
  geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,2],ymax=maxAgeIncid[,2]), fill=cols[2], alpha=5/10) +
  geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,6],ymax=maxAgeIncid[,6]), fill=cols[6], alpha=5/10) +
  geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,3],ymax=maxAgeIncid[,3]), fill=cols[3], alpha=5/10) +
  geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,5],ymax=maxAgeIncid[,5]), fill=cols[5], alpha=5/10) +
  geom_ribbon(aes(x=ageGps, ymin=minAgeIncid[,4],ymax=maxAgeIncid[,4]), fill=cols[4], alpha=5/10) +
  # add IPTi observed data
  geom_pointrange(aes(x=month/12, y=incidpv, ymin=cilo, ymax=cihi), col="black" ) +
  theme_bw() +
  ylab("number of cases pppy") +
  xlab("age in years") +
  theme(plot.margin = margin(t=1, r=1, b=0, l=0))
p3  




# MAKE GGPLOT GRAPH WITH ALL THREE PLOTS TOGETHER

figure <- ggarrange(p1, p2, p3, ncol=3, nrow=1)
figure


p123 <- ggarrange(p1, p2, p3, ncol=3, nrow=1)






