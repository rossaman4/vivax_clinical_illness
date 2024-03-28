# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# S4 table_recoveryOfKnownParameterValues.r
#  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dpylr)
library(rstan)

# specify the path to your 'input files' folder
input_files_path = "C:/.../input files/"



# read in data and expected numbers of primary infections and relapses

# 5p9 (parameter set 1)
itnvill<-read.table(paste0(input_files_path,"itnvill_5p9param1.txt"), sep=" ", header=TRUE)
itnnovill<-read.table(paste0(input_files_path,"itnnovill_5p9param1.txt"), sep=" ", header=TRUE)
noitnvill<-read.table(paste0(input_files_path,"noitnvill_5p9param1.txt"), sep=" ", header=TRUE)
noitnnovill<-read.table(paste0(input_files_path,"noitnnovill_5p9param1.txt"), sep=" ", header=TRUE)
xtemp5p9<-rbind(noitnvill,itnnovill,noitnnovill,itnvill)



# 4p3 (parameter set 2)
itnvill<-read.table(paste0(input_files_path,"itnvill_4p3param2.txt"), sep=" ", header=TRUE)
itnnovill<-read.table(paste0(input_files_path,"itnnovill_4p3param2.txt"), sep=" ", header=TRUE)
noitnvill<-read.table(paste0(input_files_path,"noitnvill_4p3param2.txt"), sep=" ", header=TRUE)
noitnnovill<-read.table(paste0(input_files-path,"noitnnovill_4p3param2.txt"), sep=" ", header=TRUE)
xtemp4p3<-rbind(noitnvill,itnnovill,noitnnovill,itnvill)

# one record per covariate category and two-month interval
# prim_ppp2m = number of primary infections: per child per two-month interval
# relnonwB2j_1 = number of first relapses ppp2mo (classification Bj)
# relnonwB2j_2p = number of second or later relapses ppp2mo (classification Bj)
# ageGpl = age-group
# endIntervall = interval
# cumInfl = mean cumulative number of infections per child 
# fever500 = number of P vivax fevers
# nchildrenL = number of children
# reltotA1j_all = number of relapses: per child per two-month interval (classification A)
# reltotA2j_12 = number of first or second relapses ppp2mo (classification A)
# reltotA2j_3p = number of third or later relapses ppp2mo (classification A)




noitnnovill<-read.table("input files/noitnnovill_5p9.txt", sep=" ", header=TRUE)
xtemp5p9_seas <- noitnnovill %>% filter(ageGpl==4)
#noitnnovill<-read.table("input files/noitnnovill_4p3.txt", sep=" ", header=TRUE)
#xtemp4p3_seas <- noitnnovill %>% filter(ageGpl==4)


xtemp<-xtemp5p9
# take out rows with zero children
xtemp<-xtemp[xtemp$nchildrenL>0,]
xtemp_seas <- xtemp5p9_seas


# number of categories (age-, village, ITN use, and time-interval combinations)
Ncats <- nrow(xtemp)

# number of cumulative lifetime primary infections to predict over
Npred<-51


runTrials<-function(xstar_true, pp0_true, pr10_true, pr20_true, Ntrials) {

  # calculate teh expected number offevers with input parameter values
   Dh1_true <- exp(-xtemp$cumInfl*xstar_true)
   ppadj_true<-pp0_true*Dh1_true   
   pr1adj_true<-pr10_true*Dh1_true
   pr2adj_true<-pr20_true*Dh1_true
   expectedNum <- (ppadj_true*xtemp$prim_ppp2m + pr1adj_true*xtemp$reltotA2j_12 + pr2adj_true*xtemp$reltotA2j_3p) * xtemp$nchildrenL
  
  # prepare a store for results
   outputStore<-array(-9, dim=c(Ntrials, 4))
   colnames(outputStore)<-c("outPp0","outPr10","outPr20", "outXstar")

   model <- stan_model("modelA2_B2_poisson_exponential_probIllFromPrimI.stan")
  

   for (k in 1:Ntrials) {
       print(k)
    
       # sample 'observed' fevers for each simulated trial
       Nfever_samp<-rep(-9, Ncats)
       for (i in 1:Ncats) {
          xtemp$Nfever_samp[i] <- rpois(n=1, lambda=expectedNum[i])
       }
     
       
       # run model A2 for each trial ----
       fit <- sampling(
	      model,
	       list(
		          Ncats=Ncats,
		          Nprimary=xtemp$prim_ppp2m*xtemp$nchildrenL,
                  Nrelapsegp1=xtemp$reltotA2j_12*xtemp$nchildrenL,
                  Nrelapsegp2=xtemp$reltotA2j_3p*xtemp$nchildrenL,
		          cumh=xtemp$cumInfl,
		          Nfever=xtemp$Nfever_samp,
                  Npred=Npred,
		    	  # to allow calculation of seasonal contribution of primary infection (not used for recovery of parameter values but required as inputs to model)
		          Ncats_seas=nrow(xtemp_seas),
		          Prim_pp_seas=xtemp_seas$prim_ppp2m,
		          Relgp1_pp_seas = xtemp_seas$reltotA2j_12,
		          Relgp2_pp_seas = xtemp_seas$reltotA2j_3p,
	 	          cumh_seas = xtemp_seas$cumInfl   
	),
	warmup=500,
	iter=1000,
	cores=4,
	chains=4,
	open_progress=FALSE,
	control=list(adapt_delta=0.99),    
  )

    out1<-summary(fit, c("pp0", "pr10", "pr20", "xstar"))$summary

    # save estimates per trial to outputStore
    outputStore[k,]<-out1[,"50%"]
   }
return(outputStore)
}




# input known parameter values to be recovered and run model A2 (example model)
out_a1_n <- runTrials(xstar_true=0.05, pp0_true=0.8, pr10_true=0.5, pr20_true=0.2, Ntrials=50)
#write.table(out_a1_n, "recov_a1_n.txt")
out_a2_n <- runTrials(xstar_true=0.05, pp0_true=0.3, pr10_true=0.1, pr20_true=0.05, Ntrials=50)
#write.table(out_a2_n, "recov_a2_n.txt")
out_a3_n <- runTrials(xstar_true=0.2, pp0_true=0.8, pr10_true=0.5, pr20_true=0.2, Ntrials=50)
#write.table(out_a3_n, "recov_a3_n.txt")
out_a4_n <- runTrials(xstar_true=0.2, pp0_true=0.3, pr10_true=0.1, pr20_true=0.05, Ntrials=50)
#write.table(out_a4_n, "recov_a4_n.txt")





# make a table 

# gets estimates from all trials together
getEstimatesAllTrials<-function(xx) {
   resultsStore2<- rbind(quantile(xx$outXstar, c(0.5, 0.0, 0.025, 0.975, 1.0)),        
                         quantile(xx$outPp0, c(0.5, 0.0, 0.025, 0.975, 1.0)),
                         quantile(xx$outPr10, c(0.5, 0.0, 0.025, 0.975, 1.0)),
                         quantile(xx$outPr20, c(0.5, 0.0, 0.025, 0.975, 1.0)))
   rownames(resultsStore2)<-c("xstar","pp0","pr10","pr20")
   return(resultsStore2)
}



# n=n batch
out_a1_n<-read.table( "recov_a1_n.txt")
out_a2_n<-read.table( "recov_a2_n.txt")
out_a3_n<-read.table( "recov_a3_n.txt")
out_a4_n<-read.table( "recov_a4_n.txt")


getEstimatesAllTrials(as.data.frame(out_a1_n))
getEstimatesAllTrials(as.data.frame(out_a2_n))
getEstimatesAllTrials(as.data.frame(out_a3_n))
getEstimatesAllTrials(as.data.frame(out_a4_n))






















