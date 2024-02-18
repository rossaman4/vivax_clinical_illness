# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calledByRunStanModels.r
# runs STAN models for chosen parameter set
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# get corrrect data for the chosen parameter set 
if (paramSet==1) {
   xtemp<-xtemp5p9
   xtemp_seas<-xtemp5p9_seas
}
if (paramSet==2) {
   xtemp<-xtemp4p3
   xtemp_seas <- xtemp4p3_seas
}


set.seed(1236)

Ncats<-nrow(xtemp)

# number of cumulative infection predictions to monitor for graph
Npred<-51




# --- model B2 ----
#model <- stan_model("modelA2_B2_poisson.stan")
model <- stan_model("modelA2_B2_poisson_exponential_probIllFromPrimI.stan")
fit <- sampling(
	model,
	list(
		Ncats=nrow(xtemp),
		Nprimary=xtemp$prim_ppp2m*xtemp$nchildrenL,
		Nrelapsegp1=xtemp$relnonwB2j_1*xtemp$nchildrenL,
        Nrelapsegp2=xtemp$relnonwB2j_2p*xtemp$nchildrenL,
		cumh=xtemp$cumInfl,
		Nfever=xtemp$fever500,
        Npred=Npred,
		# xtemp_seas: to allow calculation of seasonal contribution of primary infections
		Ncats_seas=nrow(xtemp_seas),
    Prim_pp_seas=xtemp_seas$prim_ppp2m,
    Relgp1_pp_seas = xtemp_seas$relnonwB2j_1,
    Relgp2_pp_seas = xtemp_seas$relnonwB2j_2p,
    cumh_seas = xtemp_seas$cumInfl
	),
	warmup=500,
	iter=1000,
	cores=4,
	chains=4,
	open_progress=FALSE
)

#samples <- extract(fit, c("pp0", "pr10", "pr20", "xstar", "b1", "b2"), permuted=FALSE)
#mcmc_trace(samples)
#check_hmc_diagnostics(fit)
#stan_rhat(fit)
#should be close to 1

out1<-summary(fit, c("pp0", "pr10", "pr20", "xstar", "b1", "b2", "ppadjpred", "pr1adjpred", "pr2adjpred", "propnPrimI"))$summary

# save allchains 
if (paramSet==1) { write.table(out1, "output allchains/out1_B2_5p9param1_new.txt") }
if (paramSet==2) { write.table(out1, "output allchains/out1_B2_4p3param2_new.txt") }




# --- model A2 ----
model <- stan_model("modelA2_B2_poisson_exponential_probIllFromPrimI.stan")
fit <- sampling(
	model,
	list(
		Ncats=nrow(xtemp),
		Nprimary=xtemp$prim_ppp2m*xtemp$nchildrenL,
		Nrelapsegp1=xtemp$reltotA2j_12*xtemp$nchildrenL,
    Nrelapsegp2=xtemp$reltotA2j_3p*xtemp$nchildrenL,
		cumh=xtemp$cumInfl,
		Nfever=xtemp$fever500,
    Npred=Npred,
		# to allow calculation of seasonal contribution of primary infection
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
	open_progress=FALSE
)

out2<-summary(fit, c("pp0", "pr10", "pr20", "xstar", "b1", "b2", "ppadjpred", "pr1adjpred", "pr2adjpred", "propnPrimI"))$summary

#samples <- extract(fit, c("pp0", "pr10", "pr20", "xstar", "b1", "b2"), permuted=FALSE)
#mcmc_trace(samples)
#check_hmc_diagnostics(fit)
#stan_rhat(fit)
#should be close to 1

# save allchains 
if (paramSet==1) { write.table(out2, "output allchains/out1_A2_5p9param1_new.txt") }
if (paramSet==2) { write.table(out2, "output allchains/out1_A2_4p3param2_new.txt") }





# --- model B1 -----
#model <- stan_model("modelA1_B1_poisson.stan")
model <- stan_model("modelA1_B1_poisson_exponential_probIllFromPrimI.stan")
fit <- sampling(
	model,
	list(
		Ncats=nrow(xtemp),
		Nprimary=xtemp$prim_ppp2m*xtemp$nchildrenL,
		Nrelapse=(xtemp$relnonwB2j_1 + xtemp$relnonwB2j_2p)*xtemp$nchildrenL,
		cumh=xtemp$cumInfl,
		Nfever=xtemp$fever500,
        Npred=Npred,
		# added for predicting seasonal contribution
		Ncats_seas=nrow(xtemp_seas),
		Prim_pp_seas=xtemp_seas$prim_ppp2m,
		Relgp1_pp_seas = xtemp_seas$relnonwB2j_1 + xtemp_seas$relnonwB2j_2p,
		cumh_seas = xtemp_seas$cumInfl
	),
	warmup=500,
	iter=1000,
	cores=4,
	chains=4,
	open_progress=FALSE
)

out3<-summary(fit, c("pp0", "pr0", "xstar", "b1", "ppadjpred", "pradjpred","propnPrimI"))$summary

#samples <- extract(fit, c("pp0", "pr0", "xstar", "b1"), permuted=FALSE)
#mcmc_trace(samples)
#check_hmc_diagnostics(fit)
#stan_rhat(fit)

# save results 
if (paramSet==1) { write.table(out3, "output allchains/out1_B1_5p9param1_new.txt") }
if (paramSet==2) { write.table(out3, "output allchains/out1_B1_4p3param2_new.txt") }






# --- model A1 -----
model <- stan_model("modelA1_B1_poisson_exponential_probIllFromPrimI.stan")
fit <- sampling(
	model,
	list(
		Ncats=nrow(xtemp),
		Nprimary=xtemp$prim_ppp2m*xtemp$nchildrenL,
		Nrelapse=xtemp$reltotA1j_all*xtemp$nchildrenL,
		cumh=xtemp$cumInfl,
		Nfever=xtemp$fever500,
            Npred=Npred,		
		# added for predicting seasonal contribution
		Ncats_seas=nrow(xtemp_seas),
		Prim_pp_seas=xtemp_seas$prim_ppp2m,
		Relgp1_pp_seas = xtemp_seas$reltotA1j_all,
		cumh_seas = xtemp_seas$cumInfl
	),
	warmup=500,
	iter=1000,
	cores=4,
	chains=4,
	open_progress=FALSE
)


out4<-summary(fit, c("pp0", "pr0", "xstar", "b1", "ppadjpred", "pradjpred","propnPrimI"))$summary


#samples <- extract(fit, c("pp0", "pr0", "xstar", "b1", "propnPrimI"), permuted=FALSE)
#mcmc_trace(samples)
#check_hmc_diagnostics(fit)
#stan_rhat(fit)

# save results
if (paramSet==1) { write.table(out4, "output allchains/out1_A1_5p9param1_new.txt") }
if (paramSet==2) { write.table(out4, "output allchains/out1_A1_4p3param2_new.txt") }







# --- model A0 -----
model <- stan_model("modelA0_B0_poisson_exponential_probIllFromPrimI.stan")
fit <- sampling(
  model,
  list(
    Ncats=nrow(xtemp),
    Ninf=(xtemp$prim_ppp2m+xtemp$reltotA1j_all)*xtemp$nchildrenL,
    cumh=xtemp$cumInfl,
    Nfever=xtemp$fever500,
    Npred=Npred		
    ),
  warmup=500,
  iter=1000,
  cores=4,
  chains=4,
  open_progress=FALSE
)


out5<-summary(fit, c("pi0", "xstar","piadjpred"))$summary


#samples <- extract(fit, c("pp0", "pr0", "xstar", "b1", "propnPrimI"), permuted=FALSE)
#mcmc_trace(samples)
#check_hmc_diagnostics(fit)
#stan_rhat(fit)

# save results
if (paramSet==1) { write.table(out5, "output allchains/out1_A0_5p9param1_new.txt") }
if (paramSet==2) { write.table(out5, "output allchains/out1_A0_4p3param2_new.txt") }






# --- model B0 -----
model <- stan_model("modelA0_B0_poisson_exponential_probIllFromPrimI.stan")
fit <- sampling(
  model,
  list(
    Ncats=nrow(xtemp),
    Ninf=(xtemp$prim_ppp2m+xtemp$relnonwB2j_1 + xtemp$relnonwB2j_2p)*xtemp$nchildrenL,
    cumh=xtemp$cumInfl,
    Nfever=xtemp$fever500,
    Npred=Npred		
  ),
  warmup=500,
  iter=1000,
  cores=4,
  chains=4,
  open_progress=FALSE
)


out6<-summary(fit, c("pi0", "xstar","piadjpred"))$summary


#samples <- extract(fit, c("pp0", "pr0", "xstar", "b1"), permuted=FALSE)
#mcmc_trace(samples)
#check_hmc_diagnostics(fit)
#stan_rhat(fit)
#should be close to 1

# save results 
if (paramSet==1) { write.table(out6, "output allchains/out1_B0_5p9param1_new.txt") }
if (paramSet==2) { write.table(out6, "output allchains/out1_B0_4p3param2_new.txt") }








