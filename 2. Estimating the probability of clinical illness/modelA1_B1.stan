

data {
	int             Ncats;
	int             Nfever[Ncats];
	real<lower=0.0> Nprimary[Ncats];
	real<lower=0.0> Nrelapse[Ncats];
	real<lower=1.0> cumh[Ncats];
	int             Npred;
	
	int<lower=1>     Ncats_seas;
	real<lower=0.0>  Prim_pp_seas[Ncats_seas];
	real<lower=0.0>  Relgp1_pp_seas[Ncats_seas];
	real<lower=0.0>  cumh_seas[Ncats_seas];
}

parameters {
	real<lower=0.0, upper=1.0> pr0;
	real<lower=pr0, upper=1.0> pp0;
	real<lower=0.0> xstar;
}


transformed parameters {
	real<lower=0.0, upper=1.0> Dh1[Ncats];
	real<lower=0.0, upper=1.0> ppadj[Ncats];
	real<lower=0.0, upper=1.0> pradj[Ncats];
	real<lower=0.0> lp[Ncats];
  
	
	for (i in 1:Ncats){
        Dh1[i] = exp(- cumh[i] * xstar);
		ppadj[i] = pp0 * Dh1[i];
		pradj[i] = pr0 * Dh1[i];
		// linear predictor
		lp[i] =  Nprimary[i]*ppadj[i] + Nrelapse[i]*pradj[i];      
        }
}


model {
	pr0 ~ uniform(0, 1);
	pp0 ~ uniform(0, 1);
	xstar ~ uniform(0, 20);
	// xstar could be cauchy also
  
	for (i in 1:Ncats) {	    
		Nfever[i] ~ poisson(lp[i]);
	}	
}

generated quantities{
    real<lower=0.0> b1;
	real<lower=0.0, upper=1.0> ppadjpred[Npred];
	real<lower=0.0, upper=1.0> pradjpred[Npred];
	real<lower=0.0, upper=1.0> propnPrimI[Ncats_seas];
	real Dh1_s[Ncats_seas];
	real ppadj_s[Ncats_seas];
	real pradj_s[Ncats_seas];
	real pk0;
	
    // ratio of probabilities for relapse and primary infection
	b1 = pr0/pp0;
	
	// pk0: p0 for relapses only if primary infection caused illness
	pk0 = pr0/pp0; 
	// same thing as above !!
	
	
	// credible limits for the predicted lines
        for (k in 1:Npred) {
            ppadjpred[k]= pp0*exp(- (k-1) * xstar) ; 
            pradjpred[k]= pr0*exp(- (k-1) * xstar) ;
        }
	
	// predict seasonal contribution of primary infection and relapses to clinical illness
	for (s in 1:Ncats_seas) {
	    Dh1_s[s] = exp(-cumh_seas[s] * xstar);
	    ppadj_s[s] = pp0 * Dh1_s[s];
	    pradj_s[s] = pr0 * Dh1_s[s];
		  
            propnPrimI[s] =  (Prim_pp_seas[s]*ppadj_s[s]) / ((Prim_pp_seas[s]*ppadj_s[s]) + (Relgp1_pp_seas[s]*pradj_s[s]));
	}
	
}
  







