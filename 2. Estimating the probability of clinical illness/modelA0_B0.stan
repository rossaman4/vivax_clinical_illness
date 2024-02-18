

data {
	int             Ncats;
	int             Nfever[Ncats];
	real<lower=0.0> Ninf[Ncats];
	real<lower=1.0> cumh[Ncats];
	int             Npred;
}

parameters {
	real<lower=0.0, upper=1.0> pi0;
	real<lower=0.0> xstar;
}


transformed parameters {
	real<lower=0.0, upper=1.0> Dh1[Ncats];
	real<lower=0.0, upper=1.0> piadj[Ncats];
	real<lower=0.0> lp[Ncats];
  
	
	for (i in 1:Ncats){
        Dh1[i] = exp(-cumh[i] * xstar);
		piadj[i] = pi0 * Dh1[i];
		// linear predictor
		lp[i] =  Ninf[i]*piadj[i] ;      
    }
}



model {
	pi0 ~ uniform(0, 1);
	xstar ~ uniform(0, 20);
	// xstar could be cauchy also
  
	for (i in 1:Ncats) {	    
		Nfever[i] ~ poisson(lp[i]);
	}	
}

generated quantities{
 	real<lower=0.0, upper=1.0> piadjpred[Npred];
		  
	// credible limits for the predicted lines
        for (k in 1:Npred) {
            piadjpred[k]= pi0*exp(- (k-1) * xstar) ; 
        }

}
  







