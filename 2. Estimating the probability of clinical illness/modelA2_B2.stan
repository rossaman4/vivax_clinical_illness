
data {
	int             Ncats;
	int             Nfever[Ncats];
	real<lower=0.0> Nprimary[Ncats];
	real<lower=0.0> Nrelapsegp1[Ncats];
	real<lower=0.0> Nrelapsegp2[Ncats];
	real<lower=1.0> cumh[Ncats];
	int             Npred;
	
	int<lower=1>     Ncats_seas;
	real<lower=0.0>  Prim_pp_seas[Ncats_seas];
	real<lower=0.0>  Relgp1_pp_seas[Ncats_seas];
	real<lower=0.0>  Relgp2_pp_seas[Ncats_seas];
	real<lower=0.0>  cumh_seas[Ncats_seas];
}

parameters {
 	real<lower=0.0, upper=1.0> pr20;
	real<lower=pr20, upper=1.0> pr10;
	real<lower=pr10, upper=1.0> pp0;
	real<lower=0.0> xstar;
}


transformed parameters {
	real<lower=0.0, upper=1.0> Dh1[Ncats];
	real<lower=0.0, upper=1.0> ppadj[Ncats];
	real<lower=0.0, upper=1.0> pr1adj[Ncats];
	real<lower=0.0, upper=1.0> pr2adj[Ncats];
	real<lower=0.0> lp[Ncats];
	

	for (i in 1:Ncats){
        Dh1[i] = exp(-xstar * cumh[i]);
		ppadj[i] = pp0 * Dh1[i];
		pr1adj[i] = pr10 * Dh1[i];
		pr2adj[i] = pr20 * Dh1[i];
		// linear predictor
		lp[i] =  Nprimary[i]*ppadj[i] + Nrelapsegp1[i]*pr1adj[i] + Nrelapsegp2[i]*pr2adj[i];
    }
}



model {
    pr10 ~ uniform(0, 1);
	pr20 ~ uniform(0, 1);
	pp0 ~ uniform(0, 1);
	//xstar ~ cauchy(0, 1);
	xstar ~ uniform(0,100);
	
	for (i in 1:Ncats){	    
		Nfever[i] ~ poisson(lp[i]);
	}	
}



generated quantities{
    real<lower=0.0> b1;
	real<lower=0.0> b2;
    real<lower=0.0, upper=1.0> ppadjpred[Npred];
	real<lower=0.0, upper=1.0> pr1adjpred[Npred];
	real<lower=0.0, upper=1.0> pr2adjpred[Npred];
	real<lower=0.0, upper=1.0> propnPrimI[Ncats_seas];
	real Dh1_s[Ncats_seas];
	real ppadj_s[Ncats_seas];
	real pr1adj_s[Ncats_seas];
	real pr2adj_s[Ncats_seas];
		
	// ratio of probabilities for primary infection and relapse
	b1 = pr10/pp0;
	b2 = pr20/pr10;
	

    // credible limits for the predicted lines
    for (k in 1:Npred) {
       	 ppadjpred[k]= pp0*exp(-xstar * (k-1)); 
         pr1adjpred[k]= pr10*exp(-xstar * (k-1));
	     pr2adjpred[k]= pr20*exp(-xstar * (k-1));
    }
	
	
	
	// predict seasonal contribution of primary infection and relapses to clinical illness
	for (s in 1:Ncats_seas) {
	    Dh1_s[s] = exp(-cumh_seas[s] * xstar);
	    ppadj_s[s] = pp0 * Dh1_s[s];
	    pr1adj_s[s] = pr10 * Dh1_s[s];
		pr2adj_s[s] = pr20 * Dh1_s[s];
				  
        propnPrimI[s] =  (Prim_pp_seas[s]*ppadj_s[s]) / ((Prim_pp_seas[s]*ppadj_s[s]) + (Relgp1_pp_seas[s]*pr1adj_s[s]) + (Relgp2_pp_seas[s]*pr2adj_s[s]));
	}

}












