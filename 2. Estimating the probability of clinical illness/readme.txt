2. Estimating the probability of clinical illness


The R script runStanModels.r is the master file for this sub-directory. 

It reads in the clinical data and expected numbers of primary infections and relapses from /input files (created in 1. Expected number of primary infections and relapses). The data are passed to the Stan models (modelA1_B1.stan for one category of relapses and modelA2_B2.stan for two categories of relapses).

The output Markov chains are stored in /output files. (The files here are smaller than those used in the manuscript due to filesize issues).

The R scripts in /figures and tables  re-create figures 2, 3, 4, and tables 3 and S4.
