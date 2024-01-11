Index to files for 1. Expected numbers of primary infectinos and relapses/simulation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

runSimul.r
Master file to run the simulation code for each parameter set and combination of ITN and village covariates separately.
Each simulation provides the expected number of primary infections and relapses for each age-group and two-month interval.

note 1: You will need to change the filepath at the top.
note 2: The number of individuals simulated (numIndiv) is set to 10 for testing (in setSimulInputs.r).
Once tested, it can be set to 1000 to minimise stochasticity.


simul.r
For each simulation, runSimul.r calls simul.r
This script then calls the other scripts in turn


cumLifetimeInf.r 
Creates a dataset of the expected cumulative number of infections for each age-group and two-month period.
These are included as a variable in the database for analysis (2.).
They can be run each time or, more conveniently, written to a file and read-in.
A ready-made file is provided (and used in the code) - cumInf_byIntAgegp_noitn_novill.txt.






