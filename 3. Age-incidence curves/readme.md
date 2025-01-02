

### 3. Age-incidence curves (Figure 5)

The age-incidence curves for clinical episodes are simulated using OpenMalaria, a comprehensive simulation model for malaria dynamics and interventions.

The scenarios and output files are in /scenarios. 

If you would like to rerun the scenarios:

The wiki is available at: https://github.com/SwissTPH/openmalaria/wiki. The simulations here were carried out using version 47. 
To download and compile, follow the HowTo in the User guide or the Expert guide (from the menu on the right-hand side).

The released OpenMalaria code focuses on <i>P. falciparum</i> and the <i>P. vivax</i> components are not up to date (a pull request is in process https://github.com/SwissTPH/openmalaria/pull/400). Therefore the new vivax files need to be swapped in and compiled before use.

 - Copy WHVivax.cpp and WHVivax.h from /cpp to your openmalaria/model/Host/WithinHost folder
 - Copy vivax.xsd to the openmalaria/schema folder
 - Recompile
 - Copy the scenario XML files to be run from /scenarios to the openmalaria/build folder you created when you compiled OpenMalaria and run them

The R script fig5.r takes the output files and plots the predicted age-incidence curves   

