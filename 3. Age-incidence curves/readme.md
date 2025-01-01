

### 3. Age-incidence curves (Figure 5)

The age-incidence curves for clinical episodes are simulated using OpenMalaria, a comprehensive simulation model for malaria dynamics and interventions.
The wiki is available at: https://github.com/SwissTPH/openmalaria/wiki. The simulations here were carried out using version 47. To download and compile, follow the HowTo in the User guide or the Expert guide (from the menu on the right-hand side).

The released OpenMalaria code focuses on <i>P. falciparum</i> and the <i>P. vivax</i> components are not up to date. Therefore the new vivax files need to be swapped in and compiled before use.

 - Copy the scenario XML files to be run from /scenarios to the openmalaria/build folder you created when you compiled OpenMalaria 
 - The C++ code is in /cpp. Copy WHVivax.cpp and WHVivax.h to the openmalaria/model/Host/WithinHost folder
 - Copy vivax.xsd to the openmalaria/schema folder
 - Recompile


The R script fig5.r will take the output files and plot the predicted age-incidence curves   

