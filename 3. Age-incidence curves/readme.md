

### 3. Age-incidence curves (Figure 5)

The age-incidence curves for clinical episodes are simulated using OpenMalaria, a comprehensive simulation model for malaria dynamics and interventions.
The wiki is available at: https://github.com/SwissTPH/openmalaria/wiki

The simulations here were carried out using version 45. The instructions for installing OpenMalaria are given in the wiki.

The released OpenMalaria code focuses on <i>P. falciparum</i> and the <i>P. vivax</i> components are not up to date. Therefore the new vivax files need to be swapped in and compiled before use. The C++ code for each vivax model is given in /cpp. The simulations were run on a unix cluster - the codes to swap in new C++ files, compile and run OpenMalaria are given in the shell script *.sh files.   

