
# runSimul.r
# run simulations to gain number of primary infections and relapses per village, ITN use and for each age-group and two-month interval
# eight runs to cover two parameterizations and four ITN and village combinations

rm(list=ls())


# set working directory
setwd("...")


# INDICATOR FOR ITN AND VILLAGE FOR THE SIMULATION (itnvillID)
# The simulation will be run for each ITN-village combination and for each parameter set 
# 1=no itn, no vill (no ITN use, village=Ilaita)
# 2=itn,  no village 
# 3=no itn, village (no ITN used, village=Sunuhu)
# 4=itn, village

# INDICATOR FOR PARAMETERIZATION SET (paramSetID)
# 2=4p3 (parameter set 2), 1=5p9 (parameter set 1)
# parameter sets allow for estimates from two different sources for the duration of blood-stage infection 
# described in (ii) Duration of blood-stage infections (S1 Appendix)




# DEFINE AND RUN EIGHT ITN/VILLAGE and PARAMETER VALUE SET COMBINATIONS

numScenarioSets = 8
itnvillIDList <- c(1,2,3,4,1,2,3,4)
paramSetIDList <- c(2,2,2,2,1,1,1,1)
file_name<-c("noitnnovill_4p3", "itnnovill_4p3", "noitnvill_4p3","itnvill_4p3", 
             "noitnnovill_5p9", "itnnovill_5p9", "noitnvill_5p9","itnvill_5p9")



for (scenario in 1:numScenarioSets) {
    itnvillID = itnvillIDList[scenario]
    paramSetID = paramSetIDList[scenario]
    source("simul.r")
    write.table(expected_infections, paste0(file_name[scenario],".txt"))
}



# VARIABLES IN THE OUTPUT DATASETS
# prim_ppp2m: simulated number of primary infections per child per 2month interval
# relnonwB2j_1: simulated number of relapses ppp2mo (B2j first relapse)
# relnonwB2j_2p: simulated number of relapses ppp2mo (B2j second or later relapse)
# ageGpl: age group category
# endIntervall: indicates which 2 month interval
# cumInfl: mean cumulative number of primary infections for a child (specific to age-group and interval) 
# fever500: number of observed fever with P.vivax >500ul
# nchildrenL: number of observed children in age-group and interval
# reltotA1j_all: simulated number of relapses ppp2mo (A1j)
# reltotA2j_12: simulated number of relapses ppp2mo (A2j first or second relapse)
# reltotA2j_3p: simulated number of relapses ppp2mo (A2j third or later relapse)
# (relapses classifications are explained in Table 3)














