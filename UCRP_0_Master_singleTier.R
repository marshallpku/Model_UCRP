
gc()

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************

# Plan information
#source("UCRP_Data_RP2014.R")
#source("UCRP_Data_PlanInfo.R")
load("./Data/UCRP.PlanInfo.RData")  # for all tiers

# Initial population
source("UCRP_Data_Population.R")    # for all tiers

# Decrement tables
source("UCRP_Data_Decrements.R")   # for all tiers




#**********************************************
##   Modify initial data ####
#**********************************************

## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terminated_all %<>% mutate(nterm = 0)


## Exclude initial terms with ea < 20: Data_population, line 504 
 # init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
 #                                 ea >= Global_paramlist$min.ea)


## Exclude the initial amortization basis when testing the program.
if(!paramlist$useAVamort) init_amort_raw %<>% mutate(amount.annual = 0) 

## Exclude the external fund. (currently only STIP borrowing)
if(!paramlist$useExtFund) extFund %<>% mutate_each(funs(. * 0), -year)


#*********************************************************************************************************
# 1.2  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("UCRP_Model_InvReturns.R")
i.r <- gen_returns()
i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************

source("UCRP_Data_PlanData_Transform.R")

salary  <- get_salary_proc(paramlist$Tier_select, paramlist$w.salgrowth.method)
benefit <- get_benefit_tier(paramlist$Tier_select)
init_pop       <- get_initPop_tier(paramlist$Tier_select)
entrants_dist  <- get_entrantsDist_tier(paramlist$Tier_select)


# Chnange variable names
decrement.ucrp %<>% rename_("pxT" = paste0("pxT.",          paramlist$Tier_select),
                            "qxr.la"   = paste0("qxr.la.",  paramlist$Tier_select),
                            "qxr.ca"   = paste0("qxr.ca.",  paramlist$Tier_select),
                            "qxr.LSC"  = paste0("qxr.LSC.", paramlist$Tier_select),
                            "qxr"      = paste0("qxr.",     paramlist$Tier_select),
                            "qxt"      = paste0("qxt.",     paramlist$Tier_select)
)


bfactor %<>% mutate(Tier = paramlist$Tier_select,
                    bfactor = ifelse(Tier == "t13", bf.13, bf.non13)) %>% 
             select(age, bfactor)



#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("UCRP_Model_Demographics.R")
gc()
pop <- get_Population()




#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Model_IndivLiab.R")
gc()


liab <- get_indivLab(decrement.ucrp,
                     salary,
                     benefit,
                     bfactor,
                     get(paste0("init_terminated.",paramlist$Tier_select)),
                     .Tier_select = paramlist$Tier_select 
)




#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("UCRP_Model_ContingentAnnuity.R")
liab.ca <- get_contingentAnnuity(decrement.ucrp)


#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Model_AggLiab.R")
gc()

AggLiab <- get_AggLiab(get(paste0("init_beneficiaries.", paramlist$Tier_select)),
                       liab,
                       liab.ca,
                       pop) 




#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("UCRP_Model_Sim.R")
penSim_results <- run_sim(paramlist$Tier_select, AggLiab)




#*********************************************************************************************************
# 7  Showing results ####
#*********************************************************************************************************


var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", 
                 #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
                 #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                 "B", "B.la", "B.ca", "B.LSC", "B.v", 
                 "nactives", "nterms", "PR", "NC_PR","Switch_amort")


penSim_results %>% filter(sim == -1) %>% select(one_of(var_display))
#penSim_results %>% filter(sim == -1) %>% data.frame






#*********************************************************************************************************
# Detecitve work: term rates ####
#*********************************************************************************************************
# The AL of actives becomes even higher when higher term rates are used. 

# detective.t13 <- penSim_results
# save(detective.t13, file= "detective.t13.RData")
# 
# load("detective.t13.RData")
# detective.t13 %>% filter(sim == -1) %>% select(Tier,year, FR, MA, AL, AL.act,AL.act.laca, AL.act.v,AL.act.LSC, AL.la, AL.ca, AL.term, AL, PVFB.laca, PVFB.LSC, PVFB.v, PVFB, 
#                         B, B.la, B.ca, B.LSC,B.v, nactives, nterms, PR, NC_PR) %>% data.frame











