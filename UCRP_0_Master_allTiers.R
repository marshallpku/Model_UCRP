
gc()

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************

# Plan information
# source("UCRP_Data_RP2014.R")   # not dependent on parameter lists
# source("UCRP_Data_PlanInfo.R") # not dependent on parameter lists
load("./Data/UCRP.PlanInfo.RData")  # for all tiers


# Initial population
source("UCRP_Data_Population.R")    # for all tiers; uses init.year, Grouping, pct.ca.F, pct.ca.M in paramlist

# Decrement tables
source("UCRP_Data_Decrements.R")   # for all tiers; range_age, range_ea age_range.r, pct.M.LSC, max.age, cola, i, r.full, r.max, r.min, r.yos 



#*****************************************************
##   Calibration and Modification of initial data ####
#*****************************************************

## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terminated_all %<>% mutate(nterm = 0)


## Exclude initial terms with ea < 20: Data_population, line 504
 # init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
 #                                 ea >= Global_paramlist$min.ea)


## Exclude the initial amortization basis when testing the program.
 if(!paramlist$useAVamort)  init_amort_raw %<>% mutate(amount.annual = 0) # CAUTION: For consistency check only; will make initial UAAL not amortized. 



## Matching Segal cash flow

# Matching Segal payroll 
  # Payroll from model:
    # Total: 11040551k
    # t76:   9094102199
    # t13:   1279359295 
    # tm13:   667089761   
  # Goal: 9659652k (from Segal projection and AV 2015) 
  
  # Method: Applying adjustment factors to initial population and initial salary.
    # Adjustment factor for initial workforce, applied to all 3 tiers (NEXT STEP? apply only to t76 tier.)
      # Payroll of none-lab seg / Payroll of all segs(unit： $k): 
        f1 <- 9659652 / 9927833 # 0.972987  
    # Adjustment factor for initial salary
      # payroll from vii table / payroll from model (both for all segs, unit $k):
        f2 <- 9927833 / 11040551 # 0.8992154
    # Total adjustment factor is 
      # f1 * f2 = 0.8749248

  # Adjusting initial workforce and salary:
    init_actives_all %<>% mutate(nactives = nactives * f1,
                                 salary   = salary   * f2) 



#*********************************************************************************************************
# 1.2  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("UCRP_Model_InvReturns.R")
i.r <- gen_returns()
    
i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Importing Decrement tables and Calculating Probabilities ####
#*********************************************************************************************************
source("UCRP_Data_PlanData_Transform.R")

# Create data for each tier

salary.t76  <- get_salary_proc("t76", paramlist$w.salgrowth.method)
salary.t13  <- get_salary_proc("t13", paramlist$w.salgrowth.method)
salary.tm13 <- get_salary_proc("tm13",paramlist$w.salgrowth.method)

benefit.t76  <- get_benefit_tier("t76")
benefit.t13  <- get_benefit_tier("t13")
benefit.tm13  <- get_benefit_tier("tm13")

init_pop.t76 <- get_initPop_tier("t76")
init_pop.t13 <- get_initPop_tier("t13")
init_pop.tm13 <- get_initPop_tier("tm13")

entrants_dist.t76  <- get_entrantsDist_tier("t76")
entrants_dist.t13  <- get_entrantsDist_tier("t13")
entrants_dist.tm13 <- get_entrantsDist_tier("tm13")


# Chnange variable names
make_tierDec <- function(Tier_select_, df = decrement.ucrp){
  df %<>% rename_("pxT" = paste0("pxT.", Tier_select_),
                  "qxr.la"   = paste0("qxr.la.", Tier_select_),
                  "qxr.ca"   = paste0("qxr.ca.", Tier_select_),
                  "qxr.LSC"  = paste0("qxr.LSC.", Tier_select_),
                  "qxr"      = paste0("qxr.", Tier_select_),
                  "qxt"      = paste0("qxt.", Tier_select_))
  }
decrement.ucrp.t76  <- make_tierDec("t76")
decrement.ucrp.t13  <- make_tierDec("t13")
decrement.ucrp.tm13 <- make_tierDec("tm13")


make_tierBfactor <- function(Tier_select_, df = bfactor){
  df %<>% mutate(Tier = Tier_select_,
                 bfactor = ifelse(Tier == "t13", bf.13, bf.non13)) %>% 
    select(age, bfactor)
}
bfactor.t76 <- make_tierBfactor("t76")
bfactor.t13 <- make_tierBfactor("t13")
bfactor.tm13 <- make_tierBfactor("tm13")



#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("UCRP_Model_Demographics_allTiers.R")
pop <- get_Population_allTiers()
gc()



#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Model_IndivLiab.R")
gc()

# liab <- get_indivLab(decrement.ucrp,
#                      salary,
#                      benefit,
#                      bfactor,
#                      get(paste0("init_terminated.",paramlist$Tier_select)))

liab.t76 <- get_indivLab(decrement.ucrp.t76,
                         salary.t76,
                         benefit.t76,
                         bfactor.t76,
                         get(paste0("init_terminated.","t76")),
                        .Tier_select = "t76")

liab.t13 <- get_indivLab(decrement.ucrp.t13,
                         salary.t13,
                         benefit.t13,
                         bfactor.t13,
                         get(paste0("init_terminated.","t13")),
                         .Tier_select = "t13")

liab.tm13 <- get_indivLab(decrement.ucrp.tm13,
                          salary.tm13,
                          benefit.tm13,
                          bfactor.tm13,
                          get(paste0("init_terminated.","tm13")),
                          .Tier_select = "tm13")


#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("UCRP_Model_ContingentAnnuity.R")
liab.ca.t76  <- get_contingentAnnuity(decrement.ucrp.t76)
liab.ca.t13  <- get_contingentAnnuity(decrement.ucrp.t13)
liab.ca.tm13 <- get_contingentAnnuity(decrement.ucrp.tm13)


#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Model_AggLiab.R")
gc()

AggLiab.t76 <- get_AggLiab(get(paste0("init_beneficiaries.", "t76")),
                           liab.t76,
                           liab.ca.t76,
                           pop$pop.t76) 

AggLiab.t13 <- get_AggLiab(get(paste0("init_beneficiaries.", "t13")),
                           liab.t13,
                           liab.ca.t13,
                           pop$pop.t13) 

AggLiab.tm13 <- get_AggLiab(get(paste0("init_beneficiaries.", "tm13")),
                           liab.tm13,
                           liab.ca.tm13,
                           pop$pop.tm13) 


AggLiab.sumTiers <- get_AggLiab_sumTiers(AggLiab.t76, AggLiab.t13, AggLiab.tm13)



#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("UCRP_Model_Sim.R")

if(paramlist$simTiers == "separate"){
  penSim_results.t76  <- run_sim("t76",  AggLiab.t76)
  penSim_results.t13  <- run_sim("t13",  AggLiab.t13)
  penSim_results.tm13 <- run_sim("tm13", AggLiab.tm13)
}
 
penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers)


#*********************************************************************************************************
# 7.1  Showing results: Joint simulation of all tiers ####
#*********************************************************************************************************
# Use full outputs include:
  # penSim_results.sumTiers
  # AggLiab.t76; AggLiab.t13; AggLiab.tm13
# NEXT STEP: extract useful variables from AggLiab.XXX files, so we can still see liability dynamics of each tier
#            when we simulate(do the loop) all tiers jointly. 


var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", 
                 #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
                 #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                 "B", "B.la", "B.ca", "B.LSC", "B.v", 
                 "nactives", "nterms", "PR", "NC_PR")


kable(penSim_results.sumTiers %>% filter(sim == -1) %>% select(one_of(var_display)), digits = 2) 






#*********************************************************************************************************
# 7.1  Showing results: Separate simulations of each tier ####
#*********************************************************************************************************
# Currently for the purpose of checking model consistency. 
# To make sense of separate simulation of each tier, we must allocate initial assets and amortization payments 
# among the tiers. However, we currently lack information for doing this reasonably. 




# var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", 
#                  #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
#                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
#                  "B", "B.la", "B.ca", "B.LSC", "B.v", 
#                  "nactives", "nterms", "PR", "NC_PR")
# 
# penSim_results_byTiers <- bind_rows(penSim_results.t76,
#                                     penSim_results.t13,
#                                     penSim_results.tm13)
# 
# penSim_results_sumTiers <- penSim_results_byTiers %>% 
#   group_by(sim, year) %>% 
#   summarise(MA = sum(MA)/1000,
#             AL = sum(AL)/1000,
#             NC = sum(NC)/1000,
#             PVFB = sum(PVFB)/1000,
#             B = sum(B)/1000,
#             PR = sum(PR)/1000,
#             nactives = sum(nactives)) %>% 
#   mutate(NC_PR = NC/PR * 100,
#          FR = MA/AL * 100)
# 
# 
# penSim_results.t76  %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# penSim_results.t13  %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# penSim_results.tm13 %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# 
# penSim_results_sumTiers %>% filter(sim == -1) 



#write.xlsx2(penSim_results_sumTiers %>% filter(sim == -1), file = "Data/detective_constant_wf.xlsx", sheet = "Total")














