
gc()
#*********************************************************************************************************
# 0. Parameters   ####
#*********************************************************************************************************

Global_paramlist <- list(
  
  init.year = 2015,
  nyear     = 40,
  nsim      = 5,
  ncore     = 4,
  
  min.ea    = 20,
  max.ea    = 74, 
  
  min.age   = 20,
  max.age   = 120 
)


paramlist <- list(
  
  runname = "UCRP",
  #Tier_select = Tier_select_RunControl,
  simTiers = simTiers,
  
  
  Grouping    = "fillin",
  
  r.min  = 50,
  r.max  = 75, 

  fasyears = 3,
  cola     = 0.02,
  i = 0.0725,
  
  infl = 0.03,
  prod = 0.01,
  s.year = 10,
  
  m.UAAL0 = 20,
  m.UAAL1 = 20,
  m.surplus0 = 30,
  m.surplus1 = 15,
  
  r.full = 60, # age at which vested terms are assumed to retire. 
  r.yos  = 5,
  v.yos  = 5, 
  
  startingSal_growth = 0.038,
  w.salgrowth.method =  "simple", # "simple" or "withInit"
  
  actuarial_method = "EAN.CP",
  
  
  wf_growth = 0,
  no_entrance = "F",
  newEnt_byTier = c(t76 = 0, t13 = 0.65, tm13 = 0.35),
  #entrants_dist = rep(1/length(range_ea), length(range_ea)),
  
  pct.F.LSC = 0.6, # assumed proporotion of females, for the calculation of LSC amount 
  
  
  pct.ca.F =  0.8, # proportion of females who opt for ca upon retirement
  pct.ca.M =  0.6,
  
  factor.ca = 0.25,
  
  # Investment returns
  seed = 1234,
  ir.mean = 0.0725,
  ir.sd   = 0.12,
  
  
  init_MA = "AL_pct",
  MA_0_pct = 0.8069,
  init_EAA = "MA",
  
  
  smooth_method = "method1",
  salgrowth_amort = 0,
  amort_method = "cd",
  amort_type = "closed",
  nonNegC = "FALSE",
  EEC_fixed = "TRUE",
  ConPolicy = "ADC",
  EEC_rate = 0.05
)

# Parameters derived from the parameter list above. 
paramlist$range_ea = with(Global_paramlist, min.ea:max.ea)
paramlist$range_age = with(Global_paramlist, min.age:max.age)
paramlist$range_age.r = with(paramlist, r.min:r.max)
paramlist$m.max = with(paramlist, max(m.UAAL0, m.UAAL1, m.surplus0, m.surplus1))
paramlist$v     = with(paramlist, 1/(1 + i))
paramlist$pct.M.LSC = with(paramlist, 1 - pct.F.LSC)


# # Assign parameters to the global environment
# assign_parmsList(Global_paramlist, envir = environment())
# assign_parmsList(paramlist,        envir = environment())  


devMode <- FALSE


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
source("UCRP_Model_Decrements.R")   # for all tiers; range_age, range_ea age_range.r, pct.M.LSC, max.age, cola, i, r.full, r.max, r.min, r.yos 



#*****************************************************
##   Calibration and Modification of initial data ####
#*****************************************************

## Exclude selected type(s) of initial members
# init_actives_all %<>% mutate(nactives = 0) 
# init_retirees_all %<>% mutate(nretirees = 0)
# init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
# init_terminated_all %<>% mutate(nterm = 0)


init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
                                ea >= Global_paramlist$min.ea)

## Exclude the initial amortization basis when testing the program.
init_amort_raw %<>% mutate(amount.annual = 0) # CAUTION: For consistency check only; will make initial UAAL not amortized. 


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
source("UCRP_Test_InvReturns.R")
i.r <- gen_returns()
    
i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Importing Decrement tables and Calculating Probabilities ####
#*********************************************************************************************************
source("UCRP_Test_PlanData_Transform.R")

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
source("UCRP_Test_Demographics_allTiers.R")
pop <- get_Population_allTiers()
gc()



#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Test_IndivLiab.R")
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
source("UCRP_Test_ContingentAnnuity.R")
liab.ca.t76  <- get_contingentAnnuity(decrement.ucrp.t76)
liab.ca.t13  <- get_contingentAnnuity(decrement.ucrp.t13)
liab.ca.tm13 <- get_contingentAnnuity(decrement.ucrp.tm13)


#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Test_AggLiab.R")
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
source("UCRP_Test_Sim.R")

if(paramlist$simTiers == "separate"){
  penSim_results.t76  <- run_sim("t76",  AggLiab.t76)
  penSim_results.t13  <- run_sim("t13",  AggLiab.t13)
  penSim_results.tm13 <- run_sim("tm13", AggLiab.tm13)
}
 
penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers)



#*********************************************************************************************************
# 7.1  Showing results: Joint simulation of all tiers ####
#*********************************************************************************************************
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
# 
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





# #*********************************************************************************************************
# # 8. Comparing model results with Segal open plan projections ####
# #*********************************************************************************************************
# 
# # load Segal open plan projections
# fileName <- "Data/SegalProj_Data_2015.xlsx"
# 
# results_sumTiers_SegalOpen <- read_excel(fileName, sheet = "Data", skip = 1) %>% filter(!is.na(year)) %>% 
#                 mutate(year = year(year)) %>% 
#                 right_join(penSim_results_sumTiers) %>% 
#                 mutate(NC_PR.segal = 100 * NC_PR.segal,
#                        d_AL = 100 * (AL/AL.segal - 1),
#                        d_B  = 100 * (B/B.segal - 1),
#                        d_NC = 100 * (NC/NC.segal - 1),
#                        d_PR = 100 * (PR/PR.segal - 1),
#                        d_NC_PR = 100 * (NC_PR/NC_PR.segal - 1)) %>% 
#                 select(year, 
#                        AL.segal, AL, d_AL, 
#                        B.segal, B, d_B,
#                        NC.segal, NC, d_NC, 
#                        NC_PR.segal, NC_PR, d_NC_PR, 
#                        PR.segal, PR, d_PR, 
#                        everything())
# 
# kable(results_sumTiers_SegalOpen %>% filter(sim == -1) %>% select(year:d_PR, FR), digits = 2)
# 
# df_SegalOpen_long <- df_SegalOpen %>% gather(variable, value, -year)
# 
# 
# save(results_sumTiers_SegalOpen, penSim_results.t76, penSim_results.t13, penSim_results.tm13, 
#      file = "Data/Results_SegalOpen.RData")
# write.xlsx2(results_sumTiers_SegalOpen %>% filter(sim == -1), file = "Data/Results_SegalOpen.xlsx")
# 
# 
# 
# 
# 
# ## Comparing AL
# 
# plot_comp <- function(v.segal, v, d_v, ylim1, ylim2 = c(-20, 20), df = df_SegalOpen_long){
#   
#  g1 <-  df_SegalOpen_long %>% filter(variable %in% c(v.segal, v)) %>% 
#     ggplot(aes(x = year, y = value, color = variable)) + geom_point() + geom_line() + 
#     coord_cartesian(ylim = ylim1 )
#   
#  g2 <-   df_SegalOpen_long %>% filter(variable %in% c(d_v)) %>% 
#     ggplot(aes(x = year, y = value)) + geom_point() + geom_line()+ 
#     scale_y_continuous(breaks = seq(-100, 100, 5)) + 
#     coord_cartesian(ylim = ylim2 )
# 
#  return(list(g1 = g1, g2 = g2))
#  
# }
# 
# plot_comp("AL.segal", "AL", "d_AL", c(0, 2e8))
# plot_comp("B.segal",  "B",  "d_B", c(0, 1.2e7))
# plot_comp("PR.segal", "PR",  "d_PR", c(0, 3e7))
# plot_comp("NC.segal", "NC",  "d_NC", c(0, 5e6))
# 
# 
# 
# #*********************************************************************************************************
# # 9. Comparing model results with Segal closed plan projections ####
# #*********************************************************************************************************
# 
# fileName <- "Data/SegalProj_PaymentProjection_2015_ClosedGroup.xlsx"
# results_sumTiers_ActivesOnly_SegalClosed <- read_excel(fileName, sheet = "Data", skip = 2) %>% filter(!is.na(year)) %>%
#                   mutate_each(funs(./1000), -year) %>% 
#                   right_join(penSim_results_sumTiers) %>% 
#                   mutate(d_B = 100 * (B  / B.actives.segal - 1)) %>% 
#                   select(year, B.actives.segal, B, d_B, everything())
# 
# 
# kable(results_sumTiers_ActivesOnly_SegalClosed[,1:10] %>% filter(sim == -1), digits = 2)
# 
# save(results_sumTiers_ActivesOnly_SegalClosed, penSim_results.t76, penSim_results.t13, penSim_results.tm13,
#      file = "Data/Results_ActivesOnly_SegalClosed.RData")
# 
# 
# write.xlsx2(results_sumTiers_ActivesOnly_SegalClosed %>% filter(sim == -1), file = "Data/Results_ActivesOnly_SegalClosed.xlsx")
# 
# 
# 
# 




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











