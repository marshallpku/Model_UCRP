# Master file of the UCRP model

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library("btools")

source("Functions.R")




#*********************************************************************************************************
# 0. Parameters   ####
#*********************************************************************************************************

Global_paramlist <- list(
  
  init.year = 2015,
  nyear     = 30,
  nsim      = 5,
  ncore     = 4,

  min.ea    = 20,
  max.ea    = 74, 

  min.age   = 20,
  max.age   = 120 
)




paramlist <- list(

  runname = "UCRP",
  Tier_select = "t76", 
  
  r.min  = 50,
  r.max  = 75, 

  
  
  fasyears = 3,
  cola     = 0.03,
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
  
  startingSal_growth = 0.04,
  
  actuarial_method = "EAN.CP",
  
  
  wf_growth = 0,
  no_entrance = "F",
  #entrants_dist = rep(1/length(range_ea), length(range_ea)),
  
  pct.F.LSC = 0.6,
 
  
  pct.ca.F =  0.8,
  pct.ca.M =  0.6,
  
  factor.ca = 0.25,
  
  # Investment returns
  seed = 1234,
  ir.mean = 0.0725,
  ir.sd   = 0.12,
  
  
  init_MA = "AL",
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


paramlist$range_ea = with(Global_paramlist, min.ea:max.ea)
paramlist$range_age = with(Global_paramlist, min.age:max.age)
paramlist$range_age.r = with(paramlist, r.min:r.max)
paramlist$m.max = with(paramlist, max(m.UAAL0, m.UAAL1, m.surplus0, m.surplus1))
paramlist$v     = with(paramlist, 1/(1 + i))
paramlist$pct.M.LSC = with(paramlist, 1 - pct.F.LSC)

assign_parmsList(Global_paramlist, envir = environment())
assign_parmsList(paramlist,        envir = environment())  


devMode <- FALSE


# Testing

# pct.ca.F  <- 1 # 0.8
# pct.ca.M  <- 1 # 0.6
# pct.ca <- pct.ca.F * pct.F.actives + pct.ca.M * pct.M.actives # For those opting for annuit rather than LSC, the % of choosing contingent annuity (0% for 2013 and modified 2013 tier)
# pct.la <- 1 - pct.ca


#*********************************************************************************************************
# 1.1 Import Salary table and initial retirement benefit table ####
#*********************************************************************************************************
#source("UCRP_Data_RP2014.R")
#source("UCRP_Data_PlanInfo.R")
load("./Data/UCRP.PlanInfo.RData")



source("UCRP_Data_Population.R")
# source("UCRP_Test_PlanData_Import.R")
source("UCRP_Test_PlanData_Transform.R")


# Modification to plan data for testing purpose.  

# init_pop$actives[,] <- 0
# init_pop$actives[1,"40"] <- 1

#LSCrates  %<>% mutate(qxLSC.act = 0)

# retirees  %<>% mutate(benefit = 0)
# benefit   %<>% mutate(benefit = 0)
# 
# init_beneficiaries %<>% mutate(benefit = 0)

#terminated %<>% mutate(nterm = 0) 
#termrates %<>% mutate(qxt_faculty = 0)


# Exclude the initial amortization basis when testing the program.
init_amort_raw %<>% mutate(amount.annual = 0) 



#*********************************************************************************************************
# 1.2 Importing Decrement tables and Calculating Probabilities ####
#*********************************************************************************************************
source("UCRP_Model_Decrements.R")

# Chnange variable names for 1976 tier
decrement.ucrp %<>% rename(pxT = pxT.t76,
                           qxr.la   = qxr.la.t76,
                           qxr.ca   = qxr.ca.t76,
                           qxr.LSC  = qxr.LSC.t76,
                           qxr      = qxr.t76)
bfactor %<>% rename(bfactor = bf.non13)




#*********************************************************************************************************
# 1.3  Actual investment return. ####
#*********************************************************************************************************
source("UCRP_Test_InvReturns.R")
i.r[, 3] <-  c(ir.mean, ir.mean/2, rep(ir.mean, nyear - 2))


#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("UCRP_Test_Demographics.R")
gc()



#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Test_IndivLiab.R")
gc()



#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("UCRP_Test_ContingentAnnuity.R")


#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("UCRP_Test_AggLiab.R")
gc()



#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("UCRP_Test_Sim.R")



penSim_results %>% filter(sim == -1) %>% select(year, FR, MA, AL, AL.act, AL.act.v,AL.act.LSC, AL.la, AL.ca, AL.term, AL, NC_PR, 
                                                B, B.la, B.ca, B.LSC,B.v, nactives, PR) %>% data.frame
#penSim_results %>% filter(sim == -1) %>% data.frame




# OK when only with life annuity, and no initial retirees
# OK when only with life annuity, and with initial retirees


# FR first slightly decreases then increases after around year 30, when contingent annuity added. 



# .liab$active %>% filter(year == 2015, ea == 20, age == 49) %>% select(year, ea, age, ALx.LSC, Bx.LSC, NCx.LSC)
# .liab$active %>% filter(year == 2015, ea == 20, age == 50) %>% select(year, ea, age, ALx.LSC, Bx.LSC)
# .liab$B.LSC  %>% filter(year == 2016, ea == 20, age == 50)
# 
# salary %>% filter(year == 2015)






