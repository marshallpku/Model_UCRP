# Run control file of the UCRP model

#*********************************************************************************************************
# Notes on NEXT STEPS ####
#*********************************************************************************************************
# 1.(DONE) Allow the initial UAAL to be treated as loss/gain in the intial year. 
# 2. Specify all the data modeification in the run control file. 
# 3. (Not necessary)Try calculating amortization basis using market value based UAAL.(code already in Model.Sim.)
# 4. Incorporate initial asset smoothing.
# 5. Add STIP

#*********************************************************************************************************
# Notes on UCRP features ####
#*********************************************************************************************************
# STIP borrowing.
  # STIP borrowing is directly added to total contribution (C). It reduces the gap between contribution and ADC.


rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
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
library(xlsx)
library("btools")

source("Functions.R")


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
  #Tier_select = "t76",
  simTiers = "joint",
  useAVamort  = F, 
  useExtFund  = F,
  
  Grouping    = "fillin",
  
  r.min  = 50,
  r.max  = 75, 
  
  fasyears = 3,
  cola     = 0.02,
  i = 0.0725,
  
  infl = 0.03,
  prod = 0.01,
  s.year = 5,
  
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
  
  
  pct.ca.M =  0.8, # proportion of males who opt for ca upon retirement
  pct.ca.F =  0.6,
  
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




#  Run all tiers ####
#*********************************************************************************************************


paramlist$simTiers <- "joint"  # "joint"(defult) or "separate"
source("UCRP_0_Master_allTiers.R")

 
 

# Run a single tier ####
#*********************************************************************************************************

# Only useful for the purposes of checking model consistency and looking at liability/benefit dynamics.
# External funding, initial amort payments are not available for single tiers. 


# When running this, initial amortization payments must be set to zero. And results are meaningful only when intial FR = 100, 
# since the inital UAAL will not be amortized.(amort basis of the first year is overriden by the values from AV2015, which is set to zero here.)
# (Already solved.)

# paramlist$Tier_select <- "t76"
# source("UCRP_0_Master_singleTier.R")
# 














# 
# Tier_select_RunControl <- "t76"
# source("UCRP_0_Master.R")
# penSim_results_t76 <- penSim_results
# 
# 
# Tier_select_RunControl <- "t13"
# source("UCRP_0_Master.R")
# penSim_results_t13 <- penSim_results
# 
# 
# Tier_select_RunControl <- "tm13"
# source("UCRP_0_Master.R")
# penSim_results_tm13 <- penSim_results
# 
# 
# 
# df_results_ActOnly <- bind_rows(penSim_results_t76,
#                                 penSim_results_t13,
#                                 penSim_results_tm13)
# # df_results_RetOnly  <- penSim_results_t76
# # df_results_TermOnly <- penSim_results_t76
# 
# save(df_results_TermOnly, file = "PenSim_detective_TermOnly.RData")
# 








