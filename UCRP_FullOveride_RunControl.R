# This script conducts the simulation using Segal open plan projection as inputs. 

# Funding policies can be experimented with full override model:
# 1. Plan sponsor pays with a cap of 14% payroll.(supposedly used in Segal open plan projection; try to match Segal values.)
# 2. Plan sponsor pays full ADC + STIP
# NC:   payroll * NC rate
# SC:   initial SC from AV 2015; new SC from model calculation
# STIP: Segal projection
# 3. Plan sponsor follows statutory/administrative rules to determine actual contributions.(eg. cap on ERC)

# Notes
# Note the initial amort payments should include all segments. In order to model non-lab segment only, 
# we reduce the initial amort payment to match the Sega ERC rate and funded ratio. Currently the factor is 95%.
# Currently loss/gain are calculated using market asset value based UAAL.  


#### Loading Packages ####
#********************************************************************************

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



#### Model Parameters ####
#********************************************************************************

# paramlist <- list(
#   # Simulation parameters
#   nsim    = 2000, 
#   nyear   = 30,      #nrow(df_SegalOpen_raw) 
#   ncore   = 6, 
#   
#   # contribution Policy:
#   ConPolicy    = "ADC_cap", # ADC, ADC_cap, Fixed
#   PR_pct_cap   = 0.14,
#   PR_pct_fixed = 0.14,
#   
#   nonNegC      = T,
#   EEC_fixed    = T,
#   ERC.lb_EEC   = F, # Use EEC as lower bound of ERC  
#   
#   
#   # Return assumptions and discount rate
#   i =        0.0725,
#   ir.mean = i + 0.12^2/2,
#   ir.sd   = 0.12,
#   
#   
#   # Amortization parameters
#   m = 20, # used in Segal projection
#   m.UAAL0 = 20,
#   m.UAAL1 = 20,
#   m.surplus0 = 30,
#   m.surplus1 = 15,
#   # m.max <- max(m, m.UAAL0, m.UAAL1, m.surplus0, m.surplus1)
#   
#   salgrowth_amort = 0,
#   amort_method    = "cd",
#   
#   # Asset smoothing parameters
#   s.year = 5,
#   
#   
#   # Modification factors
#   # reduction factor for initial amort payment
#   f.initAmort  <- 1,
#   f.initSmooth <- 1
# )


#### Model Parameters ####
#********************************************************************************
folder_run <- "FullOverride"
filename_RunControl <- dir(folder_run, pattern = "^RunControl")
path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)

# Import global parameters
runList <- read_excel(path_RunControl, sheet="params", skip = 0) %>% filter(!is.na(runname), include == 1 ) %>% 
           mutate(m.max = max(m, m.UAAL0, m.UAAL1, m.surplus0, m.surplus1))
runList



#### Run Models and Save  ####
#********************************************************************************

folder_save <- "FullOverride/Results/"

for(runName in runList$runname ){

paramlist <- get_parmsList(runList, runName)
assign_parmsList(paramlist, envir = environment())

source("UCRP_FullOveride.R")

save(penSim_results, file = paste0(folder_save, "results_", runName, ".RData"))

}

paste0(folder_save, "runName", ".RData")












