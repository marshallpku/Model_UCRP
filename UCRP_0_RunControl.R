# Run control file of the UCRP model

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
library(xlsx)
library("btools")

source("Functions.R")



#*********************************************************************************************************
# Run a single tier ####
#*********************************************************************************************************
Tier_select_RunControl <- "t76"
source("UCRP_0_Master.R")

#*********************************************************************************************************
# Run all tiers ####
#*********************************************************************************************************


Tier_select_RunControl <- "t76"
source("UCRP_0_Master.R")
penSim_results_t76 <- penSim_results


Tier_select_RunControl <- "t13"
source("UCRP_0_Master.R")
penSim_results_t13 <- penSim_results


Tier_select_RunControl <- "tm13"
source("UCRP_0_Master.R")
penSim_results_tm13 <- penSim_results



df_results_total <- bind_rows(penSim_results_t76,
                                penSim_results_t13,
                                penSim_results_tm13)

save(df_results_total, file = "PenSim_detective_total.RData")









