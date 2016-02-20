# Master file of the UCRP model

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
source("Functions.R")


load("Data/UCRP.inputs1.RData")
load("Data/UCRP.inputs2.RData")
load("Data/2015-10-07/retirees.rda") 




#*********************************************************************************************************
# 0. Parameters   ####
#*********************************************************************************************************

runname <- "UCRP"

init.year <- 2015
nyear <- 30
max.age <-  120

range_ea <- c(20:74)
min.ea  <- min(range_ea)
max.ea <- max(range_ea) 

range_age <- 20:120
min.age  <- min(range_age)
max.age  <- max(range_age) 

range_age.r <- 50:75
r.min  <- min(range_age.r)
r.max  <- max(range_age.r) 

fasyears <- 3
cola     <- 0.03
i <- 0.075
v <- 1/(1 + i)
infl <- 0.03


r.full <- 60 # age at which vested terms are assumed to retire. 
r.yos  <- 5
v.yos  <- 5 


actuarial_method <- "EAN.CP"


wf_growth <- 0
no_entrance <- "F"
entrants_dist <- rep(1/length(range_ea), length(range_ea))

pct.F.actives <- 0.55
pct.M.actives <- 1 - pct.F.actives

pct.F.LSC <- 0.6
pct.M.LSC <- 1 - pct.F.LSC

pct.fac.actives.t76 <- 0.5
pct.stf.actives.t76 <- 1 - pct.fac.actives.t76 
pct.fac.actives.t13 <- 0.5
pct.stf.actives.t13 <- 1 - pct.fac.actives.t13 
pct.fac.actives.tm13 <- 0.5
pct.stf.actives.tm13 <- 1 - pct.fac.actives.t13  


pct.ca <- 0.8 * pct.F.actives + 0.6 * pct.M.actives # For those opting for annuit rather than LSC, the % of choosing contingent annuity (0% for 2013 and modified 2013 tier)
pct.la <- 1 - pct.ca                                # For those opting for annuit rather than LSC, the % of choosing life annuity (100% for 2013 and modified 2013 tier)

factor.ca <- 0.25




#*********************************************************************************************************
# 1.1 Importing Decrement tables and Calculating Probabilities ####
#*********************************************************************************************************
source("UCRP_Decrements.R")

# Chnange variable names for 1976 tier
decrement.ucrp %<>% rename(pxT = pxT.t76,
                           qxr.la   = qxr.la.t76,
                           qxr.ca   = qxr.ca.t76,
                           qxr.LSC  = qxr.LSC.t76,
                           qxr      = qxr.t76)
bfactor %<>% rename(bfactor = bf.non13)




#*********************************************************************************************************
# 1.2 Import Salary table and initial retirement benefit table ####
#*********************************************************************************************************
source("UCRP_Test_Import_Plan.R")



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
#source("Model_Sim_dev.R")






