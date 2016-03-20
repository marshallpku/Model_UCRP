# Detective work to check the difference between model results and actual values. 


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
library("btools")

source("Functions.R")


select_vars <- c("Tier", "sim","year", "AL", "MA", "FR", "B", "PVFB", "NC", "NC_PR", "PR", 
                 "AL.act", "AL.act.v", "AL.act.LSC", "AL.act.laca", "AL.la", "AL.ca", "AL.term", "AL.LSC",
                 "NC.laca", "NC.LSC", "NC.v",
                 "PVFB.laca", "PVFB.LSC", "PVFB.v",
                 "B.la", "B.ca", "B.v", "B.LSC",
                 "nactives", "nla", "n.ca.R1", "n.ca.R0S1", "n.LSC")

get_detective <- function(type, SAVE = F, vars){
  load(paste0("PenSim_detective_", type, ".RData"))
  results_byTier <- get(paste0("df_results_",type)) %>% 
                    select(one_of(select_vars))
  
  results_sumTiers <- results_byTier %>% 
    group_by(sim, year) %>% 
    summarise(MA = sum(MA),
              AL = sum(AL),
              NC = sum(NC),
              PVFB = sum(PVFB),
              B = sum(B),
              PR = sum(PR),
              nactives = sum(nactives)) %>% 
    mutate(NC_PR = NC/PR * 100,
           FR = MA/AL * 100)
  
    if(SAVE) save(results_byTier, results_sumTiers, file = paste0("DetecitveWork_", type, ".RData"))
  
    return(list(byTier = results_byTier, sumTiers = results_sumTiers))
  }

lresults_total   <- get_detective("total", T)
lresults_ActOnly <- get_detective("ActOnly", T)
lresults_RetOnly <- get_detective("RetOnly", T)
lresults_TermOnly <- get_detective("TermOnly", T)


lresults_RetOnly$sumTiers
lresults_RetOnly$byTier







df_results_tot %>% filter(sim == -1)
df_results %>% filter(sim == -1, Tier == "t13")




fileName = "Data/detective_TermOnly.xlsx"
write.xlsx2(df_results_tot %>% filter(sim == -1), file = fileName, sheet = "Total")
write.xlsx2(df_results %>% filter(sim == -1, Tier == "t76"), file = fileName, sheet = "t76", append = F)
write.xlsx2(df_results %>% filter(sim == -1, Tier == "t13"), file = fileName, sheet = "t13", append = T )
write.xlsx2(df_results %>% filter(sim == -1, Tier == "tm13"), file = fileName, sheet = "tm13",  append = T)



df_results %>% filter(sim == -1, Tier == "t13") %>% 
select(Tier, year,  FR, MA,B, 
       AL, AL.act, AL.act.v,AL.act.LSC, AL.act.laca, AL.la, AL.ca, AL.term, AL, 
       PVFB.laca, PVFB.LSC, PVFB.v, PVFB, 
       B.la, B.ca, B.LSC,B.v, nactives, PR, NC_PR) %>% data.frame


select_vars <- c("Tier", "sim", "AL", "MA", "FR", "B", "PVFB", "NC", "NC_PR", "PR", 
                 "AL.act", "AL.act.v", "AL.act.LSC", "AL.act.laca", "AL.la", "AL.ca", "AL.term", "AL.LSC",
                 "NC.laca", "NC.LSC", "NC.v",
                 "PVFB.laca", "PVFB.LSC", "PVFB.v",
                 "B.la", "B.ca", "B.v", "B.LSC",
                 "nla", "n.ca.R1", "n.ca.R0S1", "n.LSC")


lresults_ActOnly$byTier %>% names









