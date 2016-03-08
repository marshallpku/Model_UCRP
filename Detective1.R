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


load("PenSim_detective_total.RData")
df_results <- df_results_total

load("PenSim_detective_ActOnly.RData")
df_results <- df_results_ActOnly

load("PenSim_detective_RetOnly.RData")
df_results <- df_results_RetOnly

load("PenSim_detective_TermOnly.RData")
df_results <- df_results_TermOnly


df_results_tot <- df_results %>% 
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


df_results_tot %>% filter(sim == -1)
df_results %>% filter(sim == -1, Tier == "t76")




fileName = "Data/detective_TermOnly.xlsx"
write.xlsx2(df_results_tot %>% filter(sim == -1), file = fileName, sheet = "Total")
write.xlsx2(df_results %>% filter(sim == -1, Tier == "t76"), file = fileName, sheet = "t76", append = F)
write.xlsx2(df_results %>% filter(sim == -1, Tier == "t13"), file = fileName, sheet = "t13", append = T )
write.xlsx2(df_results %>% filter(sim == -1, Tier == "tm13"), file = fileName, sheet = "tm13",  append = T)



df_results %>% filter(sim == -1, Tier == "t76") %>% 
select(Tier, year,  FR, MA,B, 
         AL, AL.act, AL.act.v,AL.act.LSC, AL.act.laca, AL.la, AL.ca, AL.term, AL, 
         PVFB.laca, PVFB.LSC, PVFB.v, PVFB, 
        B.la, B.ca, B.LSC,B.v, nactives, PR, NC_PR)


