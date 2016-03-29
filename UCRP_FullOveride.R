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

# Simulation parameters
nsim    <- 1000 
nyear   <- 30      #nrow(df_SegalOpen_raw) 
ncore   <- 6 

# contribution Policy:
ConPolicy  <- "ADC"
PR_pct_cap <- 0.14

nonNegC   <- TRUE
EEC_fixed <- FALSE


# Return assumptions and discount rate
i = 0.0725
ir.mean <- i + 0.12^2/2
ir.sd   <- 0.12


# Amortization parameters
m = 20 # used in Segal projection
m.UAAL0 = 20
m.UAAL1 = 20
m.surplus0 = 30
m.surplus1 = 15
m.max <- max(m, m.UAAL0, m.UAAL1, m.surplus0, m.surplus1)

salgrowth_amort = 0
amort_method    = "cd"




# Modification factors
 # reduction factor for initial amort payment
f.initAmort <- 0.95




#####   Initial amortization  ####
#********************************************************************************

fileName_planInfo  <- "Data/PlanInfo-UCRP.xlsx" 

# Loading Inital amortization payments
init_amort_raw <- read_excel(fileName_planInfo, sheet = "Init_amort", skip = 0)%>% filter(!is.na(Type)) 
names(init_amort_raw) <-  c("Type", "year.est", "m.init", "amount.init", "amount.annual", "year.remaining", "Balance")
init_amort_raw %<>% mutate(year.est = year(year.est)) %>% filter(Type != "Total") %>% 
                    mutate_each(funs(.*1000), -Type, -year.est, -m.init, -year.remaining)


# matrix representation of amortization: better visualization but larger size
SC_amort0 <- matrix(0, nyear + m.max, nyear + m.max)


# Amortization payment amounts for all prior years. 
SC_amort.init <- matrix(0, nrow(init_amort_raw), nyear + m.max)


for(j in 1:nrow(SC_amort.init)){ 
  SC_amort.init[j, 1:init_amort_raw$year.remaining[j]] <- init_amort_raw$amount.annual[j] * f.initAmort  
}

nrow.initAmort <- nrow(SC_amort.init)

SC_amort0 <- rbind(SC_amort.init, SC_amort0)
# The amortization basis of year j should be placed in row nrow.initAmort + j - 1, for j >= 2. 


# Intial SC in a data frame, to be merged into main data frame. 
df_SC_amort.init <- data.frame(year = (1:ncol(SC_amort.init))+2014, SC_init = colSums(SC_amort.init)) 
  


#### Loading data Segal open plan projection 
#********************************************************************************
fileName_SegalProj <- "Data/UCRP_FullOverride_SegalProj2015.xlsx"

df_SegalOpen_raw <- read_excel(fileName_SegalProj, sheet = "Data", skip = 1) %>% 
                    filter(!is.na(year)) %>% 
                    mutate(year = year(year)) %>%  
                    left_join(df_SC_amort.init) %>% 
                    mutate_each(funs(.*1000), -year, -NC_PR.Segal, - FR_AA.Segal, -SC_init) %>% 
                    mutate(NC.Segal = PR.Segal * NC_PR.Segal,
                           UAAL.MA.Segal = AL.Segal - MA.Segal,
                           i.r.Segal = 0.0725,
                           ADC.Segal = NC.Segal + SC_init + STIP.Segal) 
                    

penSim0 <- df_SegalOpen_raw %>% 
           select(MA.Segal = MA.Segal,
                  C.Segal,
                  ERC.Segal,
                  
                  AL = AL.Segal,
                  NC = NC.Segal,
                  SC_init = SC_init,
                  EEC = EEC.Segal,
                  UAAL.AA = UAAL.AA.Segal,
                  B = B.Segal,
                  PR = PR.Segal,
                  extFund = STIP.Segal) %>% 
           mutate(i = i,
                  MA = 0,
                  UAAL.MA  = 0,
                  EUAAL.MA = 0,
                  Amort.basis = 0,
                  LG = 0,
                  ERC = 0,
                  C = 0,
                  I.r = 0) %>% 
           as.list


#### Investment returns ####
#********************************************************************************
# i.r <- rep(0.0725, nyear)
set.seed(1234)
i.r <- matrix(rnorm(nyear*nsim, ir.mean, ir.sd), nyear, nsim)
i.r <- cbind(rep(0.0725, nyear), i.r)
colnames(i.r) <- 0:nsim


# i.r <- rnorm(nyear, ir.mean, ir.sd)


#####  Simulation ####
#********************************************************************************


cl <- makeCluster(ncore) 
registerDoParallel(cl)

#penSim_results <- list()
#for(k in 1:nsim){

penSim_results <- foreach(k = 0:nsim, .packages = c("dplyr", "tidyr", "magrittr")) %dopar% {

penSim <- penSim0
SC_amort <- SC_amort0
penSim[["i.r"]] <- i.r[, as.character(k)]

source("Functions.R")

for (j in 1:nyear){
  #j = 1
  
  ## MA
  if(j == 1) penSim$MA[j] <- penSim$MA.Segal[j] else
             penSim$MA[j] <- with(penSim, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
  
  ## UAAL
   # penSim$UAAL.AA[j] <- with(penSim, AL[j] - AA[j])
  penSim$UAAL.MA[j] <- with(penSim, AL[j] - MA[j])
  
  ## LG
   # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
  if (j == 1){
    penSim$EUAAL.MA[j] <- 0
    penSim$LG[j] <- with(penSim,  UAAL.MA[j])        # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
    penSim$Amort_basis[j] <- with(penSim, LG[j])  # This will not be used for UCRP since the amortization scheme for year 1 is provided by SC_amort.(from AV2015)
    
  } else {
    penSim$EUAAL.MA[j]     <- with(penSim, (UAAL.MA[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
    penSim$LG[j]           <- with(penSim,  UAAL.MA[j]- EUAAL.MA[j])
    penSim$Amort_basis[j]  <- with(penSim,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
  }
  

  ## SC
  if(j > 1){  #(j > ifelse(useAVamort, 1, 0))
  SC_amort[nrow.initAmort + j - 1, j:(j + m - 1)] <- amort_LG(penSim$Amort_basis[j], i, 20, salgrowth_amort, end = FALSE, method = amort_method)}
  
  penSim$SC[j] <- sum(SC_amort[, j])
  
  
  ## ADC
  # penSim$ADC[j] <- with(penSim, max(0, NC[j] + SC[j]))
  # penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) # Currently fixed EEC in any cases, even ADC < EEC.
  # 
  if(nonNegC){
    penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
    penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
    
    # Adjustment of EEC
    if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
    
  } else {
    # Allow for negative ADC and C  
    penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
    
    if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
    # EEC is not fixed
    # 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
    } else if(with(penSim, ADC[j] > EEC[j])) {
      penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
      # 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
    } else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
      penSim$ADC.ER[j] <- 0
      penSim$EEC[j]    <- with(penSim, ADC[j])
      # 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
    } else if(with(penSim, ADC[j] <= 0)) {
      penSim$ADC.ER[j] <- with(penSim, ADC[j])
      penSim$EEC[j]    <- 0
    }
    
  }
  

  ## ERC
  penSim$ERC[j] <- switch(ConPolicy,
                          ADC     = with(penSim, ADC.ER[j]),                          # Full ADC
                          ADC_cap = with(penSim, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                          Fixed   = with(penSim, PR_pct_fixed * PR[j])                # Fixed percent of payroll
  ) 
  
  
  ## C
  #penSim$C[j] = penSim$C.Segal[j] 
  penSim$C[j] = with(penSim, EEC[j] + ERC[j] + extFund[j])
  
  
  # C(j) - ADC(j)
  penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
  
  
  ## Ia(j), Ib(j), Ic(j): expected interest for cash flow components
  penSim$Ia[j] <- with(penSim,  MA[j] * i[j])
  penSim$Ib[j] <- with(penSim,  B[j]  * i[j])
  penSim$Ic[j] <- with(penSim,  C[j]  * i[j])
  
  ## I.r 
   # It is assumed that all contributions and benefit payments are made at the beginning of year.  
  penSim$I.r[j] = with(penSim, i.r[j] * (MA[j] + C[j] - B[j]))
}

penSim %<>% as.data.frame 
}

stopCluster(cl)


penSim_results <- bind_rows(penSim_results) %>%
                  mutate(year = rep(2015:2044, nsim+1),
                         sim  = rep(0:nsim, each = nyear),
                         d_MA = 100*(MA/MA.Segal - 1),
                         d_C   = 100*(C/C.Segal - 1),
                         d_ERC = 100*(ERC/(ERC.Segal- extFund) - 1),
                         
                         ERC_PR = 100 * (ERC)/PR,
                         ERC_PR.Segal = 100 * (ERC.Segal - extFund)/PR,
                         
                         FR.MA       = 100 * MA/AL,
                         FR.MA.Segal = 100 * MA.Segal/AL,
                         d_FR.MA     = FR.MA - FR.MA.Segal) %>% 
           select(sim, year, AL, MA.Segal, MA, d_MA, FR.MA.Segal, FR.MA, d_FR.MA, C.Segal, C, d_C, ERC.Segal, ERC, d_ERC, EEC, ERC_PR.Segal, ERC_PR) #, LG, C_ADC) 

penSim_results %>% filter(sim == 6) %>% kable(digits = 3)


results_ADC <- penSim_results

penSim_results %>% group_by(year) %>% summarize(FR.MA = median(FR.MA))



#SC_amort


## Matching Segal Projection under constant return of 7.25%.
 # Potential issues:
 # 1. Model ERC is higher than Segal ERC in the last couple of years, which implies there are differences in amort scheme between the model calculation and Segal projection. 
 # 2. Loss/gain are not zero, even when investment return is set equal to discount rate 7.25%. Imply difference in the calculation of UAAL and/or EUAAL?  


# quantiles










