# This is the third script for exploring the modeling approach of beneficiaries (contingent annuitant.)
# This script is mainly used to further refine the functions and produce the first version of the final outputs that can be used with the main model. 

# Notes on contigent annuity in UCRP
# 1. Only 1976 Tier members have the option of joint-survivor annuity.(25% for members with Social Security) 
# 2. In theory, the p% joint-survivor annuity is reduced for the retiree, so that it is actuarially equivalent to the life-annuity. 
#    The reudction factor can be calculated accroding to Anderson's book. We also need to confirm with UCRP that this is the way how joint-survivor
#    annuity is treated in practice. 
# 3. 2 implies that the liability for actives can be calculated as if all retirees choose life annuity upon retirement.
# 4. The calculation of liability and benefit payment for retirees and beneficiaries needs to be simplified. It would be quite cumbersome to 
#    track all the benefit payments and liability for the survivors. Current plan is to construct a series adjusted benefit payments and 
#    mortality for retirees, with which the retirees, when modeled as life annuitant, have the same liability and expected cashflow of benefit as 
#    when modeled as joint-survivor annutant in each year. 


# Notes on the morality table used in the calculation of ALs for active memebers. 
#  In order to simplify the calculation, the ALs of active members will be calculated based on on a uni-sex mortality table. 
#  For the model to be internally consistent, the ALs for actives upon the day of retirement based on the uni-sex mortality table should be
#  identical to the ALs calculated as the weighted sum of ALs for male and female members. 

#  However, this equivalence cannot be achieved by using a simple weighted average of male and female mortality tables, because the weighted average
#  mortality will not generate same series of population of retirees as that generated by calculating male and female retirees seperately. 
#  Here we propose to use the "dynamically weighted" mortality table to calculate the ALs for actives in order to ensure the model is internally consistent.
#  In the dynamically weighted mortality table for a certain retirement age, the mortality rates of the two genders are weighted by the genders' share in 
#  the remaining population in each year. I can be shown that morality tables calculated like this can generate the same series of remaining population of 
#  retirees as that generated by calculationg the populations of male and female retirees seperately.  


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



#*********************************************************************************************************
#              ##  Define model parameters  ####
#*********************************************************************************************************

load("Data/UCRP.inputs1.RData")
load("Data/UCRP.inputs2.RData")
load( "Data/UCRP.df.noAdj.RData")

source("Functions.R")


par.COLA   <- 0.02

par.i      <- 0.0725
# par.v      <- 1/(1 + par.i)

par.pct.ca    <- 0.25
par.age.range <- 53:120
par.age.r.range <- 53:70
par.mortality = mortality.post
par.reduction.factor = 1


pct.F <- 0.6
pct.M <- 1 - pct.F


#*********************************************************************************************************
#              ##  Function of calculating liability and benefit payment of contingent annuity  ####
#*********************************************************************************************************

get_liab.ben <- function(COLA, i, 
                         gender.R, pct.ca, age.range, age.r.range,
                         mortality = par.mortality,
                         reduction.factor = 1){
  # For a given range of retirement ages and gender, this function calculates the following for each retirement age:
  # 1) the individual liability and benefit payments for retirees and survivors in each year after retirement. 
  # 2) number of retirees(w/ and w/p living spouse) and survivors in each year after retirement. (initial population is 1)
  # 3) total liability and benefit payment for retirees and survivors in each year after retirement. 
  # 4) benefit reduction factor that makes the liability of contingent annuity equivalent to the liability of life annuity at the year of retirement.  
  
  
  # COLA   <- 0.02
  # B.init <- 1
  # i      <- 0.0725
  # v      <- 1/(1 + i)
  # 
  # gender.R  <- "M"
  # pct.ca    <- 0.25
  # age.range <- 53:120
  # age.r.range <- 53:70
  # mortality = mortality.post
  # reduction.factor = 1
  
  # Function to calculate individual liability attributable to surivor benefit. 
  get_surv.liab <- function(pRxm.R, qxm.R, pxm.S, i,  liab.ben){
    # This function calculates the ALs for a retiree that are attributable to benefits payable to survivors. 
    
    
    # pRxm.M = df2$pRxm.M
    # qxm.M  = df2$qxm.post.M
    # pxm.F  = df2$pxm.post.F
    # liab.ben = df2$liab.ben.indiv
    
    n <- length(liab.ben)
    surv.liab <- numeric(n)
    
    for(j in 1:(n - 1)){
      #j = 1  
      surv.liab[j] <- sum(
        pRxm.R[j:(n - 1)]/pRxm.R[j] * qxm.R[j:(n-1)] *   # probability of dying at each period 
          cumprod(pxm.S[j:(n-1)]) *                      # probability of spouse being still alive at each period 
          1/(1 + i)^(1 : (n-j)) * liab.ben[(j + 1):n]    # AL for survivor dis
      )  
    }
    
    surv.liab[n] <- 0  
    surv.liab
    
  }
  
  
  if("numeric" %in% class(reduction.factor) ) reduction.factor = data.frame(age.r = age.r.range, reduction = reduction.factor)
  
  
  df <- expand.grid(age = (min(age.range) - 3):max(age.range), age.r = age.r.range) %>% 
    left_join(mortality %>% filter(year == 2029) %>%  select(age, qxm.post.M, qxm.post.F)) %>%
    left_join(reduction.factor) %>% 
    group_by(age.r) %>%
    mutate(
      gender.R = gender.R,
      age.S = ifelse(gender.R == "M", age - 3, age + 3),
      qxm.R = ifelse(gender.R == "M", qxm.post.M, qxm.post.F),
      qxm.S = ifelse(gender.R == "M", lag(qxm.post.F, 3), lead(qxm.post.M, 3)),
      qxm.S = ifelse(gender.R == "M",
                     ifelse(age == max(age), 1, qxm.S), # For convenience, it is assumed that the max age for the female spouses is 117.)
                     ifelse(age.S > max(age.range), 1, qxm.S)
      ))%>% 
    filter(age >= age.r) %>%  
    mutate(
      COLA.scale = (1 + COLA)^(row_number() - 1 ),
      B =  COLA.scale * reduction, #* reduction,
      pxm.R = 1 - qxm.R,
      pxm.S = 1 - qxm.S,
      
      # AL of an individual retiree attributable to the life annuity at each age. 
      pRxm.R     = ifelse(age == min(age), 1, lag(cumprod(pxm.R))),
      ax.r.R     =  get_tla(pxm.R, i, COLA.scale),
      liab.ret.indiv.la = B * ax.r.R,    # "la" for life annuity
      
      # AL of an individual survivor at each age. 
      # (Note that the AL is only the function of the age of the survivor, and not affected by the the year of the retiree's death.)
      ax.b.S     = get_tla(pxm.S, i, COLA.scale),
      liab.ben.indiv = pct.ca * B * ax.b.S,
      
      # AL of an individual retriee with an living survivor atributable to the contingent annuity(survivor) at each age.
      # For each age, this is the sum of all future individual survivor liability weighed by the discount factor and 
      # the prob of the contingent annuity starting from that year. 
      
      liab.ret.indiv.ca = get_surv.liab(pRxm.R, qxm.R, pxm.S, i, liab.ben.indiv) # "ca" for contingent annuity
      
    ) %>% 
    
    ## setting up variables for demographics  ##
    
    mutate(
      
      pxm.R1S1 = pxm.R * pxm.S,
      
      qxm.R1S0 = pxm.R * (1 - pxm.S),
      qxm.R0S1 = (1 - pxm.R) * pxm.S, 
      qxm.R0S0 = (1 - pxm.R) * (1 - pxm.S), 
      
      n.R1S1 =  ifelse(age == min(age), 1, lag(cumprod(pxm.R1S1))),
      
      n.newR1S0 = n.R1S1 * qxm.R1S0,
      n.newR0S1 = n.R1S1 * qxm.R0S1,
      
      n.R1S0 = 0,
      n.R0S1 = 0,
      n.R0   = 0
    ) 
  
  
  
  ## demographic dynamics
  
  fn_demo <- function(df.demo){      
    for (j in 2:nrow(df.demo)){
      df.demo$n.R1S0[j] <- with(df.demo, n.R1S0[j-1] * pxm.R[j-1] + n.newR1S0[j-1])
      df.demo$n.R0S1[j] <- with(df.demo, n.R0S1[j-1] * pxm.S[j-1] + n.newR0S1[j-1])
      
      df.demo$n.R0[j] <- with(df.demo, n.R0[j - 1] + 
                                n.R1S1[j-1] * (1 - pxm.R[j-1]) +  
                                n.R1S0[j-1] * (1 - pxm.R[j-1]) )
    }
    return(df.demo)
  }
  
  df <- split(df, df$age.r) %>% lapply(fn_demo) %>% rbind_all
  
  # check demographic
  df.check <- df %>% mutate(tot.check = n.R1S1 + n.R1S0 + n.R0) %>% select(age, age.S, n.R1S1, n.R1S0, n.R0S1, tot.check) 
  
  
  ## Aggregate liability and benefit payment  ##
  
  df %<>% select(age.r,age, age.S, B, liab.ret.indiv.la, liab.ret.indiv.ca, liab.ben.indiv,
                 n.R1S1, n.R1S0, n.R0S1) %>% 
    group_by(age.r) %>% 
    mutate(
      B.R = (n.R1S1 + n.R1S0) * B,   # total annuity payment for retirees.
      B.S =  n.R0S1 * pct.ca * B,    # total contingent annuity payment for survivors.
      B.tot = B.R + B.S,             # total benefit payment
      
      liab.tot.R1S1 = (liab.ret.indiv.ca + liab.ret.indiv.la) * n.R1S1, # total liability for retirees whose spouses are alive.
      liab.tot.R1S0 = liab.ret.indiv.la * n.R1S0,                       # total liability for retirees whose spouses are dead.
      liab.tot.R0S1 = liab.ben.indiv * n.R0S1,                          # total liability for survivors. 
      liab.tot      = liab.tot.R1S1 + liab.tot.R1S0 + liab.tot.R0S1,    # total liability for all. 
      
      liab.tot.la   = liab.ret.indiv.la * (n.R1S1 + n.R1S0) ,           # total liability for a life annuity with the same initial benefit.
      reduction     = liab.tot.la / liab.tot,                           # only look at the first row
      
      MA = ifelse(age == min(age), liab.tot, 0),
      B_R = liab.tot.R0S1 / (liab.tot.R1S1 + liab.tot.R1S0),
      B.S_B.R = B.S / B.R)
  
  
  # Check internal consistency 
  
  fn_MA <- function(df){
    
    for (j in 2:nrow(df)){
      df$MA[j] <- with(df, (MA[j-1] - B.tot[j-1]) * (1 + i) )
    }
    return(df)
  }
  
  df <- split(df, df$age.r) %>% lapply(fn_MA) %>% rbind_all %>% 
    mutate(FR = MA / liab.tot) 
  
  return(df)
  
}


#*********************************************************************************************************
#              ##  Calculate benefit reduction factors for male and female members.   ####
#*********************************************************************************************************



# For male retirees
# w/o benefit reduction
df1.M <- get_liab.ben(COLA = par.COLA, i = par.i,
                      gender.R = "M", pct.ca = par.pct.ca, age.range = par.age.range, age.r.range = par.age.r.range)

# save the benefit reductin factor for contingent annuity. 
reduction.M <- df1.M %>% group_by(age.r) %>% filter(age == min(age)) %>% select(reduction) 



# For female retirees
# w/o benefit reduction
df1.F <- get_liab.ben(COLA = par.COLA, i = par.i,
                      gender.R = "F", pct.ca = par.pct.ca, age.range = par.age.range, age.r.range = par.age.r.range)

# save the benefit reductin factor for contingent annuity. 
reduction.F <- df1.F %>% group_by(age.r) %>% filter(age == min(age)) %>% select(reduction) 




#*********************************************************************************************************
#              ##  Calculate the liabilities and benefit payments with benefit reduction factors   ####
#*********************************************************************************************************

# w/ benefit reduction
df2.M <- get_liab.ben(COLA = par.COLA, i = par.i,
                      gender.R = "M", pct.ca = par.pct.ca, age.range = par.age.range, age.r.range = par.age.r.range,
                      reduction = reduction.M)

# w/ benefit reduction
df2.F <- get_liab.ben(COLA = par.COLA, i = par.i,
                      gender.R = "F", pct.ca = par.pct.ca, age.range = par.age.range, age.r.range = par.age.r.range,
                      reduction = reduction.F)


#*********************************************************************************************************
#              ##  calculate the grand total liability and benefit based on gender ratio.   ####
#*********************************************************************************************************

# female.pct <- 0.6
# male.pct   <- 1 - female.pct 

df.all <-   select(df2.M, age.r, age, B.tot.M = B.tot, B.R.M = B.R, B.S.M = B.S, 
                   liab.tot.M = liab.tot, liab.tot.R1S1.M = liab.tot.R1S1, liab.tot.R1S0.M = liab.tot.R1S0, liab.tot.R0S1.M = liab.tot.R0S1,
                   n.R1S1.M = n.R1S1, n.R1S0.M = n.R1S0, n.R0S1.M = n.R0S1) %>%
  
  left_join(select(df2.F, age.r, age,        B.tot.F = B.tot, B.R.F = B.R, B.S.F = B.S, 
                   liab.tot.F = liab.tot, liab.tot.R1S1.F = liab.tot.R1S1, liab.tot.R1S0.F = liab.tot.R1S0, liab.tot.R0S1.F = liab.tot.R0S1,
                   n.R1S1.F = n.R1S1, n.R1S0.F = n.R1S0, n.R0S1.F = n.R0S1)) %>% 
  mutate(
    liab.tot = pct.F * liab.tot.F + pct.M * liab.tot.M,
    liab.tot.R1S1 = pct.F * liab.tot.R1S1.F + pct.M * liab.tot.R1S1.M,
    liab.tot.R1S0 = pct.F * liab.tot.R1S0.F + pct.M * liab.tot.R1S0.M,
    liab.tot.R0S1 = pct.F * liab.tot.R0S1.F + pct.M * liab.tot.R0S1.M,
    
    B.tot    = pct.F * B.tot.F  + pct.M * B.tot.M,
    B.R      = pct.F * B.R.F    + pct.M * B.R.M,
    B.S      = pct.F * B.S.F    + pct.M * B.S.M,
    
    n.R1S1   = pct.F * n.R1S1.F + pct.M * n.R1S1.M,
    n.R1S0   = pct.F * n.R1S0.F + pct.M * n.R1S0.M,
    n.R0S1   = pct.F * n.R0S1.F + pct.M * n.R0S1.M
  )



#*********************************************************************************************************
#              ##  Final outputs   ####
#*********************************************************************************************************


df.output <- df.all %>% select(age.r, age, liab.tot, liab.tot.R1S1, liab.tot.R1S0, liab.tot.R0S1,
                                           B.tot, B.R, B.S,
                                           n.R1S1, n.R1S0, n.R0S1)

# For any given simulation year, the entire series of total liabilities and benefit payments for the members who retire in that year can be 
# obtained by multiplying the their initial retirement benefit with the B.tot and liab.tot of the corresponding retirement ages(age.r). Likewise,
# The series of population of retirees and survivors can be obtained by multiplying the number of retirees with n.R1S1, n.R1S0, n.R0S1 with the
# corresponding retirement ages(age.r). 





