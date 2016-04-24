


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
  


#####   Asset Smoothing  ####
#********************************************************************************
# a vector containing the porportion of unexpected investment loss/gain exluded from the calculation of AA.
s.vector <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector   





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
                  AA.Segal = AA.Segal,
                  init.smoothing = init.smoothing,
                  
                  C.Segal,
                  ERC.Segal,
                   
                  
                  AL = AL.Segal,
                  NC = NC.Segal,
                  SC_init = SC_init,
                  EEC = EEC.Segal,
                  UAAL.AA = UAAL.AA.Segal,
                  B = B.Segal,
                  PR = PR.Segal,
                  extFund = STIP.Segal
                  ) %>% 
           mutate(i = i,
                  MA = 0,
                  AA = 0,
                  UAAL.MA  = 0,
                  EUAAL.MA = 0,
                  Amort_basis = 0,
                  LG = 0,
                  ERC = 0,
                  C = 0,
                  I.r = 0,
                  init.smoothing = init.smoothing * f.initSmooth,
                  ERC.Segal.noSTIP = ERC.Segal - extFund) %>% 
           as.list


#### Investment returns ####
#********************************************************************************
# i.r <- rep(0.0725, nyear)

if(return_type == "simple"){
  set.seed(1234)
  i.r <- matrix(rnorm(nyear*nsim, ir.mean, ir.sd), nyear, nsim)
  i.r <- cbind(rep(ir.mean + ir.sd^2/2, nyear), i.r)
  colnames(i.r) <- 0:nsim
}


if (return_type == "internal"){
# return_scenario <- "RS4"
# nsim = 5

  returnScenarios_local <- returnScenarios %>% filter(scenario == return_scenario)

  i.r <- cbind(
    with(returnScenarios_local, create_returns(return_det, 0, period)),
    replicate(nsim, with(returnScenarios_local, create_returns(r.mean, r.sd, period)))
    )
  colnames(i.r) <- 0:nsim
}


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
  
  ## MA and AA 
  if(j == 1){
    # Initial MA and AA: both equal to Segal values
    penSim$MA[j] <- penSim$MA.Segal[j]
    penSim$AA[j] <- penSim$AA.Segal[j]            
    
  } else{
    # MA dynamics
    penSim$MA[j] <- with(penSim, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
    
    # AA dynamics
     # Alos adding UCRP initial smoothing (calculated as the difference between MA and AA in Segal projection)
    penSim$AA[j] <- with(penSim, MA[j] - init.smoothing[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j - min(j, s.year + 1) + 1):(j - 1)]))
    
  }  
  

  ## UAAL
  penSim$UAAL.AA[j] <- with(penSim, AL[j] - AA[j])
  penSim$UAAL.MA[j] <- with(penSim, AL[j] - MA[j])
  
  
  ## LG
   # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
  if (j == 1){
    #penSim$EUAAL.MA[j] <- 0
    penSim$EUAAL.AA[j] <- 0
    penSim$LG[j] <- with(penSim,  UAAL.AA[j])     # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
    penSim$Amort_basis[j] <- with(penSim, LG[j])  # This will not be used for UCRP since the amortization scheme for year 1 is provided by SC_amort.(from AV2015)
    
  } else {
    #penSim$EUAAL.MA[j]     <- with(penSim, (UAAL.MA[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
    penSim$EUAAL.AA[j]     <- with(penSim, (UAAL.AA[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
    penSim$LG[j]           <- with(penSim,  UAAL.AA[j]- EUAAL.AA[j])
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
    # EEC is fixed
    penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
    penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
    
    # # EEC is not fixed: Adjustment of EEC ADC < EEC
    if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
    
  } else {
    # Allow for negative ADC and C  
    penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
    
    # EEC is fixed
    if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
    
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
  
  # UCRP specific contribution policy "ADC":
   # SegalCon: the overrding parameter. If TRUE, just use Segal ERC - STIP as ERC
   # STIP: when SegalCon is FALSE, STIP makes up shortfall between ERC+EEC and ADC, up to the value of STIP.  

  penSim$ERC[j] <- switch(ConPolicy,
                          ADC     = with(penSim, max(0, ADC.ER[j] - extFund[j])),     # Full ADC
                          ADC_cap = with(penSim, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                          Fixed   = with(penSim, PR_pct_fixed * PR[j])                # Fixed percent of payroll
  ) 
  
  if(ERC.lb_EEC) penSim$ERC[j] <- with(penSim, max(ERC[j], EEC[j]))
  
  if(SegalCon) penSim$ERC[j] = with(penSim, ERC.Segal.noSTIP[j])
  
  ## C
  #penSim$C[j] = penSim$C.Segal[j]
  
  if(SegalCon) penSim$C[j] = with(penSim, EEC[j] + ERC[j] + extFund[j]) 
  
  else {penSim$C[j] = with(penSim, EEC[j] + ERC[j] + min(max(0, ADC[j] - (EEC[j] + ERC[j])), extFund[j]))
  }
  
  
  
  
  # C(j) - ADC(j)
  penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
  
  
  ## Ia(j), Ib(j), Ic(j): expected interest for cash flow components
  penSim$Ia[j] <- with(penSim,  MA[j] * i[j])
  penSim$Ib[j] <- with(penSim,  B[j]  * i[j])
  penSim$Ic[j] <- with(penSim,  C[j]  * i[j])
  
  ## I.r:   Actual investment income
   # It is assumed that all contributions and benefit payments are made at the beginning of year.  
  penSim$I.r[j] = with(penSim, i.r[j] * (MA[j] + C[j] - B[j]))

  
  # I.e(j): Expected investment income
  penSim$I.e[j] <- with(penSim, i[j] *(MA[j] + C[j] - B[j]))

  
    
  # I.dif(j) = I.r(j) - I.e(j): used in asset smoothing 
  penSim$I.dif[j] <- with(penSim, I.r[j] - I.e[j])
  }

penSim %<>% as.data.frame 
}

stopCluster(cl)

penSim_results <- bind_rows(penSim_results) %>%
                  mutate(runname = runname,
                         year = rep(2015:2044, nsim+1),
                         sim  = rep(0:nsim, each = nyear),
                         d_MA = 100*(MA/MA.Segal - 1),
                         d_AA = 100*(AA/AA.Segal - 1),
                         d_C   = 100*(C/C.Segal - 1),
                         d_ERC = 100*(ERC/(ERC.Segal- extFund) - 1),
                         
                         ERCwSTIP = ERC + extFund,
                         
                         ERC_PR = 100 * (ERC)/PR,
                         ERCwSTIP_PR = 100 * ERCwSTIP / PR,
                         ERC_PR.Segal = 100 * (ERC.Segal - extFund)/PR,
                         EEC_PR = 100 * EEC/PR,
                         
                         C_PR = 100 * C / PR,
                         ADC_PR = 100 * ADC / PR, 
                         
                         FR.MA       = 100 * MA/AL,
                         FR.MA.Segal = 100 * MA.Segal/AL,
                         d_FR.MA     = FR.MA - FR.MA.Segal,
                         
                         netxcf = C - B,
                         netxcf_MA = 100 * netxcf / MA,
                         netxcf_PR = 100 * netxcf / PR,
                         apratio   = MA/PR)

var.display <- c("sim","i.r",  "year", "AL", "MA.Segal", "MA", "d_MA", "AA.Segal", "AA", "d_AA", "FR.MA.Segal", "FR.MA", "d_FR.MA", "C.Segal", "C", "ERC.Segal", "ERC", "d_ERC", "ERC_PR.Segal", "ERC_PR") # , LG, C_ADC)"
# 
penSim_results %>% filter(sim == 2) %>% select(one_of(var.display)) %>% kable(digits = 3)

# 
# results_ADC <- penSim_results
# 




# save(results_ADC.cap, file = "Results_fullOverride/results_ADC.cap.RData")
# save(results_ADC, file = "Results_fullOverride/results_ADC.RData")

# 
# penSim_results %>% group_by(year) %>% summarize(FR.MA = median(FR.MA))
# 






## Matching Segal Projection under constant return of 7.25%.
 # Potential issues:
 # 1. Model ERC is slightly lower than Segal ERC in the last couple of years, which implies there are differences in amort scheme between the model calculation and Segal projection. 
 # 2. Loss/gain are not zero, even when investment return is set equal to discount rate 7.25%. Imply difference in the calculation of UAAL and/or EUAAL?  
 # 3. In Segal projection, contribuitons and benefit payments may not earn full interest since they are not made at the begining of year. For contributions, it is shown in AV2015 p25 that
      # the assumed interest rate applied to contributions is half of the discount rate when calculating expected UAAL. The Segal projection should be doing the same thing for contributions.
      # For benenfit payment, it is not sure whether partial interest rate is applied, but I suspect so.  



# 
# # Two funding policies 
# load("Results_fullOverride/results_ADC.cap.RData")
# load("Results_fullOverride/results_ADC.RData")
# 
# get_qts <- function(df){ 
#    df %>% group_by(year) %>% 
#           summarize(q90 = quantile(FR.MA, 0.90),
#                     q75 = quantile(FR.MA, 0.75),
#                     q50   = median(FR.MA),
#                     q25 = quantile(FR.MA, 0.25),
#                     q10 = quantile(FR.MA, 0.10))
# }
# 


# get_FR40less <- function(df){
#  df %>% group_by(sim) %>% 
#         mutate(FR40 = FR.MA <= 40) %>%
#         summarise(FR40 = any(FR40)) %>% 
#         summarise(FR40 = 100 * sum(FR40)/n())
# }
# 
# get_FR95more <- function(df){
#   df %>% mutate(FR95 = FR.MA >= 95) %>%
#          group_by(year) %>% 
#          summarise(FR95 = 100 * sum(FR95)/n())
# }
# 
# 
# # quantiles of FR
# results_ADC.cap %>% get_qts
# results_ADC     %>% get_qts
# 
# 
# # probability of FR below 40%
# results_ADC.cap %>% get_FR40less # 24.6%
# results_ADC %>% get_FR40less     # 0.5%
# 
# 
# # probability of 95% or better funding as of a given year
# results_ADC.cap %>% get_FR95more
# results_ADC     %>% get_FR95more



