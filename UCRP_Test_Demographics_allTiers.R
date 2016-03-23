# Simulation of the demograhics of UCRP

## Modifications on the original model
#1. Need to calculate the number of LSC claims(by ea, age) for each year. (Can be calculated after the loop) 
#2. Need to calculate the number of new retirees opting for contingent annuity(by ea, age) for each year. (Can be calculated after the loop) 
#3. The mortality for retirees are now retirement age dependent.


get_Population_allTiers <- function(
                           .init_pop.t76         = init_pop.t76 ,
                           .entrants_dist.t76    = entrants_dist.t76 ,
                           .decrement.ucrp.t76   = decrement.ucrp.t76 ,
                           
                           .init_pop.t13         = init_pop.t13,
                           .entrants_dist.t13    = entrants_dist.t13,
                           .decrement.ucrp.t13   = decrement.ucrp.t13,
                           
                           .init_pop.tm13         = init_pop.tm13,
                           .entrants_dist.tm13    = entrants_dist.tm13,
                           .decrement.ucrp.tm13   = decrement.ucrp.tm13,
                           
                           
                           .paramlist        = paramlist,
                           .Global_paramlist = Global_paramlist){

## Inputs
# - range_ea:         all possible entry ages  
# - range_age:        range of age
# - nyear:            number of years in simulation
# - wf_growth:        growth rate of the size of workforce
# - no_entrance:      no new entrants into the workforce if set "TRUE". Overrides "wf_growth"
# - Decrement table:  from Model_Decrements.R  
# - Initial workforce for each type:
#    - init_pop$actives:   matrix, max ea by max age
#    - init_pop$retirees:  matrix, max ea by max age


## An array is created for each of the 6 status:
#  (1)Active     (dim = 3)
#  (2)Terminated (dim = 4)
#  (3)Retired    (dim = 4)
#  (4)Disabled   (dim = 3) later will be expanded to dim = 4, with additional dim being year.disb
#  (5)Dead       (dim = 3) We do not really need an array for dead, what's needed is only the total number of dead.  

# Run the section below when developing new features.   
  .init_pop.t76         = init_pop.t76
  .entrants_dist.t76    = entrants_dist.t76
  .decrement.ucrp.t76   = decrement.ucrp.t76

  .init_pop.t13         = init_pop.t13
  .entrants_dist.t13    = entrants_dist.t13
  .decrement.ucrp.t13   = decrement.ucrp.t13

  .init_pop.tm13         = init_pop.tm13
  .entrants_dist.tm13    = entrants_dist.tm13
  .decrement.ucrp.tm13   = decrement.ucrp.tm13


  .paramlist        = paramlist
  .Global_paramlist = Global_paramlist
  .paramlist        = paramlist
  .Global_paramlist = Global_paramlist
# #   
#   

 assign_parmsList(.Global_paramlist, envir = environment())
 assign_parmsList(.paramlist,        envir = environment())  


#*************************************************************************************************************
#                                     Creating arrays for each status, FOR ALL TIERS ####
#*************************************************************************************************************

## In each 3D array, dimension 1(row) represents entry age, dimension 2(column) represents attained age,
# dimension 3(depth) represents number of year, dimension 4(terms only) represents the termination year. 
wf_dim      <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, range_age, init.year:(init.year + nyear - 1))

# The array of terminated has 4 dimensions: ea x age x year x year of termination
wf_dim.term      <- c(length(range_ea), length(range_age), nyear, nyear + 1)
wf_dimnames.term <- list(range_ea, range_age, init.year:(init.year + nyear - 1), (init.year - 1) :(init.year + nyear - 1))


# # The array of retirees has 4 dimensions: ea x age x year x year of retirement
wf_dim.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.la <- list(range_ea, range_age, init.year:(init.year + nyear - 1), init.year:(init.year + nyear - 1))


wf_active.t76 <- wf_active.t13 <- wf_active.tm13  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_disb.t76   <- wf_disb.t13   <- wf_disb.tm13    <- array(0, wf_dim, dimnames = wf_dimnames) 
wf_dead.t76   <- wf_dead.t13   <- wf_dead.tm13    <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term.t76   <- wf_term.t13   <- wf_term.tm13    <- array(0, wf_dim.term,    dimnames = wf_dimnames.term)
wf_la.t76     <- wf_la.t13     <- wf_la.tm13       <- array(0, wf_dim.la, dimnames = wf_dimnames.la)


newDeath.act.t76 <-  newDeath.act.t13 <-   newDeath.act.tm13   <- numeric(nyear)
newDeath.ret.t76 <-  newDeath.ret.t13 <-   newDeath.ret.tm13   <- numeric(nyear)
newDeath.term.t76 <- newDeath.term.t13 <-  newDeath.term.tm13  <- numeric(nyear)

newDisb.act.t76 <- newDisb.act.t13 <- newDisb.act.tm13 <- numeric(nyear)


#*************************************************************************************************************
#                                     Setting  initial population, TIER SPECIFIC ####
#*************************************************************************************************************

# Setting inital distribution of workforce and retirees.
# Note on initial retirees: It is assumed that all initial retirees entered the workforce at age 54 and retireed in year 1. 
# Altough this may produce yos greater than r.max - ea.min, it is irrelevant to the calculation since we do not care about initial retirees' yos.  
# 

# Tier 1976
wf_active.t76[, , 1]   <- .init_pop.t76$actives 
wf_la.t76[, , 1, 1]    <- .init_pop.t76$retirees
wf_term.t76[, , 1, 1]  <- .init_pop.t76$terms   # note that the initial terms are assigned to year.term = init.year - 1

# Tier 2013
wf_active.t13[, , 1]   <- .init_pop.t13$actives 
wf_la.t13[, , 1, 1]    <- .init_pop.t13$retirees
wf_term.t13[, , 1, 1]  <- .init_pop.t13$terms   # note that the initial terms are assigned to year.term = init.year - 1

# Tier Modified 2013
wf_active.tm13[, , 1]   <- .init_pop.tm13$actives 
wf_la.tm13[, , 1, 1]    <- .init_pop.tm13$retirees
wf_term.tm13[, , 1, 1]  <- .init_pop.tm13$terms   # note that the initial terms are assigned to year.term = init.year - 1




#*************************************************************************************************************
#                                     Defining population dynamics, TIER SPECIFIC  ####
#*************************************************************************************************************

## Transition matrices ####

# Assume the actual decrement rates are the same as the rates in decrement tables.
# Later we may allow the actual decrement rates to differ from the assumed rates. 


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = decrement_wf) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transtion matrix
  df %<>% select_("age", "ea", qx) %>% ungroup %>% spread_("ea", qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}


# select decrement tables
decrement_wf.t76  <- sapply(.decrement.ucrp.t76,  function(x){x[is.na(x)] <- 0; return(x)}) %>% data.frame # just for safety
decrement_wf.t13  <- sapply(.decrement.ucrp.t13,  function(x){x[is.na(x)] <- 0; return(x)}) %>% data.frame # just for safety
decrement_wf.tm13 <- sapply(.decrement.ucrp.tm13, function(x){x[is.na(x)] <- 0; return(x)}) %>% data.frame # just for safety


# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transtition matrices. 




## For Tier 1976

  # Where do the active go
  p_active2term.t76    <- make_dmat("qxt",     decrement_wf.t76)
  p_active2disb.t76    <- make_dmat("qxd",     decrement_wf.t76)
  p_active2dead.t76    <- make_dmat("qxm.pre", decrement_wf.t76)
  p_active2retired.t76 <- make_dmat("qxr",     decrement_wf.t76)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t76      <- make_dmat("qxr.la",  decrement_wf.t76)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t76    <- make_dmat("qxm.term", decrement_wf.t76) # need to modify later
  
  # Where do the disabled go
  p_disb2dead.t76    <- make_dmat("qxm.pre", decrement_wf.t76) #need to modify later.

  
## For Tier 2013
  
  # Where do the active go
  p_active2term.t13    <- make_dmat("qxt",     decrement_wf.t13)
  p_active2disb.t13    <- make_dmat("qxd",     decrement_wf.t13)
  p_active2dead.t13    <- make_dmat("qxm.pre", decrement_wf.t13)
  p_active2retired.t13 <- make_dmat("qxr",     decrement_wf.t13)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t13      <- make_dmat("qxr.la",  decrement_wf.t13)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t13    <- make_dmat("qxm.term", decrement_wf.t13) # need to modify later
  
  # Where do the disabled go
  p_disb2dead.t13    <- make_dmat("qxm.pre", decrement_wf.t13) #need to modify later.
  

## For Tier Modified 2013
  
  # Where do the active go
  p_active2term.tm13    <- make_dmat("qxt",     decrement_wf.tm13)
  p_active2disb.tm13    <- make_dmat("qxd",     decrement_wf.tm13)
  p_active2dead.tm13    <- make_dmat("qxm.pre", decrement_wf.tm13)
  p_active2retired.tm13 <- make_dmat("qxr",     decrement_wf.tm13)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.tm13      <- make_dmat("qxr.la",  decrement_wf.tm13)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.tm13    <- make_dmat("qxm.term", decrement_wf.tm13) # need to modify later
  
  # Where do the disabled go
  p_disb2dead.tm13    <- make_dmat("qxm.pre", decrement_wf.tm13) #need to modify later.
  
  
  
# Where do the retirees go, FOR ALL TIERS 
  # Before we find better approach, the age.r dependent mortality for retirees are given in a data frame containing all combos 
  # of year, year.r, ea, and age that exist in wf_la. 

  p_la2dead.t76 <- p_la2dead.t13 <-p_la2dead.tm13 <- 
    expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
    #filter(age >= ea) %>% 
    mutate(age.r = age - (year - year.r)) %>% 
    left_join(mortality.post.ucrp %>% select(age.r, age, qxm.post.W)) %>%
    mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
    arrange(year, year.r, age, ea)





#*************************************************************************************************************
#                                     Creating a function to calculate new entrants ####
#*************************************************************************************************************

# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, dist, no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after  decrement.  
  # delta: growth rate of workforce
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(r.max - 1)
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0*(1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1 # number of workers need to hire
  ne <- size_hire*dist               # vector, number of new entrants by age
  
  # Create the new entrant matrix 
  NE <- wf0; NE[ ,] <- 0
  
  if (no.entrants){ 
    return(NE) 
  } else {
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
  } 
}

# test the function 
# wf0 <- wf_active[, , 1]
# wf1 <- wf_active[, , 1]*(1 - p_active2term)
# sum(wf0, na.rm = T) - sum(wf1, na.rm = T)
# sum(calc_entrants(wf0, wf1, 0), na.rm = T)


#*************************************************************************************************************
#                               Creating a function to calculate new entrants, FOR ALL TIERS ####
#*************************************************************************************************************

# define function for determining the number of new entrants 
calc_entrants_allTiers <- function(wf0.t76,  wf0.t13,  wf0.tm13, 
                                   wf1.t76,  wf1.t13,  wf1.tm13, 
                                   dist.t76, dist.t13, dist.tm13,
                                   newEnt_byTier,
                                   delta, 
                                   no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after  decrement.  
  # delta: growth rate of workforce
  # newEnt_byTier: named vector, proportion of new entrants entering each tier. names must be "t76", "t13", "tm13"
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(r.max - 1) # FOR ALL TIERS
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0.t76 <- sum(wf0.t76[,as.character(working_age)], na.rm = TRUE)
  size1.t76 <- sum(wf1.t76[,as.character(working_age)], na.rm = TRUE)
  
  size0.t13 <- sum(wf0.t13[,as.character(working_age)], na.rm = TRUE)
  size1.t13 <- sum(wf1.t13[,as.character(working_age)], na.rm = TRUE)
  
  size0.tm13 <- sum(wf0.tm13[,as.character(working_age)], na.rm = TRUE)
  size1.tm13 <- sum(wf1.tm13[,as.character(working_age)], na.rm = TRUE)
  
  
  # computing new entrants
  size_target <- (size0.t76 + size0.t13 + size0.tm13) * (1 + delta)   # size of the workforce next year
  size_hire   <- size_target - (size1.t76 + size1.t13 + size1.tm13)  # number of workers need to hire
  
  ne.t76  <- size_hire * newEnt_byTier["t76"]  * dist.t76 # vector, number of new entrants by age
  ne.t13  <- size_hire * newEnt_byTier["t13"]  * dist.t13 
  ne.tm13 <- size_hire * newEnt_byTier["tm13"] * dist.tm13 
  
  # Create the new entrant matrix 
  NE.t76 <- wf0.t76  ;  NE.t76[ ,]  <- 0
  NE.t13 <- wf0.t13  ;  NE.t13[ ,]  <- 0
  NE.tm13 <- wf0.tm13;  NE.tm13[ ,] <- 0
  
  if (no.entrants){ 
    return(NE = list(NE.t76 = NE.t76, NE.t13 = NE.t13, NE.tm13 = NE.tm13)) 
  
  } else {
    NE.t76[, rownames(NE.t76)] <- diag(ne.t76) # place ne on the matrix of new entrants
    NE.t13[, rownames(NE.t13)] <- diag(ne.t13)
    NE.tm13[, rownames(NE.tm13)] <- diag(ne.tm13)
    return(NE = list(NE.t76 = NE.t76, NE.t13 = NE.t13, NE.tm13 = NE.tm13)) 
  } 
}  

# # test the function 
#   wf0.t76  <- wf_active.t76[, , 1]
#   wf0.t13  <- wf_active.t13[, , 1]
#   wf0.tm13 <- wf_active.tm13[, , 1]
#   
#   wf1.t76  <- wf_active.t76[, , 1] *(1 - p_active2term.t76)
#   wf1.t13  <- wf_active.t13[, , 1] *(1 - p_active2term.t13)
#   wf1.tm13 <- wf_active.tm13[, , 1]*(1 - p_active2term.tm13)
#   
#   (sum(wf0.t76 + wf0.t13 + wf0.tm13, na.rm = T) - sum(wf1.t76 + wf1.t13 + wf1.tm13, na.rm = T))
#   
#   NE <- calc_entrants_allTiers(wf0.t76, wf0.t13, wf0.tm13,
#                              wf1.t76,  wf1.t13,  wf1.tm13, 
#                              entrants_dist.t76, entrants_dist.t13, entrants_dist.tm13,
#                              c(t76 = 0, t13 = 0.8, tm13 = 0.2), 0)
#   sapply(NE, sum) %>% sum
#   
  
  


#*************************************************************************************************************
#                                     Simulating the evolution of population  ####
#*************************************************************************************************************


# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
# A square matrix with the dimension length(range_age)
# created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 



# Now the next slice of the array (array[, , i + 1]) is defined
# wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
# i runs from 2 to nyear. 

for (j in 1:(nyear - 1)){
   #j <-  1  
  
  #*******************************************
  # Stage 1 Seperations by type in each tier *
  #*******************************************
  
  ## Tier 1976
  # compute the inflow to and outflow
  active2term.t76    <- wf_active.t76[, , j] * p_active2term.t76  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
  active2disb.t76    <- wf_active.t76[, , j] * p_active2disb.t76
  active2dead.t76    <- wf_active.t76[, , j] * p_active2dead.t76
  active2retired.t76 <- wf_active.t76[, , j] * p_active2retired.t76    # This will be used to calculate the number of actives leaving the workforce
  active2la.t76      <- wf_active.t76[, , j] * p_active2la.t76          # This will join wf_la[, , j + 1, j + 1].
  
  # Where do the terminated_vested go
  term2dead.t76  <- wf_term.t76[, , j, ] * as.vector(p_term2dead.t76)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
  
  # Where do the retired go
  la2dead.t76   <- wf_la.t76[, , j, ] * (p_la2dead.t76 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  
  # Where do the disabled go
  disb2dead.t76 <- wf_disb.t76[, , j] * p_disb2dead.t76
  
  
  ## Tier 2013
  # compute the inflow to and outflow
  active2term.t13    <- wf_active.t13[, , j] * p_active2term.t13  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
  active2disb.t13    <- wf_active.t13[, , j] * p_active2disb.t13
  active2dead.t13    <- wf_active.t13[, , j] * p_active2dead.t13
  active2retired.t13 <- wf_active.t13[, , j] * p_active2retired.t13    # This will be used to calculate the number of actives leaving the workforce
  active2la.t13      <- wf_active.t13[, , j] * p_active2la.t13          # This will join wf_la[, , j + 1, j + 1].
  
  # Where do the terminated_vested go
  term2dead.t13  <- wf_term.t13[, , j, ] * as.vector(p_term2dead.t13)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
  
  # Where do the retired go
  la2dead.t13   <- wf_la.t13[, , j, ] * (p_la2dead.t13 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  
  # Where do the disabled go
  disb2dead.t13 <- wf_disb.t13[, , j] * p_disb2dead.t13
  
  
  ## Tier Modified 2013
  # compute the inflow to and outflow
  active2term.tm13    <- wf_active.tm13[, , j] * p_active2term.tm13  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
  active2disb.tm13    <- wf_active.tm13[, , j] * p_active2disb.tm13
  active2dead.tm13    <- wf_active.tm13[, , j] * p_active2dead.tm13
  active2retired.tm13 <- wf_active.tm13[, , j] * p_active2retired.tm13    # This will be used to calculate the number of actives leaving the workforce
  active2la.tm13      <- wf_active.tm13[, , j] * p_active2la.tm13          # This will join wf_la[, , j + 1, j + 1].
  
  # Where do the terminated_vested go
  term2dead.tm13  <- wf_term.tm13[, , j, ] * as.vector(p_term2dead.tm13)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
  
  # Where do the retired go
  la2dead.tm13   <- wf_la.tm13[, , j, ] * (p_la2dead.tm13 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  
  # Where do the disabled go
  disb2dead.tm13 <- wf_disb.tm13[, , j] * p_disb2dead.tm13
  
  

  #***********************************
  # Stage 2 Seperations in each tier *
  #***********************************
  
  ### Total inflow and outflow for each status
  
  # Calculating new entrants
  out_active.t76   <- active2term.t76 + active2disb.t76 + active2retired.t76 + active2dead.t76 
  out_active.t13   <- active2term.t13 + active2disb.t13 + active2retired.t13 + active2dead.t13 
  out_active.tm13   <- active2term.tm13 + active2disb.tm13 + active2retired.tm13 + active2dead.tm13 
  
  
  new_entrants_allTiers <- 
  calc_entrants_allTiers(wf_active.t76[, , j],  wf_active.t13[, , j], wf_active.tm13[, , j],
                         wf_active.t76[, , j]  - out_active.t76, wf_active.t13[, , j]  - out_active.t13,  wf_active.tm13[, , j] - out_active.tm13,
                         
                         entrants_dist.t76, entrants_dist.t13, entrants_dist.tm13,
                         
                         newEnt_byTier,
                         
                         wf_growth,
                         
                         no.entrants = no_entrance) # new entrants
  
  # 
  # sum(out_active.t76) + 
  # sum(out_active.t13) +
  # sum(out_active.tm13)
  # 
  # sum(new_entrants_allTiers$NE.t76) + 
  # sum(new_entrants_allTiers$NE.t13) +
  # sum(new_entrants_allTiers$NE.tm13)
  # 
           
  # new_entrants_allTiers %>% names
  # new_entrants <- calc_entrants(wf_active[, , j], wf_active[, , j] - out_active, wf_growth, dist = .entrants_dist, no.entrants = no_entrance) # new entrants
  
  
  ## Tier 1976
  out_term.t76 <- term2dead.t76    # This is a 3D array 
  in_term.t76  <- active2term.t76  # This is a matrix
  
  out_disb.t76 <- disb2dead.t76
  in_disb.t76  <- active2disb.t76
  
  out_la.t76 <- la2dead.t76        # This is a 3D array (ea x age x year.retire)
  in_la.t76  <- active2la.t76      # This is a matrix
  
  in_dead.t76 <- active2dead.t76 +                                        # In UCRP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
    apply(term2dead.t76, c(1,2), sum) + apply(la2dead.t76, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
    disb2dead.t76 

  
  ## Tier 2013
  out_term.t13 <- term2dead.t13    # This is a 3D array 
  in_term.t13  <- active2term.t13  # This is a matrix
  
  out_disb.t13 <- disb2dead.t13
  in_disb.t13  <- active2disb.t13
  
  out_la.t13 <- la2dead.t13        # This is a 3D array (ea x age x year.retire)
  in_la.t13  <- active2la.t13      # This is a matrix
  
  in_dead.t13 <- active2dead.t13 +                                        # In UCRP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
    apply(term2dead.t13, c(1,2), sum) + apply(la2dead.t13, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
    disb2dead.t13 
  
  
  ## Tier Modified 2013
  out_term.tm13 <- term2dead.tm13    # This is a 3D array 
  in_term.tm13  <- active2term.tm13  # This is a matrix
  
  out_disb.tm13 <- disb2dead.tm13
  in_disb.tm13  <- active2disb.tm13
  
  out_la.tm13 <- la2dead.tm13        # This is a 3D array (ea x age x year.retire)
  in_la.tm13  <- active2la.tm13      # This is a matrix
  
  in_dead.tm13 <- active2dead.tm13 +                                        # In UCRP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
    apply(term2dead.tm13, c(1,2), sum) + apply(la2dead.tm13, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
    disb2dead.tm13 
  

  
  #*********************************************
  # Stage 3  Calculate workforce for next year. 
  #*********************************************  
  
  ## Tier 1976
  wf_active.t76[, , j + 1]  <- (wf_active.t76[, , j] - out_active.t76) %*% A + new_entrants_allTiers$NE.t76
  
  wf_term.t76[, , j + 1, ]      <- apply((wf_term.t76[, , j, ] - out_term.t76), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
  wf_term.t76[, , j + 1, j + 1] <- in_term.t76 %*% A     # Note that termination year j = 1 correponds to init.year - 1
  
  wf_la.t76[, ,j + 1, ]       <- apply((wf_la.t76[, , j, ] - out_la.t76), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
  wf_la.t76[, , j + 1, j + 1] <- in_la.t76 %*% A
  
  wf_disb.t76[, ,   j + 1]    <- (wf_disb.t76[, , j] + in_disb.t76 - out_disb.t76) %*% A
  wf_dead.t76[, ,   j + 1]    <- (wf_dead.t76[, , j] + in_dead.t76) %*% A
  
  newDeath.act.t76[j]  <- sum(active2dead.t76)
  newDeath.ret.t76[j]  <- sum(la2dead.t76)
  # newDeath.term[j] <- sum()
  
  newDisb.act.t76[j] <- sum(active2disb.t76)
  
  
  
  ## Tier 1976
  wf_active.t13[, , j + 1]  <- (wf_active.t13[, , j] - out_active.t13) %*% A + new_entrants_allTiers$NE.t13
  
  wf_term.t13[, , j + 1, ]      <- apply((wf_term.t13[, , j, ] - out_term.t13), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
  wf_term.t13[, , j + 1, j + 1] <- in_term.t13 %*% A     # Note that termination year j = 1 correponds to init.year - 1
  
  wf_la.t13[, ,j + 1, ]       <- apply((wf_la.t13[, , j, ] - out_la.t13), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
  wf_la.t13[, , j + 1, j + 1] <- in_la.t13 %*% A
  
  wf_disb.t13[, ,   j + 1]    <- (wf_disb.t13[, , j] + in_disb.t13 - out_disb.t13) %*% A
  wf_dead.t13[, ,   j + 1]    <- (wf_dead.t13[, , j] + in_dead.t13) %*% A
  
  newDeath.act.t13[j]  <- sum(active2dead.t13)
  newDeath.ret.t13[j]  <- sum(la2dead.t13)
  # newDeath.term[j] <- sum()
  
  newDisb.act.t13[j] <- sum(active2disb.t13)
  
  
  
  ## Tier Modified 2013
  wf_active.tm13[, , j + 1]  <- (wf_active.tm13[, , j] - out_active.tm13) %*% A + new_entrants_allTiers$NE.tm13
  
  wf_term.tm13[, , j + 1, ]      <- apply((wf_term.tm13[, , j, ] - out_term.tm13), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
  wf_term.tm13[, , j + 1, j + 1] <- in_term.tm13 %*% A     # Note that termination year j = 1 correponds to init.year - 1
  
  wf_la.tm13[, ,j + 1, ]       <- apply((wf_la.tm13[, , j, ] - out_la.tm13), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
  wf_la.tm13[, , j + 1, j + 1] <- in_la.tm13 %*% A
  
  wf_disb.tm13[, ,   j + 1]    <- (wf_disb.tm13[, , j] + in_disb.tm13 - out_disb.tm13) %*% A
  wf_dead.tm13[, ,   j + 1]    <- (wf_dead.tm13[, , j] + in_dead.tm13) %*% A
  
  newDeath.act.tm13[j]  <- sum(active2dead.tm13)
  newDeath.ret.tm13[j]  <- sum(la2dead.tm13)
  # newDeath.term[j] <- sum()
  
  newDisb.act.tm13[j] <- sum(active2disb.tm13)
}



#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************

## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

get_df.wf_active <- function(df){
    df <- adply(df, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
    rename(year = X1) %>%
    gather(age, number.a, -ea, -year) %>% 
    mutate(year = f2n(year), age = as.numeric(age)) %>% 
    filter(age >= ea)
}
 wf_active.t76 <- get_df.wf_active(wf_active.t76)
 wf_active.t13 <- get_df.wf_active(wf_active.t13)
 wf_active.tm13 <- get_df.wf_active(wf_active.tm13)

 
get_df.wf_la <- function(df){
  df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)),
                   number.la = as.vector(df)) %>% 
    filter(age >= ea)
}
wf_la.t76  <- get_df.wf_la(wf_la.t76)
wf_la.t13  <- get_df.wf_la(wf_la.t13)
wf_la.tm13 <- get_df.wf_la(wf_la.tm13)



get_df.wf_term <- function(df){
    df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.term = (init.year-1):(init.year + nyear - 1)),
                     number.v = as.vector(df)) %>% 
    filter(age >= ea)
  
}

wf_term.t76  <- get_df.wf_term(wf_term.t76)
wf_term.t13  <- get_df.wf_term(wf_term.t13)
wf_term.tm13 <- get_df.wf_term(wf_term.tm13)


# summarize term across termination year. Resulting data frame will join .Liab$active as part of the output. 
# term_reduced <- wf_term %>% group_by(year, age) %>% summarise(number.v = sum(number.v, na.rm = TRUE))




#*************************************************************************************************************
#                                     Number of members opting for LSC and contingent annuity   ####
#*************************************************************************************************************

get_wf_LSC.ca <- function(df_actives, decrement){
  wf_LSC.ca <- df_actives %>% left_join(decrement %>% select(age, ea, qxr.LSC, qxr.ca)) %>% 
               mutate(new_LSC = number.a * qxr.LSC,
               new_ca  = number.a * qxr.ca,
               year = year + 1,
               age  = age + 1)
}

wf_LSC.ca.t76  <- get_wf_LSC.ca(wf_active.t76,  decrement_wf.t76)
wf_LSC.ca.t13  <- get_wf_LSC.ca(wf_active.t13,  decrement_wf.t13)
wf_LSC.ca.tm13 <- get_wf_LSC.ca(wf_active.tm13, decrement_wf.tm13)



# Final outputs

pop <- list(   
     pop.t76 = list(active = wf_active.t76, term = wf_term.t76, disb = wf_disb.t76, la = wf_la.t76, dead = wf_dead.t76, LSC.ca = wf_LSC.ca.t76),
     pop.t13 = list(active = wf_active.t13, term = wf_term.t13, disb = wf_disb.t13, la = wf_la.t13, dead = wf_dead.t13, LSC.ca = wf_LSC.ca.t13),
     pop.tm13 = list(active = wf_active.tm13, term = wf_term.tm13, disb = wf_disb.tm13, la = wf_la.tm13, dead = wf_dead.tm13, LSC.ca = wf_LSC.ca.tm13)
)

return(pop)

}


#pop <- get_Population_allTiers()





# pop$term %>% filter(year == 2016) %>% select(number.v) %>% sum



# # Spot check the results
# wf_active %>% group_by(year) %>% summarise(n = sum(number.a)) %>% mutate(x = n == 1000) %>% data.frame # OK
# wf_active %>% filter(year == 2025) %>% spread(age, number.a)
# 
# 
# wf_la %>% group_by(year) %>% summarise(n = sum(number.la)) %>% data.frame  
# 
# wf_la %>% filter(year.r == 2016, year == 2018, age==65) %>% mutate(number.la_next = number.la * 0.9945992) %>% 
#   left_join(wf_la %>% filter(year.r == 2016, year == 2019, age==66) %>% select(year.r, ea, number.la_true = number.la)) %>% 
#   mutate(diff = number.la_true - number.la_next) # looks ok.
# 
# mortality.post.ucrp %>% filter(age.r == 63)
# 
# 
# 
# 
# # check retirement
# wf_active %>% filter(year == 2020, ea == 30) %>% select(-year) %>% 
# left_join(wf_la     %>% filter(year == 2021, year.r == 2021, ea == 30)) %>% 
# left_join(wf_LSC.ca %>% filter(year == 2021, ea == 30) %>% select(year, ea, age, new_LSC, new_ca)) %>% 
# left_join(decrement_wf %>% filter(ea == 30) %>% select(ea, age, qxr, qxr.la, qxr.ca, qxr.LSC)) %>% 
# filter(age >= 49 & age <=75) %>% 
# mutate(diff.la = lag(number.a *qxr.la) - number.la,
#        diff.ca = lag(number.a *qxr.ca) - new_ca,
#        diff.LSC= lag(number.a *qxr.LSC) - new_LSC,
#        diff.r  = lag(number.a *qxr) - (new_ca + new_LSC + number.la))
#   # looks ok.
#





# wf_active %>% group_by(year) %>% summarise(n = sum(number.a))
# 
# wf_active %>% head
# 






