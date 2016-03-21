
# need pct.ca and pct.la from Data_population


# Final outputs
 # 1. decrement.ucrp
 # 2. mortality.post.ucrp


#*************************************************************************************************************
#                                Prepare mortality tables for UCRP                        #####                  
#*************************************************************************************************************

mortality.ucrp <- data.frame(age = range_age) %>% 
  left_join(mortality.post %>% filter(year == 2029) %>% select(age ,qxm.post.M, qxm.post.F)) %>% 
  left_join(mortality.pre  %>% filter(year == 2029) %>% select(age ,qxm.pre.M,  qxm.pre.F)) %>% 
  mutate(qxm.pre = qxm.pre.M  * pct.M.actives + qxm.pre.F  * pct.F.actives,   # mortality for actives
         qxm.LSC = lag(qxm.post.M * pct.M.LSC + qxm.post.F * pct.F.LSC),      # mortality used for LSC, age is moved one year ahead according to AV.
         qxm.LSC = ifelse(age == 50, lead(qxm.LSC),                           # Note the hard-coded "50", it is the starting post-retirement mortality in RP2014
                          ifelse(age == max(age), 1, qxm.LSC)),
         
         qxm.post.M = ifelse(age == 50, qxm.post.M, 
                             ifelse(age == max(age), 1, lag(qxm.post.M))),
         
         qxm.post.F = ifelse(age == 50, qxm.post.F, 
                             ifelse(age == max(age), 1, lag(qxm.post.F)))) %>% 
  select(age, qxm.pre, qxm.LSC, qxm.post.M, qxm.post.F)


# compute present values of life annuity(with cola) at each retirement age, using uni-sex mortality with age dependent weights
mortality.post.ucrp <- expand.grid(age = range_age, age.r = min(range_age.r):max.age) %>% 
  left_join(mortality.ucrp) %>%
  filter(age >= age.r) %>% 
  group_by(age.r) %>%  
  mutate(
    pxm.post.M = 1 - qxm.post.M,
    pxm.post.F = 1 - qxm.post.F,
    
    pRxm.M     = pct.M.actives * ifelse(age == min(age), 1, lag(cumprod(pxm.post.M))),
    pRxm.F     = pct.F.actives * ifelse(age == min(age), 1, lag(cumprod(pxm.post.F))),
    
    w.M = pRxm.M / (pRxm.M + pRxm.F),
    w.F = pRxm.F / (pRxm.M + pRxm.F),
    
    qxm.post.W = qxm.post.M * w.M + qxm.post.F * w.F, # dynamically weighted mortality
    pxm.post.W = 1 - qxm.post.W,
    
    COLA.scale = (1 + cola)^(row_number() - 1 ),
    B =  COLA.scale,
    ax.r.W     =  get_tla(pxm.post.W, i, COLA.scale),
    liab.la.W = B * ax.r.W    # "la" for life annuity
  )  %>% 
  select(age, qxm.post.W, pxm.post.W, ax.r.W)


# construct mortality rate for terms: 
 # before r.full: qxm.pre
 # after r.full: qxm.post.W with age.r ==  r.full

mortality.ucrp %<>% left_join(mortality.post.ucrp %>% ungroup %>%  filter(age.r == r.full) %>% select(age, qxm.post.term = qxm.post.W)) %>% 
                    mutate(qxm.term = ifelse(age < r.full, qxm.pre, qxm.post.term))




                              
disbrates.ucrp <- disbrates %>%  mutate(qxd = qxd.M * pct.M.actives + qxd.F * pct.F.actives)

termrates.ucrp <- termrates %>% mutate(#qxt = qxt_faculty
                                        qxt.t76  = qxt.faculty * pct.fac.actives.t76 + qxt.staff * pct.stf.actives.t76,
                                        qxt.t13  = qxt.faculty * pct.fac.actives.t13 + qxt.staff * pct.stf.actives.t13,
                                        qxt.tm13 = qxt.faculty * pct.fac.actives.tm13 + qxt.staff * pct.stf.actives.tm13
                                       )


# termrates.ucrp <- termrates %>% mutate(
#   qxt.t76  = qxt.faculty, 
#   qxt.t13  = qxt.faculty, 
#   qxt.tm13 = qxt.faculty
# )



retrates.ucrp  <- retrates %>% mutate(qxr.t76  = qxr.t76.fac * pct.fac.actives.t76 + qxr.t76.stf * pct.stf.actives.t76,
                                      qxr.t13  = qxr.t13.fac * pct.fac.actives.t13 + qxr.t13.stf * pct.stf.actives.t13,
                                      qxr.tm13 = qxr.t13.fac * pct.fac.actives.tm13 + qxr.tm13.stf * pct.stf.actives.tm13) %>% # assume Tier 2013 and Tier modified 2013 have the same faculty retirement rates.
             
                  select(age, qxr.t76, qxr.t13, qxr.tm13) 


#*************************************************************************************************************
#                      2. Putting together decrements and calculate surviving rates  ####
#*************************************************************************************************************

# Create decrement table and calculate probability of survival
decrement.ucrp <- expand.grid(age = range_age, ea = range_ea) %>% 
  mutate(yos = age - ea) %>% 
  filter(age >= ea) %>% 
  left_join(mortality.ucrp) %>%                  # mortality 
  left_join(termrates.ucrp)  %>%                 # termination
  left_join(disbrates.ucrp)  %>%                 # disability
# left_join(dbl)   %>%                           # mortality for disabled
  left_join(retrates.ucrp) %>%                   # early retirement
  left_join(LSCrates) %>%                        # LSC rate
  select(ea, age, everything()) %>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.) %>% 
  group_by(ea) 



## Imposing restrictions 
decrement.ucrp %<>% mutate(
  # 1. Coerce termination rates to 0 when eligible for early retirement or reaching than r.full(when we assume terms start to receive benefits). 
  qxt.t76  = ifelse((age >= r.min & (age - ea) >= r.yos) | age >= r.full, 0, qxt.t76),
  qxt.t13  = ifelse((age >= r.min & (age - ea) >= r.yos) | age >= r.full, 0, qxt.t13),
  qxt.tm13 = ifelse((age >= r.min & (age - ea) >= r.yos) | age >= r.full, 0, qxt.tm13),
  
  
  #qxt = ifelse(age >= r.min | age >= r.full, 0, qxt),
  # qxt = ifelse( age >= r.full, 0, qxt),
  # 2. Coerce retirement rates to 0 when age greater than r.max                     
  #   qxr = ifelse(age == r.max, 1, 
  #                ifelse(age %in% r.min:(r.max - 1), qxr, 0))
  #   
  qxr.t76 = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
               ifelse(yos < r.yos, 0, 
                      ifelse(age %in% 50:(r.max - 1), qxr.t76, 0)
                      )
                   ),
  
  qxr.t13 = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
                   ifelse(yos < r.yos, 0, 
                          ifelse(age %in% 55:(r.max - 1), qxr.t13, 0)
                   )
  ),

  qxr.tm13 = ifelse(age == r.max, 1,  # Assume retirement rates applies only when they are applicable (according to Bob North.)
                   ifelse(yos < r.yos, 0, 
                          ifelse(age %in% 50:(r.max - 1), qxr.tm13, 0)
                   )
  ) 
  ) 





# Adjustment to the decrement table:
# Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
# For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
# which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
# whether they will die at r.max)      

decrement.ucrp %<>% group_by(ea) %>%  
  mutate(
         # 1976 Tier: LSC, life annuity and contingent annuity
         qxr.t76 = ifelse(age == r.max - 1,
                            1 - qxt.t76 - qxm.pre - qxd, 
                            lead(qxr.t76)*(1 - qxt.t76 - qxm.pre - qxd)),                         # Total probability of retirement
         qxr.LSC.t76     = ifelse(age == r.max, 0 , qxr.t76 * lead(qxLSC.act)),               # Prob of opting for LSC
         qxr.la.t76      = ifelse(age == r.max, 0 , qxr.t76 * lead(1 - qxLSC.act) * pct.la.t76),  # Prob of opting for life annuity
         qxr.ca.t76      = ifelse(age == r.max, 0 , qxr.t76 * lead(1 - qxLSC.act) * pct.ca.t76),  # Prob of opting for contingent annuity

         # 2013 Tier: life annuity only  
         qxr.t13 = ifelse(age == r.max - 1,
                            1 - qxt.t13 - qxm.pre - qxd, 
                            lead(qxr.t13)*(1 - qxt.t13 - qxm.pre - qxd)),                         # Total probability of retirement
         qxr.LSC.t13     = 0,                                                                 # Prob of opting for LSC
         qxr.la.t13      = qxr.t13,                                                           # Prob of opting for life annuity
         qxr.ca.t13      = 0,                                                                 # Prob of opting for contingent annuity

         # modified 2013 Tier: LSC and life annuity.   
         qxr.tm13        = ifelse(age == r.max - 1,
                            1 - qxt.tm13 - qxm.pre - qxd, 
                            lead(qxr.tm13)*(1 - qxt.tm13 - qxm.pre - qxd)),                   # Total probability of retirement
         qxr.LSC.tm13     = ifelse(age == r.max, 0 , qxr.tm13 * lead(qxLSC.act)),           # Prob of opting for LSC                                                        # Prob of opting for LSC
         qxr.la.tm13      = ifelse(age == r.max, 0 , qxr.tm13 * lead(1 - qxLSC.act)),         # Prob of opting for life annuity                                                 # Prob of opting for life annuity
         qxr.ca.tm13      = 0)                                                                # Prob of opting for contingent annuity
         





######!!!! need to construct retirement age dependent mortality for life annuitants.
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  #mutate(qxm.la.r   = qxm.r) 



# Calculate various survival probabilities
decrement.ucrp %<>% 
  mutate( pxm.pre = 1 - qxm.pre,
          # pxm.r = 1 - qxm.r,
          
          pxT.t76     = 1 - qxt.t76 - qxd - qxm.pre - qxr.t76,                              # (1 - qxm.p) * (1 - qxt.p) * (1 - qxd.p),
          pxT.t13     = 1 - qxt.t13 - qxd - qxm.pre - qxr.t13,  
          pxT.tm13    = 1 - qxt.tm13 - qxd - qxm.pre - qxr.tm13,  
          
          pxRm        = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm.pre))), # prob of surviving up to r.max, mortality only
          px_r.full_m = order_by(-age, cumprod(ifelse(age >= r.full, 1, pxm.pre)))
          
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
  ) %>% 
  sapply(function(x){x[is.na(x)] <- 0; return(x)}) %>% data.frame # just for safety

















