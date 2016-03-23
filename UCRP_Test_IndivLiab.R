# This script test the modeling approach for lump sum cash flow in UCRP.



# Notes on LSC in UCRP

# The LSC is modeled as an new type of benefit.
# The probability of opting for LSC upon retirement depends on yos and member type (active, inactive, disability crossovers.) 
# The amount of LSC is cacluated as the discount value of future retirement benefit payments with the following assumptions:
#   1. discount rate 7.25% (starting from 2015)
#   2. COLA is assumed to be 2%. 
#   3. uni-sex mortality table, weighted 40% male and 60% female. 

# All calculations can be done in liab.active.   




get_indivLab <- function(.decrement.ucrp,
                         .salary,
                         .benefit,
                         .bfactor,
                         .init_terminated,
                         .Tier_select,
                         # parameter for all tiers
                         # mortality.post.ucrp,
                         .paramlist = paramlist,
                         .Global_paramlist = Global_paramlist){

# Inputs
 # decrement.ucrp
 # salary
 # benefit
 # bfactor
 # mortality.post.ucrp
 # paramlist
 # init.term
 # Global_paramlist


assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(.paramlist,        envir = environment())


#*************************************************************************************************************
#                               1. Preparation                        #####                  
#*************************************************************************************************************
# Starts with a simple case with only 2 entry ages: 20 and 74

min.year <- min(init.year - (max.age - (r.max - 1)), init.year - (r.max - 1 - min.ea))
## Track down to the year that is the smaller one of the two below: 
# the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
# the year a r.max year old active in year 1 enter the workforce at age min.ea 




liab.active <- expand.grid(start.year = min.year:(init.year + nyear - 1) , 
                           ea = range_ea, age = range_age) %>%
  filter(start.year + max.age - ea >= init.year, age >= ea) %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>% 
  left_join(.salary) %>%
# left_join(.benefit) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  left_join(.decrement.ucrp) %>% 
  left_join(.bfactor) %>%
  left_join(mortality.post.ucrp %>% filter(age == age.r) %>% select(age, ax.r.W)) %>%
  group_by(start.year, ea) %>%
  
  
  # # Chnange variable names for 1976 tier
  # rename(pxT = pxT.t76,
  #        qxr.la = qxr.la.t76,
  #        qxr.LSC  = qxr.LSC.t76,
  #        bfactor = bf.non13) %>% 
  
  # Calculate salary and benefits
  mutate(
    sx_cap = ifelse(year<=2015, Inf, ifelse(start.year < 1994, 265000, 395000) * (1 + infl)^(year - 2015)),  # Compensation limit, see AV 2015 p50.
    sx     = pmin(sx, sx_cap),
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    fas= fas - 133 * 12 * (.Tier_select == "t76"),      # benefit rule for different tiers
    COLA.scale = (1 + cola)^(age - min(age)),          # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    Bx = pmin(fas, na2zero(bfactor * yos * fas)),      # accrued benefits, note that only Bx for ages above r.min are necessary under EAN.
    bx = lead(Bx) - Bx,                                # benefit accrual at age x

    
    # ax = get_tla(pxm, i, COLA.scale),                  # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    # ax.r = get_tla(pxm.r, i, COLA.scale),              # ax calculated with mortality table for retirees. 
    
    ax.LSC = get_tla(1 - qxm.LSC, i, COLA.scale),        # ax calculated with mortality table for LSC. When multiplied by Bx, it is the LSC amount.
    
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i, sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    #   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    #   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    axr = ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i), rep(0, max.age - r.full + 1))),                 # Similar to axR, but based on r.full.  For calculation of term benefits when costs are spread up to r.min.        
    axrs= ifelse(ea >= r.full, 0, c(get_tla(pxT[age < r.full], i, sx[age<r.full]), rep(0, max.age - r.full + 1))),  # Similar to axRs, but based on r.full. For calculation of term benefits when costs are spread up to r.min.
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )


# liab.active %>% select(start.year, ea, age, year, fas, b)
#                 #filter(start.year == 2015 , ea == 70) %>% 
#                 mutate( COLA.scale = (1 + cola)^(age - min(age)), 
#                 ax.LSC = get_tla(1 - qxm.LSC, i, COLA.scale))
# 
# 

#*************************************************************************************************************
#                        2.1  AL and NC of life annuity and contingent annuity for active                #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.laca = ifelse(yos < r.yos, 0,  
                        ifelse(age > r.full, 1, ifelse(age %in% r.min:r.full, 1, 0))),         # eligibility rule 1
          gx.laca = ifelse(start.year >= 1989, gx.laca, ifelse(age >= 62, 1, gx.laca)),              # eligibility rule 2
          gx.LSC  = gx.laca,
          #gx.laca = 0,
  Bx.laca  = gx.laca * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
  TCx.laca = lead(Bx.laca) * (qxr.la + qxr.ca)  * lead(ax.r.W) * v,  # term cost of retirement at the internal retirement age x (start to claim benefit at age x + 1)
  # TCx.r = Bx.r * qxr.a * ax,
  PVFBx.laca  = c(get_PVFB(pxT[age <= r.max], v, TCx.laca[age <= r.max]), rep(0, max.age - r.max)),
  
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
  
  # NC and AL of EAN.CD
  NCx.EAN.CD.laca = ifelse(age < r.max, PVFBx.laca[age == min(age)]/ayx[age == r.max], 0),
  ALx.EAN.CD.laca = PVFBx.laca - NCx.EAN.CD.laca * axR,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.laca   = ifelse(age < r.max, sx * PVFBx.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
  PVFNC.EAN.CP.laca = NCx.EAN.CP.laca * axRs,
  ALx.EAN.CP.laca   = PVFBx.laca - PVFNC.EAN.CP.laca
  ) 

#*************************************************************************************************************
#                       2.2   AL and benefit for retirees with life annuity                        #####                  
#*************************************************************************************************************





# Calculate AL and benefit payment for retirees having retired at different ages.
liab.la <- rbind(
  # grids for initial retirees in year 1
    # To simplified the calculation, it is assmed that all initial retirees/beneficiaries entered the workforce at age r.min - 1 and 
    # retiree right in year 1. This assumption will cause the retirement age and yos of some of the retirees not compatible with the eligiblility rules,
    # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial retirees/beneficiaries.
  expand.grid(ea         = r.min - 1,
              age.r      = .benefit$age, # This ensures that year of retirement is year 1.
              start.year = init.year - (.benefit$age - (r.min - 1)),
              age        = range_age[range_age >= r.min]) %>%
    filter(age >= ea + 1 - start.year),

  # grids for who retire after year 1.
  expand.grid(ea         = range_ea[range_ea < r.max],
              age.r      = r.min:r.max,
              start.year = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age        = range_age[range_age >=r.min]) %>%
    filter(age   >= ea,
           age.r >= ea,
           age   >= age.r,
           start.year + (age.r - ea) >= init.year + 1, # retire after year 2
           start.year + age - ea     >= init.year + 1)
) %>%
  data.table(key = "start.year,ea,age.r,age")
 

liab.la <- liab.la[!duplicated(liab.la %>% select(start.year, ea, age, age.r ))]
 
 
liab.la <- merge(liab.la,
                      select(liab.active, start.year, ea, age, Bx.laca, COLA.scale, gx.laca) %>% data.table(key = "ea,age,start.year"),
                      all.x = TRUE, by = c("ea", "age","start.year")) %>%
                arrange(start.year, ea, age.r) %>% 
                as.data.frame %>% 
                left_join(select(mortality.post.ucrp, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
                left_join(.benefit)


liab.la %<>% as.data.frame  %>% # filter(start.year == -41, ea == 21, age.retire == 65) %>%
  # filter(ea == 54) %>%
  group_by(start.year, ea, age.r) %>%
  mutate(
    year   = start.year + age - ea,
    year.r = start.year + age.r - ea,
    Bx.laca  = ifelse(is.na(Bx.laca), 0, Bx.laca),
    B.la   = ifelse(year.r <= init.year,
                    benefit[year == init.year] * COLA.scale / COLA.scale[year == init.year],      # Benefits for initial retirees
                    Bx.laca[age == age.r] * COLA.scale / COLA.scale[age == age.r]),               # Benefits for retirees after year 1
    ALx.la = B.la * ax.r.W.ret                                                                    # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA

  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.r, age.r, start.year, B.la, ALx.la) %>% 
  arrange(age.r, start.year, ea, age)


# Spot check
liab.la %>% filter(start.year == 2016, ea == 49, age.r == 60) %>% as.data.frame()



#*************************************************************************************************************
#                       3.1   AL and NC of LSC for actives                                         #####                  
#*************************************************************************************************************

 liab.active %<>%   
  mutate( 
          # gx.la also applies to LSC 
          #gx.LSC  = gx.laca,
          
          Bx.LSC  = gx.LSC * Bx * ax.LSC,         # This is the LSC amount if the employee CLAIMs at age x, not internally retire at age x. 
          TCx.LSC = lead(Bx.LSC) * qxr.LSC  * v,  # term cost of retirement at the internal retirement age x (start to claim benefit at age x + 1)
          PVFBx.LSC  = c(get_PVFB(pxT[age <= r.max], v, TCx.LSC[age <= r.max]), rep(0, max.age - r.max)),
          
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          
          # NC and AL of EAN.CD
          NCx.EAN.CD.LSC = ifelse(age < r.max, PVFBx.LSC[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.LSC = PVFBx.LSC - NCx.EAN.CD.LSC * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.LSC = ifelse(age < r.max, sx * PVFBx.LSC[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.LSC = NCx.EAN.CP.LSC * axRs,
          ALx.EAN.CP.LSC = PVFBx.LSC - PVFNC.EAN.CP.LSC
  ) 



#*************************************************************************************************************
#                          3.2 LSC payments                                                              #####                  
#************************************************************************************************************* 
 liab.LSC <- liab.active %>% select(start.year, year, ea, age, yos, Bx.LSC) %>% 
          mutate(ALx.LSC = Bx.LSC) %>% 
          filter(age %in% r.min:r.max)



#*************************************************************************************************************
#                         4.1 AL and NC of deferred benefits for actives                        #####
#*************************************************************************************************************

# Calculate normal costs and liabilities of deferred retirement benefits
# Vested terms begin to receive deferred retirement benefit at r.full.
# Notes on deferred retirement benefits for vested terms.
# 1. Note that the PVFB and AL are different at age r.min - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
#    at age r.max
# 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
#    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the
#    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balanced.
#
# CAUTION!: There will be a problem if actives entering after r.min can get vested, when PVFB is only amortized up to age r.min

liab.active %<>%
  mutate(gx.v = ifelse(yos >= v.yos, 1, 0),                                     # actives become vested after reaching v.yos years of yos
         gx.v = ifelse(start.year >= 1989, gx.v, ifelse(age >= 62, 1, gx.v)),   # eligibility rule 2
         
         Bx.v = ifelse(ea < r.full, 
                       gx.v * pmin(fas, na2zero(bfactor[age == r.full] * yos * fas))* (1 + ifelse(.Tier_select == "t76", infl, 0))^(r.full - age), 
                       0), # initial annuity amount when the vested term retires at age r.full. Accrued benefit is CPI adjustd for 1976 Tier.(AV2015 p53) 

         TCx.v   = ifelse(ea < r.full, Bx.v * qxt * lead(px_r.full_m) * v^(r.full - age) * ax.r.W[age == r.full], 0),             # term cost of vested termination benefits. We assume term rates are 0 after r.full.
         PVFBx.v = ifelse(ea < r.full, c(get_PVFB(pxT[age < r.full], v, TCx.v[age < r.full]), rep(0, max.age - r.full + 1)), 0),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).

         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),

         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < r.full, PVFBx.v[age == min(age)]/ayx[age == r.full], 0), # for testing spreading NC.v up to r.full
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,

         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < r.full, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.full]) * sx, 0),  # for testing spreading NC.v up to r.full
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axrs
  ) %>%
  ungroup %>% select(start.year, year, ea, age, everything())
  
# x <- liab.active %>% filter(start.year == 1, ea == 20)



#*************************************************************************************************************
#                       4.2 AL for vested terminatede members                        #####
#*************************************************************************************************************

# # Calculate AL and benefit payment for initial vested terms.
liab.term.init <- expand.grid(ea         = unique(.init_terminated$ea),
                              age.term   = unique(.init_terminated$age.term),
                              start.year = unique(.init_terminated$start.year),
                              age = range_age) %>%
  filter(start.year + age - ea >= 1,
         age >= ea,
         age.term >= ea) %>%
  left_join(.init_terminated %>% select(ea, age.term, start.year, yos, fas = HAPC)) %>%
  left_join(select(liab.active, start.year, ea, age, bfactor, COLA.scale, pxRm, px_r.full_m)) %>%
  left_join(mortality.post.ucrp %>% filter(age.r == r.full) %>% select(age, ax.r.W.term = ax.r.W)) %>%
  group_by(start.year, ea, age.term) %>%

  mutate(
    year = start.year + age - ea,
    age.ben  = ifelse(age[year == init.year] > r.full, age[year == init.year], r.full), # Age at which term starts to receive benefit.
    year.term = year[age == age.term],

    Bx.v = pmin(fas, na2zero(bfactor[age == ifelse(age.ben <= r.full, r.full, age[age == age.ben])] * yos * fas))* (1 + infl)^(r.full - age), #
    B.v   = ifelse(age.ben > r.full, 0,   ifelse(age >= r.full, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.full], 0)),  # Benefit payment after r.full
    B.v   = ifelse(age.ben == r.full, B.v, ifelse(age >= age.ben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == age.ben], 0)),
    ALx.v = ifelse(age <  r.full, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.full] * px_r.full_m * v^(r.full - age),
                   B.v * ax.r.W.term)) %>%
  ungroup %>%
  select(ea, age, start.year, year, year.term, B.v, ALx.v) %>%
  filter(year %in% seq(init.year, len = nyear),
         year.term == init.year - 1)



# liab.term.init %>% filter(start.year == 2007, ea == 55, age.term == 63) %>% data.frame()


# Calculate AL and benefit payment for vested terms terminating at different ages.
# Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab.term <- expand.grid(start.year = (init.year - (r.full - 1 - min.age)):(init.year + nyear - 1), # 2015
                         ea = range_ea[range_ea < r.full], 
                         age = range_age, 
                         age.term = range_age[range_age < r.full]) %>% 
  filter(start.year + max.age - ea >= init.year, 
         age >= ea, age.term >= ea,
         age >= age.term) %>% # drop redundant combinations of start.year and ea.
  data.table(key = "ea,age,start.year,age.term") 


liab.term <- merge(liab.term,
                   select(liab.active, start.year, year, ea, age, Bx.v, COLA.scale, pxRm, px_r.full_m) %>% data.table(key = "ea,age,start.year"),
                   all.x = TRUE, by = c("ea", "age","start.year")) %>% as.data.frame %>% 
             left_join(mortality.post.ucrp %>% filter(age.r == r.full) %>% select(age, ax.r.W.term = ax.r.W))   # load present value of annuity for retirement age r.full

liab.term %<>% as.data.frame %>%
  group_by(start.year, ea, age.term) %>%
  mutate(year.term = year[age == age.term],

         B.v   = ifelse(age >= r.full, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.full], 0),  # Benefit payment after r.full
         ALx.v = ifelse(age <  r.full, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.full] * px_r.full_m * v^(r.full - age),
                        B.v * ax.r.W.term)

  ) %>%
  ungroup  %>%
  # select(#-start.year, -age.term,
  #        -Bx.v, -ax.r.W, -COLA.scale, -pxRm) %>%

  select(-age.term, -Bx.v, -ax.r.W.term, -COLA.scale, -pxRm, - px_r.full_m, -age.r) %>%
  filter(year %in% seq(init.year, len = nyear)) 


# liab.term %<>% mutate(B.v   = ifelse(year.term == init.year - 1, 0, B.v),
#                       ALx.v = ifelse(year.term == init.year - 1, 0, ALx.v))





liab.term <-  bind_rows(list(liab.term.init,                                  # Using rbind produces duplicated rows with unknown reasons. Use bind_rows from dplyr instead.
                             filter(liab.term, year.term != init.year - 1)))

# liab.term %>% filter(year == 2015)

# liab.term %>% filter(year.term == 2014, start.year == 1980) %>% head
# liab.term[!duplicated(liab.term %>% select(start.year, ea, age, year.term)),]
#   any(T)
#   
#*************************************************************************************************************
#                 # Choosing AL and NC variables corresponding to the chosen acturial methed             #####
#*************************************************************************************************************


ALx.laca.method   <- paste0("ALx.", actuarial_method, ".laca")
NCx.laca.method   <- paste0("NCx.", actuarial_method, ".laca")

ALx.v.method <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method <- paste0("NCx.", actuarial_method, ".v")

ALx.LSC.method <- paste0("ALx.", actuarial_method, ".LSC")
NCx.LSC.method <- paste0("NCx.", actuarial_method, ".LSC")



var.names <- c("sx", ALx.laca.method, NCx.laca.method, ALx.v.method, NCx.v.method, ALx.LSC.method, NCx.LSC.method, "PVFBx.laca", "PVFBx.LSC", "PVFBx.v", "Bx.laca", "Bx.LSC")
liab.active %<>% 
  filter(year %in% seq(init.year, len = nyear)) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx.laca"  = ALx.laca.method,  "NCx.laca"   = NCx.laca.method, 
          "ALx.v"   = ALx.v.method,   "NCx.v"    = NCx.v.method,
          "ALx.LSC" = ALx.LSC.method, "NCx.LSC"  = NCx.LSC.method) # Note that dplyr::rename_ is used. 



## Final outputs
  # liab.active
  # liab.la
  # liab.term
  # B.LSC

liab <- list(active = liab.active, la = liab.la, term = liab.term, liab.LSC = liab.LSC)

}


# liab <- get_indivLab(decrement.ucrp,
#                      salary,
#                      benefit,
#                      bfactor,
#                      init_terminated.t76)


