# This script calculate aggregate annual ALs, NCs and benefit payments for UCRP.



get_AggLiab <- function( 
                         .init_beneficiaries,
                         .liab,
                         .liab.ca,
                         .pop,
                         
                         .paramlist = paramlist,
                         .Global_paramlist = Global_paramlist){

  # This function calculates total AL, NC and benefits.
  
  
  # Run the section below when developing new features.  
     # .liab   = liab
     # .pop    = pop
     # .init_beneficiaries
     # .paramlist = paramlist
     # .Global_paramlist = Global_paramlist
  
   assign_parmsList(.Global_paramlist, envir = environment())
   assign_parmsList(.paramlist,        envir = environment())
  
  
  
  #*************************************************************************************************************
  #                                     ## Liabilities and NCs for actives   ####
  #*************************************************************************************************************
  
  # Join population data frames and liability data frames. 
  .liab$active <- left_join(.pop$active, .liab$active) # %>% left_join(new_retirees)
  .liab$active[-(1:3)] <- colwise(na2zero)(.liab$active[-(1:3)]) # replace NAs with 0, so summation involing missing values will not produce NAs. 
  
  .liab$active %<>%  
    mutate(ALx.laca.tot = ALx.laca * number.a,
           ALx.LSC.tot  = ALx.LSC  * number.a,
           ALx.v.tot    = ALx.v    * number.a,
           ALx.av.tot   = ALx.laca.tot + ALx.LSC.tot + ALx.v.tot,
           
           NCx.laca.tot = NCx.laca * number.a,
           NCx.LSC.tot  = NCx.LSC  * number.a,
           NCx.v.tot    = NCx.v    * number.a,
           NCx.av.tot   = NCx.laca.tot + NCx.LSC.tot + NCx.v.tot,
           
           PVFBx.laca.tot = PVFBx.laca * number.a,
           PVFBx.LSC.tot  = PVFBx.LSC  * number.a,
           PVFBx.v.tot    = PVFBx.v    * number.a,
           PVFBx.av.tot   = PVFBx.laca.tot + PVFBx.LSC.tot + PVFBx.v.tot,
           
           PR.tot  = sx * number.a,
           
           runname = runname)
  
  active.agg <- .liab$active %>%  
    group_by(year) %>% 
    summarise(
      ALx.laca.sum = sum(ALx.laca.tot, na.rm = TRUE),
      ALx.LSC.sum  = sum(ALx.LSC.tot,  na.rm = TRUE),
      ALx.v.sum    = sum(ALx.v.tot,    na.rm = TRUE),
      ALx.av.sum   = sum(ALx.av.tot,   na.rm = TRUE), 
      
      NCx.laca.sum = sum(NCx.laca.tot, na.rm = TRUE),
      NCx.LSC.sum  = sum(NCx.LSC.tot,  na.rm = TRUE),
      NCx.v.sum    = sum(NCx.v.tot,    na.rm = TRUE),
      NCx.av.sum   = sum(NCx.av.tot,   na.rm = TRUE),
      
      PVFBx.laca.sum = sum(PVFBx.laca.tot, na.rm = TRUE),
      PVFBx.LSC.sum  = sum(PVFBx.LSC.tot,  na.rm = TRUE),
      PVFBx.v.sum    = sum(PVFBx.v.tot,    na.rm = TRUE),
      PVFBx.av.sum   = sum(PVFBx.av.tot,   na.rm = TRUE),
      
      
      PR.sum    = sum(PR.tot,  na.rm = TRUE),
      
      nactives  = sum(number.a,  na.rm = TRUE)) %>% 
      # mutate(runname  = runname) %>% 
      as.matrix # extracting elements from matrices is much faster than from data.frame
  
  

  
  #*************************************************************************************************************
  #                                     ## Liabilities and benefits for retirees   ####
  #*************************************************************************************************************
  
  .liab$la  <- data.table(.liab$la, key = "ea,age,year,year.r")
  .pop$la   <- data.table(.pop$la,  key = "ea,age,year,year.r")
  .liab$la  <- merge(.pop$la, .liab$la, by = c("ea", "age","year", "year.r"), all.x = TRUE)
  .liab$la  <- as.data.frame(.liab$la)
  
  
  .liab$la %<>% 
    mutate(ALx.la.tot = ALx.la * number.la,
           B.la.tot   = B.la   * number.la,
           runname = runname)
  
  la.agg <- .liab$la %>% 
    group_by(year) %>% 
    summarise(ALx.la.sum   = sum(ALx.la.tot, na.rm = TRUE),
              B.la.sum     = sum(B.la.tot  , na.rm = TRUE),
              nla          = sum(number.la , na.rm = TRUE)) %>% 
    # mutate(runname = runname) %>% 
    as.matrix
  
  
  
  #*************************************************************************************************************
  #                                 ## Total LSC amounts                                                  ####
  #*************************************************************************************************************  
  LSC.agg <- expand.grid(year = init.year:(init.year + nyear - 1), age.r = range_age.r, ea = range_ea) %>% 
    filter(age.r > ea) %>% 
    left_join(.liab$liab.LSC %>% select(year, ea, age.r = age, Bx.LSC, ALx.LSC)) %>% 
    left_join(.pop$LSC.ca %>% select(year, ea, age.r = age, new_LSC)) %>% 
    mutate(B.LSC.sum = new_LSC * Bx.LSC,
           ALx.LSC.sum = new_LSC * ALx.LSC) %>% 
    group_by(year) %>% 
    summarise(B.LSC.sum   = sum(B.LSC.sum,   na.rm = TRUE),
              ALx.LSC.sum = sum(ALx.LSC.sum, na.rm = TRUE),
              n.LSC     = sum(new_LSC, na.rm = TRUE)) %>% 
    as.matrix
  
  
  
  #*************************************************************************************************************
  #                                 ## Liabilities and benefits for vested terms.   ####
  #*************************************************************************************************************

  # Save 10 seconds by using data.table to merge. Update 2/2016: the latest version of left_join is fast enough.
  .liab$term <- left_join(.pop$term, .liab$term)
  # .liab$term  <- data.table(.liab$term, key = "ea,age,year,year.term")
  # .pop$term   <- data.table(.pop$term,  key = "ea,age,year,year.term")
  # .liab$term  <- merge(.pop$term, .liab$term, by = c("ea", "age","year", "year.term"), all.x = TRUE)
  # .liab$term  <- as.data.frame(.liab$term)
  
  #.liab$term %>% filter(year == 2015, year.term == 2014, age == 40)
  
  
  
  .liab$term %<>% 
    mutate(ALx.v.tot = ALx.v * number.v,
           B.v.tot   = B.v  * number.v,
           runname = runname)
  
  
  term.agg <- .liab$term %>% 
    group_by(year) %>% 
    summarise(ALx.v.sum   = sum(ALx.v.tot, na.rm = TRUE),
              B.v.sum     = sum(B.v.tot  , na.rm = TRUE),
              nterms      = sum(number.v , na.rm = TRUE)) %>% 
    # mutate(runname = runname) %>% 
    as.matrix
 

  #*************************************************************************************************************
  #                                 ## Liabilities and benefits for contingent annuitants and survivors   ####
  #*************************************************************************************************************  
  
  # Initial beneficiaries
   # Assumptions about the retirement ages of initial beneficiaries:
   # age.r = 60, if age in 2015 is greater than or equal to 60.
   # age.r = age in 2015,  if age in 2015 is smaller than 60. 

  .init_beneficiaries %<>% mutate(init.age = age, age.r = ifelse(age >= 60, 60, age), year = init.year, age = NULL)
  
  init.ca.agg <- expand.grid(init.age = unique(.init_beneficiaries$init.age), age = r.min:max.age) %>%
                 filter(age >= init.age) %>% 
                 left_join(.init_beneficiaries) %>% 
                 group_by(init.age) %>% arrange(init.age, age) %>% 
                 mutate(age.r = age.r[age == init.age]) %>% 
                 left_join(select(mortality.post.ucrp, age, age.r, ax.r.W.ben = ax.r.W, pxm.post.W)) %>% 
                 mutate(year   =  init.year + age -  init.age,
                        n.R0S1 = n.R0S1 * cumprod(ifelse(age == init.age, 1, lag(pxm.post.W))),
                        B.ca   = benefit * (1 + cola)^(age - init.age),
                        B.ca.sum = B.ca * n.R0S1,
                        liab.ca  = B.ca * ax.r.W.ben,
                        liab.ca.sum = liab.ca * n.R0S1
                        ) %>% 
                 group_by(year) %>% 
                 summarise(n.R0S1.init = sum(n.R0S1, na.rm = TRUE),
                           B.ca.sum.init = sum(B.ca.sum, na.rm = TRUE),
                           liab.ca.sum.init = sum(liab.ca.sum, na.rm = TRUE)) %>% 
                 filter(year %in% init.year:(init.year + nyear - 1))
    
  # init.ca.agg
  

  ca.agg <- expand.grid(year.r = init.year:(init.year + nyear - 1), age.r = range_age.r, ea = range_ea, age = range_age) %>% 
            mutate(year = year.r + age - age.r) %>% 
             filter(age >= ea,
                   age >= age.r,
                   age.r > ea,
                   year <= max(year.r)) %>%

            left_join(.liab$active %>% filter(age %in% range_age.r) %>% select(year.r = year, ea, age.r = age, Bx.laca)) %>% 
            left_join(.pop$LSC.ca  %>% select(year.r = year, ea, age.r = age, new_ca)) %>% 
            left_join(.liab.ca) %>% 
            mutate(new_ca = na2zero(new_ca),
                   liab.ca.sum = new_ca * Bx.laca * liab.ca.sum.1,
                   B.ca.sum    = new_ca * Bx.laca * B.ca.sum.1,
                   n.R1        = new_ca * (n.R1S0.1 + n.R1S1.1), # Total number of living contingent annuitants
                   n.R0S1      = new_ca * n.R0S1.1) %>%          # Total number of survivors
                   #filter(year.r == 2025, age.r == 50, ea == 20) %>%            
            group_by(year) %>% 
            summarise(liab.ca.sum = sum(liab.ca.sum, na.rm = TRUE),
                      B.ca.sum    = sum(B.ca.sum, na.rm = TRUE),
                      n.R1        = sum(n.R1, na.rm = TRUE),
                      n.R0S1      = sum(n.R0S1, na.rm = TRUE),
                      n.new_ca    = sum(new_ca, na.rm = TRUE)) 
  
 
  
   # Combine the initial beneficiaries and new beneficiaries
   ca.agg %<>% left_join(init.ca.agg) %>% 
              colwise(na2zero)() %>% 
              mutate(n.R0S1   = n.R0S1 + n.R0S1.init,
                     B.ca.sum = B.ca.sum + B.ca.sum.init,
                     liab.ca.sum = liab.ca.sum + liab.ca.sum.init) %>% 
              as.matrix
  
  
  # 
  # LSC.agg %>% data.frame
  # ca.agg %>% data.frame
  # active.agg %>% data.frame
  
  #return(
  AggLiab <-  list(active = active.agg, 
                   la     = la.agg,
                   ca     = ca.agg, 
                   term   = term.agg,
                   LSC    = LSC.agg)
              
              # ind_active  = if(paramlist$save.indiv) .liab$active  else "Not saved", 
              # ind_retiree = if(paramlist$save.indiv) .liab$retiree else "Not saved",
              # ind_term    = if(paramlist$save.indiv) .liab$term    else "Not saved")
    
    #)
}




#*************************************************************************************************************
#                                     ## Summing up tier values to get total values of a plan   ####
#*************************************************************************************************************

get_AggLiab_sumTiers <- function(...){
 # This function create list of aggregate values of a plan from list of tiers. 
 # ... :  a series data list of tiers.   
  
 #  AggLiab.list <- list(AggLiab.t76, AggLiab.t13, AggLiab.tm13)
  
  AggLiab.list <- list(...)
  
  AggLiab.list %<>% lapply( function(List) lapply(List, as.data.frame)) 

  nTiers <- length(AggLiab.list)
  nTypes <- length(AggLiab.list[[1]])
  TypeNames <- names(AggLiab.list[[1]])
  
  AggLiab.list2 <- vector("list", nTypes)
  names(AggLiab.list2) <- TypeNames
  
  for (j in TypeNames){
    AggLiab.list2[[j]] <- lapply(AggLiab.list, function(df){df[[j]]}) 
  }
  
  sum_tiers <- function(df){ df %<>% group_by(year) %>% 
      summarise_each(funs(sum))
  } 
  
  AggLiab_sumTiers <- AggLiab.list2 %>% 
                      lapply(bind_rows) %>% 
                      lapply(sum_tiers) %>% 
                      lapply(as.matrix)
  
  return(AggLiab_sumTiers)
}
  
  
  
  






  
  

# start_time_prep_loop <-  proc.time()
# 
# AggLiab <- get_AggLiab()
# 
# end_time_prep_loop <-  proc.time()
# Time_prep_loop <- end_time_prep_loop - start_time_prep_loop


