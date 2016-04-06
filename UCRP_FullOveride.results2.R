





#### measure of contribution volatility ####
#**********************************************************

# What presures the political system?
# 1. ERC rate rises unexpectedly in a relatively short period of time. This threats budget planning.
# 2. ERC rate rises to a very high level, even through a relatively long period of time, that is unaffordable to the plan sponsor. 
#    This threats the affordability and may cause benefit cuts, tax increase, and crowding out expenditure on other public services.   

# Questions we may want to ask plan sponsor/policymakers:
# How big is the rise in ERC rate in a short period time that can threat budget planning?
# How big is the ERC rate that can threat the affordability?


# General measure of volatility
# - standard devation of year-to-year change in

# Measures of sharp rise of ERC rate in a short time period
# - max 5-year  change in ERC rate in each simulation, median over all simulations
# - max 10-year change in ERC rate in each simulation, median over all simulations
# - max deviation from 5/10 year moving average. For ERC rate and ERC
# - probability(over all simulations) of ERC rate rising by a% in 5/10 years, through year x  


# Measure of high ERC/ERC rate level
# - probability of ERC exceeding 2*NC in any of the years through year x
# - probability of ERC rate exceeding a% in any of the years through year x


make_lContVol <- function(runname, folderName){
  
# runname <- "C.ADC_r7.25"
# folderName <- "FullOverride/Results/"
fileName <- paste0(folderName,"results_", runname, ".RData") 
load(fileName)

results.stch <- penSim_results %>% filter(sim != 0)


qts <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# 5/10 year max changes
maxChg.ERC_PR <- results.stch %>% 
  group_by(sim) %>%
  mutate(maxChg5y   = roll_maxChg(ERC_PR, max, 5),
         maxChg10y  = roll_maxChg(ERC_PR, max, 10)) %>% 
         #maxChg5y2   = ERC_PR - lag(ERC_PR, 4),
         #maxChg10y2  = ERC_PR - lag(ERC_PR, 9)) %>% 
  summarise(maxChg5y  = max(maxChg5y, na.rm = TRUE),
            maxChg10y = max(maxChg10y, na.rm = TRUE))
            #maxChg5y2  = max(maxChg5y2, na.rm = TRUE),
            #maxChg10y2 = max(maxChg10y2, na.rm = TRUE))


maxChg.ERC_PR %>% sapply(quantile, probs = qts) %>% data.frame %>% select(-sim)


# max devation from 5/10 year moving average over 30 years,  
max.dMA.ERC_PR <- results.stch %>% 
  group_by(sim) %>% 
  mutate(MA5y   = rollapply(ERC_PR, c(1:5, rep(5, length(ERC_PR)-5)),    mean, align = "right", fill = NA),
         MA10y  = rollapply(ERC_PR, c(1:10, rep(10, length(ERC_PR)-10)), mean, align = "right", fill = NA),
         dMA5y  = ERC_PR - MA5y,
         dMA10y = ERC_PR - MA10y
  ) %>% 
  summarise(max.dMA5y  = max(dMA5y, na.rm = T),
            max.dMA10y = max(dMA10y, na.rm = T))


max.dMA.ERC_PR %>% sapply(quantile, probs = qts) %>% data.frame %>% select(-sim)


# - probability(over all simulations) of ERC rate rising by over 50%/100% in 5/10 years, through year 30 
#   Notes:
#    - suffix "2": In any 5/10 year period, calculate pct change in ERC rate between year 1 and year 5/10.

prob_chgPct <- results.stch %>% 
  group_by(sim) %>% 
  mutate(Chg5y2.pct    = 100 * (ERC_PR / lag(ERC_PR, 5) - 1),
         Chg50pct5y2   = Chg5y2.pct >= 50,
         Chg100pct5y2  = Chg5y2.pct >= 100) %>% 
  summarise(Chg50pct5y2  = any(Chg50pct5y2, na.rm = T),
            Chg100pct5y2 = any(Chg100pct5y2, na.rm = T)) %>% 
  summarise(Chg50pct5y2  = 100 * sum(Chg50pct5y2)/n(),
            Chg100pct5y2 = 100 * sum(Chg100pct5y2)/n())



# Measure of high ERC/ERC rate level
# - probability of ERC exceeding 2*NC in any of the years through year x
# - probability of ERC rate exceeding a% in any of the years through year x

df_highERC <- results.stch %>% 
  group_by(sim) %>% 
  mutate(ERC_2NC  = ERC >= 2 * NC,
         ERC_PR30 = ERC_PR >= 30,
         ADC_3NC  = ADC >= 3* NC,
         ADC_PR50 = 100*ADC/PR >= 50) %>% 
  select(sim, year, ERC_2NC, ERC_PR30, ADC_3NC, ADC_PR50)



fn <- function(y){
  df_highERC %>% group_by(sim) %>% 
    filter(year <= y ) %>% 
    summarise_each(funs(any), -sim, - year) %>% 
    summarise_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
    mutate(year = y)
}


prob_highERC <- t(sapply(2015:2044, fn)) %>% data.frame %>% select(year, everything())



# maxChg.ERC_PR
# max.dMA.ERC_PR 
# 
# prob_chgPct
# prob_highERC


assign(paste0("lContVol_", runname), 
       list(maxChg.ERC_PR  = maxChg.ERC_PR,  
            maxChg.ERC_PR  = maxChg.ERC_PR,   
            max.dMA.ERC_PR = max.dMA.ERC_PR,  
            prob_chgPct    = prob_chgPct,  
            prob_highERC   = prob_highERC)
)

do.call(save, list(paste0("lContVol_", runname), file=paste0(folderName, "lContVol_", runname, ".RData")))

}


runname.list <- runList$runname
folderName <- "FullOverride/Results/"
for(runname in runname.list) make_lContVol(runname, folderName)



load_lContVol <- function(runname){
  fn <- paste0(folderName, "lContVol_", runname, ".RData")
  load(fn, envir = globalenv())}



load_lContVol("C.ADC_r7.25")
load_lContVol("C.Cap_r7.25")
load_lContVol("C.ADC_r5.25")
load_lContVol("C.Cap_r5.25")



lContVol_C.Cap_r7.25$maxChg.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)
lContVol_C.ADC_r7.25$maxChg.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)

lContVol_C.Cap_r5.25$maxChg.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)
lContVol_C.ADC_r5.25$maxChg.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)



lContVol_C.Cap_r7.25$max.dMA.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)
lContVol_C.ADC_r7.25$max.dMA.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)

lContVol_C.Cap_r5.25$max.dMA.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)
lContVol_C.ADC_r5.25$max.dMA.ERC_PR %>% sapply(quantile, probs = qts)  %>% data.frame %>% select(-sim) %>% kable(digit = 2)



lContVol_C.Cap_r7.25$prob_chgPct %>% kable(digit = 2)
lContVol_C.ADC_r7.25$prob_chgPct %>% kable(digit = 2)

lContVol_C.Cap_r5.25$prob_chgPct %>% kable(digit = 2)
lContVol_C.ADC_r5.25$prob_chgPct %>% kable(digit = 2)
  


lContVol_C.Cap_r7.25$prob_highERC %>% kable(digit = 2)
lContVol_C.ADC_r7.25$prob_highERC %>% kable(digit = 2)

lContVol_C.Cap_r5.25$prob_highERC %>% kable(digit = 2)
lContVol_C.ADC_r5.25$prob_highERC %>% kable(digit = 2)
















# runname <- "C.ADC_r9.25"
# folderName <- "FullOverride/Results/"
# load(paste0(folderName,"results_", runname, ".RData") )
# 
# get_quantiles(unique(penSim_results$runname), "FR.MA", data = penSim_results, year.max = Inf)
# 
# df_geoReturn <- penSim_results %>% filter(sim != 0) %>% group_by(sim) %>% 
#   summarise(geoReturn = get_geoReturn(i.r))
# 
# df_geoReturn$geoReturn %>% quantile(c(0, 0.1, 0.25， 0.5))
# # Even with mean return = 9.97%, the worst 10% simulations have 30-year compound returns less than 6.5%. which is 
# # insufficient to bring them to full funding.  



