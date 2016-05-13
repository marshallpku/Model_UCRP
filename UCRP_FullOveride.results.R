## Looking at results of the full override models and making graphs and tables for discussion with UCRP
## 3/31/2016
## Yimeng Yin


## For each funding policy, creat following tables and graphs

 # Finance table for deterministic run, by year
   # AL, MA, AA, B, C, ERC, netxcf, invest inc, FR%, ERC%payroll, netxcf%MA, apratio, (netxcf%payroll)  
   
 # Graphs for deterministic run
   # g1: ERC in $m
   # g2: ERC%payroll 
   # g3: FR%

 # Graphs for stochastic runs
   # g1: 4 individual runs: 2 with compound return at DC, 1 at 25%-tile and 1 at 75%-tile 
   # g2: prob of FR below 40% overtime
   # g3: prob of FR above 95% overtime
   # g4: max 5-year change in ERC overtime
   # t:  table for prob FR<40%, FR>95, and 5-year max change in ERC



make_lresults <- function(runname, folderName){

#runname <- "RS1.Cap"
#folderName <- "FullOverride/Results/"
fileName <- paste0(folderName,"results_", runname, ".RData") 
load(fileName)

results.stch <- penSim_results %>% filter(sim != 0)
results.det  <- penSim_results %>% filter(sim == 0) # %>% select(year, sim, ERC, ERC_PR, FR.MA)



#### Deterministic Run ####
#**********************************************************


# table for deterministic run 
make_tbl.det <- function(df_results){
 var.tbl.det <- c("runname","year", "AL", "MA", "AA", "B", "C", "EEC", "ERC", "extFund", "netxcf", "FR.MA", 
                  "ERC_PR", "ERCwSTIP", "ERCwSTIP_PR", "netxcf_MA", "netxcf_PR", "apratio") 

  df_results %<>% mutate(netxcf = C - B,
                         netxcf_MA = 100 * netxcf / MA,
                         netxcf_PR = 100 * netxcf / PR,
                         apratio   = MA/PR) %>% 
                  filter(sim == 0)

  tbl.det <- df_results %>% select(one_of(var.tbl.det))
              
}

tbl.det <-  make_tbl.det(results.det) 
tbl.det %>% kable(digits = 3)



# Graphs for deterministic run
 # g1: ERC in $m
 # g2: ERC%payroll 
 # g3: FR%

# function to create basic line plot
get_lineplot <- function(df, x, y){
    ggplot(df, aes_string(x = x, y = y)) + theme_bw() + 
    geom_line() + geom_point() + 
    scale_x_continuous(breaks = seq(2015, 2045, 5))
}


g.det.ERC <- results.det %>% mutate(ERC.UC = (ERCwSTIP)/1e6) %>% 
        get_lineplot("year", "ERC.UC") + 
        labs(title = "UC employer contribution\n  ", x = "Year", y = "Contribution ($million)")
# can add ADC to the plot
                        
                
g.det.ERC_PR <- results.det %>% get_lineplot("year", "ERCwSTIP_PR") + 
                labs(title = "UC employer contribution \nas % of payroll", x = "Year", y = "Percent")


g.det.FR <- (results.det %>% get_lineplot("year", "FR.MA")) + 
                geom_hline(yintercept = 100, linetype = 2, color = "red") + 
                scale_y_continuous(breaks = seq(0, 500, 5)) +
                coord_cartesian(ylim = c(70, 110)) + 
                labs(title = "Funded ratio \nbased on market asset value", x = "Year", y = "Percent")


g.det.CvADCmed <- results.det %>% select(year,C, ADC) %>% gather(var, value, -year) %>% 
  ggplot(aes(x = year, y = value/1e6, color = var)) + theme_bw() + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_color_manual(values = c("red","darkblue"), name = "", 
                     label = c("ADC", "Contribution")) + 
  labs(title = "Total contribution (including STIP) and ADC",
       x = "Year", y = "$million")


g.det.CvADC_PRmed <- results.det %>% select(year, C_PR, ADC_PR) %>% gather(var, value, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) +
  scale_color_manual(values = c("red","darkblue"), name = "", 
                     label = c("ADC", "Contribution")) + 
  labs(title = "Total contribution rate (including STIP) and ADC as % payroll",
       x = "Year", y = "Percentage of payroll")



g.det.ERC
g.det.ERC_PR
g.det.FR


#### Stochastic Runs: individual runs ####
#***********************************************************************



DC <- results.stch$i[1]
FR.MA.year1 <- results.stch$FR.MA[1]



# distribution of compound return

g.distReturn <- results.stch %>% group_by(sim) %>% 
   summarize(geoReturn = get_geoReturn(i.r)) %>% 
   ggplot(aes(100*geoReturn)) + theme_bw() + 
   geom_histogram(color = "black", fill = "blue", binwidth = 0.5, boundary = 0) + 
   geom_vline(xintercept = DC * 100, color = "red") + 
   scale_x_continuous(breaks = seq(0,20,1))+
   labs(title = "Distribution of 30-year compound annual return over 2000 simulations",
        x = "%")

g.distReturn


# FR% and ERC% in 4 indiv runs
df_geoReturn <-  results.stch %>% group_by(sim) %>%
                 summarize(geoReturn = get_geoReturn(i.r)) %>% 
                 arrange(geoReturn) %>% 
                 mutate(order.ir = 1:n()) %>% 
                 mutate(dist2DC = abs(geoReturn - DC)) %>% 
                 arrange(dist2DC) %>%
                 mutate(order.dist = 1:n())
      
         
# looking for runs with geo return at around 0.725%, 25th-tile and 75th-tile  
df_geoReturn$geoReturn %>% quantile
df_indiv_selcet <- df_geoReturn %>% 
                   filter(order.ir == round(0.25*n()) |
                          order.ir == round(0.75*n()) | 
                          order.dist %in% 1:2) %>% 
                   arrange(geoReturn)

#(geoReturn >= 0.0724& geoReturn <= 0.0726))
df_indiv_selcet

# selecting runs
results_indiv <- results.stch %>% filter(sim %in% df_indiv_selcet$sim) %>% 
                 left_join(df_indiv_selcet %>% select(sim, order.ir, geoReturn)) %>% 
                 mutate(plot.label = paste0(round(100*geoReturn, digits = 2), "%"))

results_indiv$plot.label
df_indiv_selcet

# Creating graphs
plot.label <- paste0(round(100*df_indiv_selcet$geoReturn, digits = 2), "%")


g.ind.FR <- results_indiv %>% 
  ggplot(aes(x = year, y = FR.MA, color = factor(order.ir), label = plot.label)) + theme_bw() + 
  geom_line(linetype = 1) + geom_point() +
  geom_hline(yintercept = c(100, FR.MA.year1), linetype = 2, color = c("red", "black") ) +
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = c(seq(0, 500, 10))) + 
  scale_color_manual(values = c("red","deepskyblue","dodgerblue", "green3"),
                     label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
  labs(title = "Funded ratio of selected individual simulations",
             x = "Year", y = "Funded ratio (%, based on market value asset)")
  


g.ind.ERCwSTIP <-  results_indiv %>% 
  ggplot(aes(x = year, y = ERCwSTIP_PR, color = factor(order.ir), label = plot.label)) + theme_bw() + 
  geom_line(linetype = 1) + geom_point() + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_color_manual(values = c("red","deepskyblue","dodgerblue", "green3"),
                     label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
  labs(title = "Employer contribution rate (including STIP) of \nselected individual simulations",
       x = "Year", y = "Contribution as % of payroll")

#g.ind.FR
#g.ind.ERCwSTIP

# Rolling compound return

roundedReturn <- round(100*df_indiv_selcet$geoReturn,2)

hlineNotes <- c(min(roundedReturn), DC*100, max(roundedReturn))


results_indiv %<>% group_by(sim) %>%  
  mutate(rollgeoReturn   = get_rollingReturns(i.r, "moving", window = 5),
         expandgeoReturn = get_rollingReturns(i.r, "expanding")) 

g.ind.expandgeoReturn <-  results_indiv %>% 
  ggplot(aes(x = year, y = expandgeoReturn*100, color = factor(order.ir))) + theme_bw() + 
  geom_line(linetype = 1) + geom_point() + 
  geom_hline(yintercept = unique(round(100*df_indiv_selcet$geoReturn,2)), linetype = 3)+
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  # scale_y_continuous(breaks = c(seq(-20,30,5), unique(round(100*df_indiv_selcet$geoReturn,2)))) + 
  annotate("text", label = hlineNotes, x = 2015 , y = hlineNotes, size = 4, colour = c("red","dodgerblue", "green3")) + 
  scale_color_manual(values = c("red","deepskyblue","dodgerblue", "green3"),
                     label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
  labs(title = "Rolling geometric returns up to a given year for \nselected individual simulations",
       x = "Year", y = "Percent")


g.ind.annualReturn <-  results_indiv %>% 
  ggplot(aes(x = year, y = i.r*100, color = factor(order.ir))) + theme_bw() + 
  geom_line(linetype = 1) + geom_point() + 
  geom_hline(yintercept = unique(round(100*df_indiv_selcet$geoReturn,2)), linetype = 3)+
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  # scale_y_continuous(breaks = c(seq(-20,30,5), unique(round(100*df_indiv_selcet$geoReturn,2)))) + 
  annotate("text", label = hlineNotes, x = 2015 , y = hlineNotes, size = 4, colour = c("red","dodgerblue", "green3")) + 
  scale_color_manual(values = c("red","deepskyblue","dodgerblue", "green3"),
                     label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
  labs(title = " Annual returns of \nselected individual simulations",
       x = "Year", y = "Percent")

g.ind.expandgeoReturn
g.ind.annualReturn


## Stochastic Runs: Risk measures ####
#***********************************************************************
tbl.riskMeasure <- results.stch %>%   
  mutate(FR40less = FR.MA <= 40,
         FR95more = FR.MA >= 95
         ) %>%
  group_by(year) %>%
  summarise(FR95more   = 100 * sum(FR95more)/n(),
            FR40less   = 100 * sum(FR40less)/n(),
            FR.q25     = quantile(FR.MA, 0.25),
            FR.q75     = quantile(FR.MA, 0.75),
            FR.med     = median(FR.MA),
            ERC_PR.med = median(ERC_PR),
            ERCwSTIP.med = median(ERCwSTIP),
            ERCwSTIP_PR.med = median(ERCwSTIP_PR),
            ERCwSTIP_PR.q25 = quantile(ERCwSTIP_PR, 0.25),
            ERCwSTIP_PR.q75 = quantile(ERCwSTIP_PR, 0.75),
            ADC.med    = median(ADC),
            ADC_PR.med = median(ADC_PR),
            C.med      = median(C),
            C_PR.med   = median(C_PR)
            ) %>% 
  mutate(runname = runname) %>% 
  select(runname, everything())


g.stch.FR95 <- tbl.riskMeasure %>% 
  get_lineplot("year", "FR95more") + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  labs(title = "Probability of funded ratio above 95% \nas of a given year",
       x = "Year", y = "Probability(%)")
                    

g.stch.FR40 <- tbl.riskMeasure %>% 
  get_lineplot("year", "FR40less") + 
  coord_cartesian(ylim = c(0,25)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  labs(title = "Probability of funded ratio below 40% \nas of a given year",
       x = "Year", y = "Probability(%)")


g.stch.FR40full <- tbl.riskMeasure %>% 
  get_lineplot("year", "FR40less") + 
  coord_cartesian(ylim = c(0,60)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  labs(title = "Probability of funded ratio below 40% \nas of a given year",
       x = "Year", y = "Probability(%)")


g.stch.FRmed <- tbl.riskMeasure %>% 
  get_lineplot("year", "FR.med") + 
  coord_cartesian(ylim = c(0,100))  + 
  scale_y_continuous(breaks = seq(0,200, 10)) + 
  labs(title = "Median funded ratio over all simulations",
       x = "Year", y = "Percent")


g.stch.ERC_PRmed <- tbl.riskMeasure %>% 
  get_lineplot("year", "ERC_PR.med") + 
  coord_cartesian(ylim = c(0,30))  + 
  scale_y_continuous(breaks = seq(0,200, 5)) + 
  labs(title = "Median UC contributions rate (STIP excluded) over all simulations",
       x = "Year", y = "Percentage of payroll")



g.stch.ERCwSTIPmed <- tbl.riskMeasure %>% mutate(ERCwSTIP.med = ERCwSTIP.med/1e6) %>% 
  get_lineplot("year", "ERCwSTIP.med") + 
  # coord_cartesian(ylim = c(0,30))  + 
  # scale_y_continuous(breaks = seq(0,200, 5)) + 
  labs(title = "Median UC contribution (including STIP) \nover all simulations",
       x = "Year", y = "$million")


g.stch.ERCwSTIP_PRmed <- tbl.riskMeasure %>%  
  get_lineplot("year", "ERCwSTIP_PR.med") + 
  coord_cartesian(ylim = c(0,30))  + 
  scale_y_continuous(breaks = seq(0,200, 5)) + 
  labs(title = "Median UC contribution rate (including STIP) \nover all simulations",
       x = "Year", y = "Percentage of payroll")


# g.stch.ERCchg <- tbl.riskMeasure %>% 
#   get_lineplot("year", "ERC_PR.5yChg") + 
#   coord_cartesian(ylim = c(0,20))  + 
#   scale_y_continuous(breaks = seq(0,200, 5)) + 
#   labs(title = "5-year change in UC contributions rate (STIP excluded), \nmedian over all simulations",
#        x = "Year", y = "Percentage of payroll")




# median ERC w/STIP against ADC 

g.stch.CvADCmed <- tbl.riskMeasure %>% select(year,C.med, ADC.med) %>% gather(var, value, -year) %>% 
  ggplot(aes(x = year, y = value/1e6, color = var)) + theme_bw() + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_color_manual(values = c("red","darkblue"), name = "", 
                     label = c("ADC", "Contribution")) + 
  labs(title = "Median total contribution (including STIP) and \n Median ADC (Medians over all simulations)",
       x = "Year", y = "$million")


g.stch.CvADC_PRmed <- tbl.riskMeasure %>% select(year, C_PR.med, ADC_PR.med) %>% gather(var, value, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) +
  scale_color_manual(values = c("red","darkblue"), name = "", 
                     label = c("ADC", "Contribution")) + 
  labs(title = "Median total contribution rate (including STIP) and \n Median ADC as % payroll (Medians over all simulations)",
       x = "Year", y = "Percentage of payroll")


g.stch.Cmed <- tbl.riskMeasure %>% select(year,C.med) %>% 
  ggplot(aes(x = year, y = C.med/1e6)) + theme_bw() + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  labs(title = "Median total contribution (including STIP) \n(Medians over all simulations)",
       x = "Year", y = "$million")


g.stch.C_PRmed <- tbl.riskMeasure %>% select(year, C_PR.med) %>% 
  ggplot(aes(x = year, y = C_PR.med)) + theme_bw() + 
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) +
  labs(title = "Median total contribution rate (including STIP) \n(Medians over all simulations)",
       x = "Year", y = "Percentage of payroll")








# g.stch.CvADCmed 
# g.stch.CvADC_PRmed
# 
# g.stch.Cmed
# g.stch.C_PRmed
# 
# g.stch.FR95
# g.stch.FR40
# g.stch.FRmed
# g.stch.ERC_PRmed
# g.stch.ERCchg

assign(paste0("lresults_", runname), 
            list(tbl.det = tbl.det,
                 g.det.ERC = g.det.ERC,
                 g.det.ERC_PR = g.det.ERC_PR,
                 g.det.FR = g.det.FR,
                 g.det.CvADCmed = g.det.CvADCmed,
                 g.det.CvADC_PRmed =  g.det.CvADC_PRmed,
                 g.stch.Cmed =  g.stch.Cmed,
                 g.stch.C_PRmed = g.stch.C_PRmed,
                 
                 g.ind.FR  = g.ind.FR,
                 g.ind.ERCwSTIP = g.ind.ERCwSTIP,
                 g.distReturn = g.distReturn,
                 g.ind.expandgeoReturn = g.ind.expandgeoReturn,
                 g.ind.annualReturn    = g.ind.annualReturn,
                 
                 tbl.riskMeasure = tbl.riskMeasure,
                 g.stch.FR95     =  g.stch.FR95,
                 g.stch.FR40     =  g.stch.FR40,
                 g.stch.FR40full =  g.stch.FR40full,
                 g.stch.FRmed    =  g.stch.FRmed,
                 g.stch.ERC_PRmed   =  g.stch.ERC_PRmed,
                 g.stch.ERCwSTIPmed = g.stch.ERCwSTIPmed,
                 g.stch.ERCwSTIP_PRmed = g.stch.ERCwSTIP_PRmed,
                 g.stch.CvADCmed = g.stch.CvADCmed,
                 g.stch.CvADC_PRmed =  g.stch.CvADC_PRmed))


do.call(save, list(paste0("lresults_", runname), file=paste0(folderName, "lresults_", runname, ".RData")))
}


# run the "Model Parameters" section in FullOVerride_RunControl file to get runList
runname.list <- runList$runname
folderName <- "FullOverride/Results/"
for(runname in runname.list) make_lresults(runname, folderName)








# 
# runname <- "C.ADC_r7.25"
# folderName <- "FullOverride/Results/"
# runname.list <- runList$runname
# 
# fileName <- paste0(folderName,"results_", runname, ".RData") 
# load(fileName)
#   
# r1 <- penSim_results
# r2 <- penSim_results 
# 
# 
# r1 %>% group_by(year) %>% summarise(SC_q25 = quantile(SC - SC_init, 0.75)/1e6,
#                                     SC_q50 = quantile(SC - SC_init, 0.5)/1e6)
# 
# 
# r2 %>% group_by(year) %>% summarise(SC_q25 = quantile(SC - SC_init, 0.75)/1e6,
#                                     SC_q50 = quantile(SC - SC_init, 0.5)/1e6)
# 
# 
# 
# 











