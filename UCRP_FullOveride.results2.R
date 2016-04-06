






















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



