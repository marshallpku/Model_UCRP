



# #*********************************************************************************************************
# # 8. Comparing model results with Segal open plan projections ####
# #*********************************************************************************************************
# 
# # load Segal open plan projections
# fileName <- "Data/SegalProj_Data_2015.xlsx"
# 
# results_sumTiers_SegalOpen <- read_excel(fileName, sheet = "Data", skip = 1) %>% filter(!is.na(year)) %>% 
#                 mutate(year = year(year)) %>% 
#                 right_join(penSim_results_sumTiers) %>% 
#                 mutate(NC_PR.segal = 100 * NC_PR.segal,
#                        d_AL = 100 * (AL/AL.segal - 1),
#                        d_B  = 100 * (B/B.segal - 1),
#                        d_NC = 100 * (NC/NC.segal - 1),
#                        d_PR = 100 * (PR/PR.segal - 1),
#                        d_NC_PR = 100 * (NC_PR/NC_PR.segal - 1)) %>% 
#                 select(year, 
#                        AL.segal, AL, d_AL, 
#                        B.segal, B, d_B,
#                        NC.segal, NC, d_NC, 
#                        NC_PR.segal, NC_PR, d_NC_PR, 
#                        PR.segal, PR, d_PR, 
#                        everything())
# 
# kable(results_sumTiers_SegalOpen %>% filter(sim == -1) %>% select(year:d_PR, FR), digits = 2)
# 
# df_SegalOpen_long <- df_SegalOpen %>% gather(variable, value, -year)
# 
# 
# save(results_sumTiers_SegalOpen, penSim_results.t76, penSim_results.t13, penSim_results.tm13, 
#      file = "Data/Results_SegalOpen.RData")
# write.xlsx2(results_sumTiers_SegalOpen %>% filter(sim == -1), file = "Data/Results_SegalOpen.xlsx")
# 
# 
# 
# 
# 
# ## Comparing AL
# 
# plot_comp <- function(v.segal, v, d_v, ylim1, ylim2 = c(-20, 20), df = df_SegalOpen_long){
#   
#  g1 <-  df_SegalOpen_long %>% filter(variable %in% c(v.segal, v)) %>% 
#     ggplot(aes(x = year, y = value, color = variable)) + geom_point() + geom_line() + 
#     coord_cartesian(ylim = ylim1 )
#   
#  g2 <-   df_SegalOpen_long %>% filter(variable %in% c(d_v)) %>% 
#     ggplot(aes(x = year, y = value)) + geom_point() + geom_line()+ 
#     scale_y_continuous(breaks = seq(-100, 100, 5)) + 
#     coord_cartesian(ylim = ylim2 )
# 
#  return(list(g1 = g1, g2 = g2))
#  
# }
# 
# plot_comp("AL.segal", "AL", "d_AL", c(0, 2e8))
# plot_comp("B.segal",  "B",  "d_B", c(0, 1.2e7))
# plot_comp("PR.segal", "PR",  "d_PR", c(0, 3e7))
# plot_comp("NC.segal", "NC",  "d_NC", c(0, 5e6))
# 
# 
# 
# #*********************************************************************************************************
# # 9. Comparing model results with Segal closed plan projections ####
# #*********************************************************************************************************
# 
# fileName <- "Data/SegalProj_PaymentProjection_2015_ClosedGroup.xlsx"
# results_sumTiers_ActivesOnly_SegalClosed <- read_excel(fileName, sheet = "Data", skip = 2) %>% filter(!is.na(year)) %>%
#                   mutate_each(funs(./1000), -year) %>% 
#                   right_join(penSim_results_sumTiers) %>% 
#                   mutate(d_B = 100 * (B  / B.actives.segal - 1)) %>% 
#                   select(year, B.actives.segal, B, d_B, everything())
# 
# 
# kable(results_sumTiers_ActivesOnly_SegalClosed[,1:10] %>% filter(sim == -1), digits = 2)
# 
# save(results_sumTiers_ActivesOnly_SegalClosed, penSim_results.t76, penSim_results.t13, penSim_results.tm13,
#      file = "Data/Results_ActivesOnly_SegalClosed.RData")
# 
# 
# write.xlsx2(results_sumTiers_ActivesOnly_SegalClosed %>% filter(sim == -1), file = "Data/Results_ActivesOnly_SegalClosed.xlsx")
# 
# 
# 
# 




#*********************************************************************************************************
# Detecitve work: term rates ####
#*********************************************************************************************************
# The AL of actives becomes even higher when higher term rates are used. 

# detective.t13 <- penSim_results
# save(detective.t13, file= "detective.t13.RData")
# 
# load("detective.t13.RData")
# detective.t13 %>% filter(sim == -1) %>% select(Tier,year, FR, MA, AL, AL.act,AL.act.laca, AL.act.v,AL.act.LSC, AL.la, AL.ca, AL.term, AL, PVFB.laca, PVFB.LSC, PVFB.v, PVFB, 
#                         B, B.la, B.ca, B.LSC,B.v, nactives, nterms, PR, NC_PR) %>% data.frame


