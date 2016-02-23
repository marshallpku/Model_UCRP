


#*************************************************************************************************************
#                                       Importing Data for actives                                       #####                  
#*************************************************************************************************************

actives_raw <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Active_t76", skip = 5) %>% rename(age = Age)
actives <- actives_raw %>% filter(!is.na(age))
salary  <- actives_raw %>% filter(is.na(age)); salary$age <- actives$age


actives %<>% gather(yos, nactives, -age) %>% filter(yos != "Total", age!= "Total") %>% 
             mutate(age_l = ifelse(grepl("over",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")),
                    age_u = ifelse(grepl("Under", age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")),
                    yos_l = ifelse(grepl("over",  yos),  str_extract(yos, "\\d+"),  
                                                         str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)),
                    yos_u = ifelse(grepl("Under", yos),  str_extract(yos, "\\d+"),  
                                                         str_extract(yos, "-\\d+") %>% gsub("\\D+", "",.))
                   ) %>% 
             mutate(nactives = as.numeric(nactives) %>% na2zero,
                    age_l    = as.numeric(age_l),
                    age_u    = as.numeric(age_u),
                    yos_l    = as.numeric(yos_l),
                    yos_u    = as.numeric(yos_u),
                    age = NULL,
                    yos = NULL) %>% 
             mutate(age_cell = ifelse(is.na(age_l),age_u - 3, age_l + 2),
                    yos_cell = yos_l + 2)

salary  %<>% gather(yos, salary, -age) %>% filter(yos != "Total", age!= "Total") %>% 
             mutate(age_l = ifelse(grepl("over",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")),
                    age_u = ifelse(grepl("Under", age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")),
                    yos_l = ifelse(grepl("over",  yos),  str_extract(yos, "\\d+"),  
                                   str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)),
                    yos_u = ifelse(grepl("Under", yos),  str_extract(yos, "\\d+"),  
                                   str_extract(yos, "-\\d+") %>% gsub("\\D+", "",.))
             ) %>% 
             mutate(salary       = as.numeric(salary) %>% na2zero,
                    age_l    = as.numeric(age_l),
                    age_u    = as.numeric(age_u),
                    yos_l    = as.numeric(yos_l),
                    yos_u    = as.numeric(yos_u),
                    age = NULL,
                    yos = NULL) %>% 
             mutate(age_cell = ifelse(is.na(age_l),age_u - 3, age_l + 2),
                    yos_cell = yos_l + 2)


actives %<>% select(age = age_cell, yos = yos_cell, nactives) %>% mutate(ea = age - yos) %>%  
             left_join(salary  %>% select(age = age_cell, yos = yos_cell, salary))


actives


#*************************************************************************************************************
#                                       Importing Data for retirees                                     #####                  
#*************************************************************************************************************

retirees <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Retirees", skip = 2)
names(retirees) <- c("age", "nretirees", "benefit")


retirees %<>% mutate(age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
                    age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
                    nretirees = as.numeric(nretirees), 
                    benefit   = benefit * 12, # convert to annual benefit
                    age_cell  = ifelse(is.na(age_l), age_u, age_l + 2),
                    age = NULL)


retirees %<>% select(age = age_cell, nretirees, benefit)

#*************************************************************************************************************
#                                       Importing Data for terms                                     #####                  
#*************************************************************************************************************

terms_HAPC <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Terms_HAPC", skip = 3) %>% rename(age = Age) %>% 
  gather(yos, HAPC, -age) %>% 
  filter(yos != "non-Vested") %>% 
  mutate(
    age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
    age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
    
    yos_l =  ifelse(grepl("\\D$",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)) %>% as.numeric,
    yos_u =  ifelse(grepl("^\\D",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d{2}$")) %>% as.numeric,
    
    HAPC = (as.numeric(HAPC) * 12) %>% na2zero,
    
    # age_cell = ifelse(is.na(age_l), age_u - 3, age_l+3),
    # yos_cell = ifelse(is.na(yos_u), yos_l + 4, round((yos_l + yos_u)/2 )),
    
    age_cell = ifelse(is.na(age_u), age_l + 3, age_u),
    yos_cell = yos_l,
    
    age = NULL,
    yos = NULL
  )

terms_n <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Terms_N", skip = 5) %>% rename(age = Age) %>% 
  gather(yos, nterm, -age) %>% 
  filter(yos != "non-Vested", yos != "All", age != "All") %>% 
  mutate(
    age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
    age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
    
    yos_l =  ifelse(grepl("\\D$",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)) %>% as.numeric,
    yos_u =  ifelse(grepl("^\\D",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d{2}$")) %>% as.numeric,
    
    nterm = as.numeric(nterm) %>% na2zero,
    
    # age_cell = ifelse(is.na(age_l), age_u - 3, age_l+3),
    # yos_cell = ifelse(is.na(yos_u), yos_l + 4, round((yos_l + yos_u)/2 )),
    
    age_cell = ifelse(is.na(age_u), age_l + 3, age_u),
    yos_cell = yos_l,
    
    age = NULL,
    yos = NULL
  )

terminated <-  terms_n %>% select(age = age_cell, yos = yos_cell, nterm) %>% 
  left_join(terms_HAPC %>% select(age = age_cell, yos = yos_cell, HAPC)) %>% 
  filter(age - 1 - yos >= min.ea) # assme age.term is age - 1, ea must be greater than 20



#*************************************************************************************************************
#                                       Importing Summary data                                     #####                  
#*************************************************************************************************************

summary_actives <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Sum_Active", skip = 1) %>% 
                   gather(Tier, n, -Type, -Sex) %>% 
                   filter(!Tier %in% c("Tier2", "All"), Type != "Safety") %>% 
                   mutate(n    = as.numeric(n) %>% na2zero,
                          Tier = levels(Tier)[Tier], 
                          Tier = ifelse(Tier == "13Tier", "t13", Tier),
                          Tier = ifelse(Tier == "76Tier", "t76", Tier),
                          Tier = ifelse(Tier == "Modi13", "tm13", Tier))
             
summary_actives




#*************************************************************************************************************
#                                        gender and faculty-staff ratios                                 #####                  
#*************************************************************************************************************

# Gender ratio for selected Tier
ratio_gender <- summary_actives %>% filter(Tier == Tier_select) %>% group_by(Sex) %>% 
  summarise(n = sum(n))

pct.F.actives <- with(ratio_gender, n[Sex == "F"]/sum(n))
pct.M.actives <- 1 - pct.F.actives


# Employment type ratios
ratio_type <- summary_actives %>% group_by(Type, Tier) %>% 
  summarise(n = sum(n))

pct.fac.actives.t76 <- with(ratio_type %>% filter(Tier == "t76"), n[Type == "Faculty"]/sum(n))
pct.stf.actives.t76 <- 1 - pct.fac.actives.t76 

pct.fac.actives.t13 <- with(ratio_type %>% filter(Tier == "t13"), n[Type == "Faculty"]/sum(n))
pct.stf.actives.t13 <- 1 - pct.fac.actives.t13 


# The relatively small number of members for tm13 may not accurately reflect the type ratio at the maturity of the plan. 
# For now we may want to apply the ratio for t13 to it.
# Need to check with UCRP about whether the ratio of 15/9291 reflects the nature of the plan.

# pct.fac.actives.tm13 <- with(ratio_type %>% filter(Tier == "tm13"), n[Type == "Faculty"]/sum(n))
# pct.stf.actives.tm13 <- 1 - pct.fac.actives.tm13  

pct.fac.actives.tm13 <- pct.fac.actives.t13
pct.stf.actives.tm13 <- 1 - pct.fac.actives.tm13 

pct.ca <- pct.ca.F * pct.F.actives + pct.ca.M * pct.M.actives # For those opting for annuit rather than LSC, the % of choosing contingent annuity (0% for 2013 and modified 2013 tier)
pct.la <- 1 - pct.ca                                          # For those opting for annuit rather than LSC, the % of choosing life annuity (100% for 2013 and modified 2013 tier)





