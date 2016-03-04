# This script constructs tables of the retirement rates for the UCRP model. 


#*********************************************************************************************************
#                      ## Produce mortality tables ####
#*********************************************************************************************************
load("./Data/UCRP.RP2014.RData")

# pre-retirement table 
mortality.pre <- mortality_RP2014 %>% filter(type %in% c("qxm.pre.M", "qxm.pre.F")) %>% ungroup %>% 
  spread(type, qxm_proj)

# post-retirement table 
mortality.post <- mortality_RP2014 %>% filter(type %in% c("qxm.post.M", "qxm.post.F")) %>% ungroup %>% 
  spread(type, qxm_proj)

# disabled retirees table 
mortality.disb <- mortality_RP2014 %>% filter(type %in% c("qxm.d.M", "qxm.d.F")) %>% ungroup %>% 
  spread(type, qxm_proj)


# mortality.pre %>% filter(year == 2029)  %>% print(n = 1000)
# mortality.disb %>% filter(year == 2029) %>% print(n = 1000)
# 


#*********************************************************************************************************
#                      ## Retirement rates.  ####
#*********************************************************************************************************

retrates <- read_excel("Data/PlanInfo-UCRP.xlsx", sheet = "Ret_dec", skip = 2) %>% filter(!is.na(age))

# The following rules need to be applied to the retirement rate when merged with the demographic data. 
#  - These rates apply for those with ten to twenty years of service. For ages under 65, 70% of these rates will be used for those with less than ten years of 
#    service and 160% of these rates will be used for those with twenty or more years of service, with the exception that the age 64 rate is set equal to the 
#    age 63 rate for those with twenty or more years of service. 
#


#*********************************************************************************************************
#                      ## Lump Sum Cashout rates.  ####
#*********************************************************************************************************

LSCrates_raw <- read_excel("Data/PlanInfo-UCRP.xlsx", sheet = "Ret_cashout", skip = 2) %>% filter(!is.na(yos)) %>% 
                mutate(yos.match = gsub("\\D+", "", yos) %>% substr(1, 2) %>% as.numeric)


LSCrates <- data.frame(yos = 0:55) %>% 
            mutate(yos.match = ifelse(yos <= 5, 5, yos),
                   yos.match = ifelse(yos >= 15 & yos <= 19, 15, yos.match),
                   yos.match = ifelse(yos >= 20 & yos <= 24, 20, yos.match),
                   yos.match = ifelse(yos >= 25 & yos <= 29, 25, yos.match),
                   yos.match = ifelse(yos >= 30, 30, yos.match)
                   ) %>% 
            left_join(LSCrates_raw %>% select(-yos)) %>% 
            select(-yos.match)



#*********************************************************************************************************
#                      ## Termination rates  ####
#*********************************************************************************************************

termrates_raw <- read_excel("Data/PlanInfo-UCRP.xlsx", sheet = "Term_dec", skip = 2) %>% filter(!is.na(yos)) %>% 
                 rename(yos.match = yos)

termrates <- data.frame(yos = 0:55) %>% 
             mutate(yos.match = ifelse(yos>=20, 20, yos)) %>% 
             left_join(termrates_raw) %>% 
             select(-yos.match)


#*********************************************************************************************************
#                      ## Disability Incidence rates  ####
#*********************************************************************************************************

disbrates_raw <- read_excel("Data/PlanInfo-UCRP.xlsx", sheet = "Disb_dec", skip = 2) %>% filter(!is.na(age.lower))

disbrates <- data.frame(age = 20:75) %>% 
             mutate(age.lower = floor(age*0.2)/0.2, 
                    age.lower = ifelse(age>=65, 65, age.lower)) %>% 
             left_join(disbrates_raw) %>% 
             select(-age.lower, -age.upper)


#*********************************************************************************************************
#                      ## benefit factors  ####
#*********************************************************************************************************

bfactor_raw <- read_excel("Data/PlanInfo-UCRP.xlsx", sheet = "Ret_bfactor", skip = 3) %>% filter(!is.na(age)) %>% 
               mutate(age.match = gsub("\\D+", "", age) %>% substr(1, 2) %>% as.numeric)

bfactor <- data.frame(age = 50:75) %>% 
  mutate(age.match = ifelse(age >= 65, 65, age)) %>% 
  left_join(bfactor_raw %>% select(-age)) %>% 
  select(-age.match)



#*********************************************************************************************************
#                      ## Salary scale  ####
#*********************************************************************************************************

infl <- 0.03
raise <- 0.005

salgrowth_raw <- read_excel("Data/PlanInfo-UCRP.xlsx", sheet = "SalaryGrowth", skip = 2) %>% filter(!is.na(yos)) %>% 
  mutate(yos.match = gsub("\\D+", "", yos) %>% substr(1, 2) %>% as.numeric)


salgrowth <- data.frame(yos = 0:55) %>% 
  mutate(yos.match = ifelse(yos >= 20, 20, yos)) %>% 
  left_join(salgrowth_raw %>% select(-yos)) %>% 
  select(-yos.match) %>% 
  mutate(salgrowth.fac = salgrowth.fac + infl + raise,
         salgrowth.stf = salgrowth.stf + infl + raise)



#*********************************************************************************************************
#                      ## Initial Amortization Basis  ####
#*********************************************************************************************************

init_amort_raw <- read_excel("Data/PlanInfo-UCRP.xlsx", sheet = "Init_amort", skip = 0)%>% filter(!is.na(Type)) 
names(init_amort_raw) <-  c("Type", "year.est", "m.init", "amount.init", "amount.annual", "year.remaining", "Balance")

init_amort_raw %<>% mutate(year.est = year(year.est)) %>% filter(Type != "Total")




save(mortality.pre, mortality.post, mortality.disb, retrates, LSCrates, termrates, disbrates, bfactor, salgrowth, init_amort_raw, 
     file  = "Data/UCRP.PlanInfo.RData")












