


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

salary  %<>% gather(yos, sx, -age) %>% filter(yos != "Total", age!= "Total") %>% 
             mutate(age_l = ifelse(grepl("over",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")),
                    age_u = ifelse(grepl("Under", age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")),
                    yos_l = ifelse(grepl("over",  yos),  str_extract(yos, "\\d+"),  
                                   str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)),
                    yos_u = ifelse(grepl("Under", yos),  str_extract(yos, "\\d+"),  
                                   str_extract(yos, "-\\d+") %>% gsub("\\D+", "",.))
             ) %>% 
             mutate(sx       = as.numeric(sx) %>% na2zero,
                    age_l    = as.numeric(age_l),
                    age_u    = as.numeric(age_u),
                    yos_l    = as.numeric(yos_l),
                    yos_u    = as.numeric(yos_u),
                    age = NULL,
                    yos = NULL) %>% 
             mutate(age_cell = ifelse(is.na(age_l),age_u - 3, age_l + 2),
                    yos_cell = yos_l + 2)



#*************************************************************************************************************
#                                       Importing Data for actives                                       #####                  
#*************************************************************************************************************

retirees <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Retirees", skip = 2)
names(retirees) <- c("age", "nla", "B.la")


retirees %>% mutate(age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
                    age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
                    nla   = as.numeric(nla), 
                    B.la  = B.la * 12, # convert to annual benefit
                    age_cell = ifelse(is.na(age_l), age_u, age_l + 2),
                    age = NULL)










# Use AZ-PERS data as place holder
load("Data/PlanDataMain.RData")

benefit

init_pop$actives


# Benefit payment for initial retirees/beneficiaries in year 1.
# It is assumed that all initial retirees entered the workforce at the age r.min - 1.
benefit <- retirees %>% 
  mutate(year       = init.year,
         ea         = r.min - 1,
         age.r      = age,
         start.year = year - (age - ea)) %>% 
  filter(planname == "AZ-PERS-6.fillin",
         age >= r.min) %>% 
  select(start.year, ea, age, age.r, benefit)


init_pop$retirees
# init_pop$actives %>% sum
# init_pop$retirees


