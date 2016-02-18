








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
