

library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library("btools")


#****************************************************************************************************
#                    Global constants ####
#****************************************************************************************************
#setwd("..")
path <- "./Data/"
fileName <- "UCRP-MembersData-2015(1).xlsx"
            

#****************************************************************************************************
#                                       Tools                                                   #####                  
#****************************************************************************************************

getcell <- function(file, sheet, cell) {
  require(XLConnect)
  value <- readWorksheetFromFile(file, sheet=sheet, header=FALSE, region=cell, colTypes="character")
  return(as.character(value))
}


xlrange <- function(file, sheet, cell1, cell2) {
  startcell <- getcell(file, sheet, cell1)
  endcell   <- getcell(file, sheet, cell2)
  range     <- paste0(startcell, ":", endcell)
  return(range)
}


get_bound <- function(range, bound = c("l","u")){
  # range must be in the form of "XX-YY", "X" is a single digit, and "XX" <= "YY".
  switch(bound,
         l =  str_extract(range, "\\d+-") %>% gsub("\\D+", "",.) %>% as.integer,
         u =  str_extract(range, "-\\d+") %>% gsub("\\D+", "",.) %>% as.integer)
}



# Load actives
import_actives <- function(file, sheet, planname){
  
# tier <- "t76"
# file <- paste0(path, fileName)
# sheet <- paste0("Actives_", tier)

  range <- xlrange(file, sheet, "B2", "B3")
  
  df <- readWorksheetFromFile(file, sheet=sheet, header=TRUE, region=range, colTypes="character")
  
  yoscuts <- df %>% filter(type == "yosgrp") %>%
    select(starts_with("X")) %>%
    gather(yos.cell, yosgrp) %>%
    mutate(yos.cell=as.integer(gsub("[^0-9]", "", yos.cell)),
           yoslb = get_bound(yosgrp, "l"),
           yosub = get_bound(yosgrp, "u")) %>% 
    select(-yosgrp)
  yoscuts
  
  agecuts <- df %>% filter(type != "yosgrp") %>% 
             filter(type == unique(type)[1]) %>% 
             select(age.cell, agegrp) %>% 
             mutate(agelb = get_bound(agegrp, "l"),
                    ageub = get_bound(agegrp, "u")) %>% 
             select(-agegrp)
  agecuts
  
  
  df %<>% filter(type != "yosgrp") %>% 
         select(type, age.cell, starts_with("X")) %>%
         gather(yos.cell, value, -type, -age.cell) %>%
         mutate(yos.cell = as.integer(gsub("[^0-9]", "", yos.cell)),
                age.cell = as.integer(age.cell),
                value    = as.numeric(value),
                age = age.cell,
                yos = yos.cell,
                planname = planname) %>%
         filter(!is.na(value)) %>% 
         spread(type, value) %>%
         arrange(age.cell, yos.cell)
  
  
  lactives <- list()
  lactives$agecuts <- agecuts
  lactives$yoscuts <- yoscuts
  lactives$actives.yos <- df
  
  return(lactives)
}


# Load retirees, also can be used to load initial beneficiaries
import_retirees_byAge <- function(file, sheet, planname){

# file <- paste0(path, fileName)
# sheet <- "Beneficiaries"
# planname <- Tier_select


range <- xlrange(file, sheet, "B2", "B3")
benperiod <- getcell(file, sheet, "B4")
benmult <- ifelse(benperiod=="month", 12, 1)
name_N  <- getcell(file, sheet, "B5")
name_V  <- getcell(file, sheet, "B6")


df <- readWorksheetFromFile(file, sheet=sheet, header=TRUE, region=range, colTypes="character")

agecuts <- df %>%  
  select(age.cell, agegrp) %>% 
  mutate(agelb = get_bound(agegrp, "l"),
         ageub = get_bound(agegrp, "u")) %>% 
  select(-agegrp)

df %<>%  
  select(-agegrp) %>%
  colwise(as.numeric)() %>% 
  mutate(age.cell = as.integer(age.cell),
         age = age.cell,
         V = V * benmult,
         planname = planname)


list_out <- list()
list_out$data <- df
list_out$agecuts <- agecuts
list_out$varNames <- c(name_N = name_N, name_V = name_V)

return(list_out)
}


# Interpolation of actives
fillin.actives.spreadyos.splineage <- function(lactives) {
  # salary:
  #   first spread uniformly within age.cell-yos.cell group (same salary for all)
  #   then for every yos, estimate salary for each age using a spline - adjust endpoints first for plausibility
  #   finally, adjust resulting salary within each age.cell-yos.cell proportionately to hit total payroll values from grouped data
  #   then add ea to the data
  # nactives: spread uniformly within age.cell-yos.cell group (same nactives for all), then add ea to the data
  
  lactives
  
  adf <- lactives$actives.yos
  agecuts <- lactives$agecuts
  yoscuts <- lactives$yoscuts
  #eacuts <- lactives$eacuts
  minage <- min(agecuts$agelb)
  maxage <- max(agecuts$ageub)
  minyos <- min(yoscuts$yoslb)
  maxyos <- max(yoscuts$yosub)
  
  planname <- paste0(adf$planname[1])
  
  # adf %>% select(age, ea, salary) %>% spread(ea, salary)
  # adf %>% select(age, ea, nactives) %>% spread(ea, nactives)
  
  # create a master grouped data frame
  adf.g <- adf %>% select(-planname, -age, -yos, nactives.cell=nactives, salary.cell=salary) %>%
    mutate(pay.cell=nactives.cell * salary.cell) %>%
    mutate(ageidx = findInterval(age.cell, agecuts$agelb),
           age.lb = agecuts$agelb[ageidx],
           age.ub = agecuts$ageub[ageidx],
           yosidx = findInterval(yos.cell, yoscuts$yoslb),
           yos.lb = yoscuts$yoslb[yosidx],
           yos.ub = yoscuts$yosub[yosidx]) %>%
    select(age.cell, yos.cell, age.lb, age.ub, yos.lb, yos.ub, nactives.cell, salary.cell, pay.cell)
  
  # expand the grouped data frame to all allowable age-yos combinations ####
  xpnd <- function(df) {
    # expand to all age-yos combinations but only keep those where ea>=15 or, if there are no such records,
    # keep the recrods with max ea
    df2 <- expand.grid(age=df$age.lb:df$age.ub, yos=df$yos.lb:df$yos.ub) %>%
      mutate(ea=age - yos) %>%
      filter((ea >= 20) | (ea<20 & ea==max(ea))) %>%
      select(-ea)
    return(df2)
  }
  
  adf.x <- adf.g %>% rowwise() %>%
    do(cbind(., xpnd(.))) %>%
    ungroup %>%  # get rid of rowwise
    group_by(age.cell, yos.cell) %>%
    mutate(n.cell=n()) %>%
    select(age, yos, everything()) %>%
    arrange(age, yos)
  
  
  # work with the expanded data ####
  
  # we have to anchor the endpoints with reasonable values BEFORE computing the spline
  adjustends <- function(age, salary) {
    # the basic idea is that if an endpoint is NA, insert a plausible value
    
    # simple rule: if spline first or last value falls within +/ 50% of the nearest nonNA value, use spline estimate
    # otherwise use the capped value
    firstnonna <- salary[which.min(is.na(salary))]
    lastnonna <- rev(salary)[which.min(is.na(rev(salary)))]
    bound <- .5
    firstrange <- c(firstnonna * bound, firstnonna * (1 + bound))
    lastrange <- c(lastnonna * bound, lastnonna * (1 + bound))
    cap <- function(sal, range) {
      cappedval <- max(sal, range[1])
      cappedval <- min(cappedval, range[2])
      return(cappedval)
    }
    
    salary.est <- spline(age, salary, xout=age)$y # what does spline think naively?
    salary.adjusted <- salary
    
    if(is.na(salary[1])) salary.adjusted[1] <- cap(salary.est[1], firstrange)
    ilast <- length(salary)
    if(is.na(salary[ilast])) salary.adjusted[ilast] <- cap(salary.est[ilast], firstrange)
    
    return(salary.adjusted)
  }
  
  # test out adjustends
  # fs <- function(age, sal) return(spline(age, sal, xout=age)$y) # spline doesn't seem to work with dplyr if not in function
  # # various salaries to try out
  # salary <- seq(20, 50, length.out = 10)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 80)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, NA)
  # salary <- c(NA, 10, 30, NA, 40, NA, 50, 80, NA, NA)
  # age <- 21:30
  # d <- data_frame(age, salary, saladj=adjustends(age, salary)) %>%
  #   mutate(sal.spline=fs(age, salary),
  #          saladj.spline=fs(age, saladj))
  # d
  # qplot(age, value, data=gather(d, variable, value, -age), colour=variable, geom=c("point", "line")) + scale_x_continuous(breaks=0:100) + geom_hline(y=0)
  
  
  spline.y2 <- function(age, salary, safesalary) {
    # safesalary is what we use if salary has no data
    if(all(is.na(salary))) {
      print("AllNA")
      salary <- safesalary
    }
    salary.adjusted <- adjustends(age, salary)
    
    sp.out <- spline(age, salary.adjusted, xout=age)
    salout <- sp.out$y
    return(salout)
  }
  
  adf.x3 <- adf.x %>% ungroup %>% # MUST be ungrouped or ifelse won't work if there is only one rec in a group
    mutate(nactives=nactives.cell / n.cell, # always spread nactives uniformly
           salary.group=ifelse(age==age.cell & yos==yos.cell, salary.cell, NA),
           salary.group=ifelse(salary.group==0, NA, salary.group),
           salary.agecell=ifelse(age==age.cell, salary.cell, NA)) %>% # Yimeng's first step
    group_by(yos) %>%
    arrange(age) %>%
    mutate(salary.spline.adjep=spline.y2(age, salary.agecell, salary.cell)) %>% # Yimeng's 2nd step with endpoint adjustment
    group_by(age.cell, yos.cell) %>%
    mutate(planname=planname,
           pay.unadj=sum(salary.spline.adjep * nactives),
           adjust=pay.cell / pay.unadj,
           salary.final=salary.spline.adjep * adjust,
           pay.adj=sum(salary.final * nactives),
           ea=age - yos
           #ea.cell=eacuts$stub[findInterval(ea, eacuts$lb)]
           )
  
  return(adf.x3)
}


# Interpolation of retirees
fillin.retirees <- function(list_data) {
  
  rdf <- select(list_data$data, planname, age, N, V) # keep only the vars we want
  agecuts <- lretirees$agecuts
  
  planname <- paste0(rdf$planname[1], ".fillin")
  name_N <- list_data$varNames["name_N"]
  name_V <- list_data$varNames["name_V"]
  
  # add group ranges to the retirees data frame
  combo <- rdf %>%
    mutate(totben=N * V) %>%
    mutate(ageidx=findInterval(age, agecuts$agelb),
           age.lb=agecuts$agelb[ageidx],
           age.ub=agecuts$ageub[ageidx]) %>%
    arrange(age)
  
  # get avg benefits by age, via spline
  avgben <- splong(select(combo, age, V), "age", min(combo$age.lb):max(combo$age.ub))
  # force benefit to be non-negative DJB added 10/30/2015
  avgben <- avgben %>% mutate(V=ifelse(V<0, 0, V))
  
  guessdf <- data.frame(age=min(combo$age.lb):max(combo$age.ub)) %>%
    mutate(ageidx=findInterval(age, agecuts$agelb),
           age.cell=combo$age[match(ageidx, combo$ageidx)],
           N.cell=combo$N[match(ageidx, combo$ageidx)],
           V.cell=combo$V[match(ageidx, combo$ageidx)]) %>%
    group_by(age.cell) %>%
    mutate(n.cell=n(),
           N=N.cell / n.cell, # spread nretirees evenly
           adjV=avgben$V[match(age, avgben$age)], # get the spline-based avg benefit
           adjtotben=N * adjV)
  
  # refine the guess by adjusting ensure that we hit the right total benefits in each group
  guessdf2 <- guessdf %>% group_by(age.cell) %>%
    mutate(adjust=mean(N.cell * V.cell) / sum(adjtotben),
           V=adjV*adjust,
           totben=N * V)
  
  rdf.fillin <- guessdf2 %>% mutate(planname=planname) %>%
                select(planname, age.cell, age, N, V) %>%
                ungroup
                #plyr::rename(c("N" = list_data$varNames["name_N"])))

  names(rdf.fillin)[names(rdf.fillin) == "N"] <- name_N
  names(rdf.fillin)[names(rdf.fillin) == "V"] <- name_V

  return(rdf.fillin)
}



#*************************************************************************************************************
#                                       Importing Data for initial actives                                       #####                  
#*************************************************************************************************************

lactives <- import_actives(paste0(path, fileName), paste0("Actives_", Tier_select), paste0("Actives_", Tier_select))
lactives$actives.yos %<>% filter(age - yos >= 20) # need to figure out how to add memebers with ea<20 back             


actives <- fillin.actives.spreadyos.splineage(lactives) %>% ungroup %>% 
           select(planname, age, yos, ea,
                  #age.cell, yos.cell, 
                  nactives, salary=salary.final)


#*************************************************************************************************************
#                                       Importing Data for initial retirees and beneficiaries            #####                  
#*************************************************************************************************************

lretirees      <- import_retirees_byAge(paste0(path, fileName), "Retirees",      paste0("Retirees_",      Tier_select))
lbeneficiaries <- import_retirees_byAge(paste0(path, fileName), "Beneficiaries", paste0("Beneficiaries_", Tier_select))

retirees           <- fillin.retirees(lretirees) %>% ungroup %>% select(-age.cell)
init_beneficiaries <- fillin.retirees(lbeneficiaries) %>% select(-age.cell)


# actives.fillin %<>% filter(ea>=20) 
# actives.fillin$nactives %>% sum


## Check results
# actives %>% arrange(ea, age)
# 
# actives %>% filter(ea %in% seq(20, 70, 5)) %>%  
# ggplot(aes(x = age, y = nactives, color = factor(ea))) + geom_line() + geom_point()
# 
# 
# actives %>% filter(yos %in% seq(2, 42, 5)) %>%  
#   ggplot(aes(x = age, y = salary, color = factor(yos))) + geom_line() + geom_point()
# 
# actives %>% filter(ea %in% seq(20, 70, 5)) %>%  
#   ggplot(aes(x = age, y = salary, color = factor(ea))) + geom_line() + geom_point()
# 
# 
# 
# lactives$actives.yos %>% mutate(ea = age - yos) %>%   #  filter(yos %in% seq(0, 50, 5)) %>%  
#   ggplot(aes(x = age, y = salary, color = factor(ea))) + 
#   geom_line() + 
#   geom_point()
# 
# 
# lactives$actives.yos %>% mutate(ea = age - yos) %>%   #  filter(yos %in% seq(0, 50, 5)) %>%  
#   ggplot(aes(x = age, y = salary, color = factor(yos))) + 
#   geom_line() + 
#   geom_point()








#*************************************************************************************************************
#                                       Importing Data for initial actives                                       #####                  
#*************************************************************************************************************

# actives_raw <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Active_t76", skip = 5) %>% rename(age = Age)
# actives <- actives_raw %>% filter(!is.na(age))
# salary  <- actives_raw %>% filter(is.na(age) & !is.na(Total)); salary$age <- actives$age
# 
# # actives
# # salary
# 
# actives %<>% gather(yos, nactives, -age) %>% filter(yos != "Total", age!= "Total") %>% 
#              mutate(age_l = ifelse(grepl("over",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")),
#                     age_u = ifelse(grepl("Under", age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")),
#                     yos_l = ifelse(grepl("over",  yos),  str_extract(yos, "\\d+"),  
#                                                          str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)),
#                     yos_u = ifelse(grepl("Under", yos),  str_extract(yos, "\\d+"),  
#                                                          str_extract(yos, "-\\d+") %>% gsub("\\D+", "",.))
#                    ) %>% 
#              mutate(nactives = as.numeric(nactives) %>% na2zero,
#                     age_l    = as.numeric(age_l),
#                     age_u    = as.numeric(age_u),
#                     yos_l    = as.numeric(yos_l),
#                     yos_u    = as.numeric(yos_u),
#                     age = NULL,
#                     yos = NULL) %>% 
#              mutate(age_cell = ifelse(is.na(age_l),age_u - 3, age_l + 2),
#                     yos_cell = yos_l + 2)
# 
# salary  %<>% gather(yos, salary, -age) %>% filter(yos != "Total", age!= "Total") %>% 
#              mutate(age_l = ifelse(grepl("over",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")),
#                     age_u = ifelse(grepl("Under", age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")),
#                     yos_l = ifelse(grepl("over",  yos),  str_extract(yos, "\\d+"),  
#                                    str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)),
#                     yos_u = ifelse(grepl("Under", yos),  str_extract(yos, "\\d+"),  
#                                    str_extract(yos, "-\\d+") %>% gsub("\\D+", "",.))
#              ) %>% 
#              mutate(salary       = as.numeric(salary) %>% na2zero,
#                     age_l    = as.numeric(age_l),
#                     age_u    = as.numeric(age_u),
#                     yos_l    = as.numeric(yos_l),
#                     yos_u    = as.numeric(yos_u),
#                     age = NULL,
#                     yos = NULL) %>% 
#              mutate(age_cell = ifelse(is.na(age_l),age_u - 3, age_l + 2),
#                     yos_cell = yos_l + 2)

# 
# actives %<>% select(age = age_cell, yos = yos_cell, nactives) %>% mutate(ea = age - yos) %>%  
#              left_join(salary  %>% select(age = age_cell, yos = yos_cell, salary))

 # x <- actives  %>% mutate(salary_sum = nactives*salary)
 # x$salary_sum %>% sum
 # 
 # actives


#*************************************************************************************************************
#                                       Importing Data for initial retirees                                     #####                  
#*************************************************************************************************************

# retirees <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Retirees", skip = 2)
# names(retirees) <- c("age", "nretirees", "benefit")
# 
# 
# retirees %<>% mutate(age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
#                     age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
#                     nretirees = as.numeric(nretirees), 
#                     benefit   = benefit * 12, # convert to annual benefit
#                     age_cell  = ifelse(is.na(age_l), age_u, age_l + 2),
#                     age = NULL)
# 
# 
# retirees %<>% select(age = age_cell, nretirees, benefit)

#*************************************************************************************************************
#                                       Importing Data for initial terms                                     #####                  
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
  mutate(year = init.year,
         age.term = age - 1,   # assume all terms are terminated in init.year - 1.
         ea   = age.term - yos,
         start.year = year - (age - ea)) %>% 
  filter(age.term >= min.ea,
         ea >= min.ea)

   # assme age.term is age - 1, ea must be greater than 20



#*************************************************************************************************************
#                                       Importing Data for initial beneficiaries                                     #####                  
#*************************************************************************************************************

# n.R0S1 represents the number of beneficiaries of contingent annuitants.
# 
# init_beneficiaries <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Beneficiaries", skip = 2)
# names(init_beneficiaries) <- c("age", "n.R0S1", "benefit")
# 
# 
# init_beneficiaries %<>% mutate(age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
#                         age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
#                         n.R0S1 = as.numeric(n.R0S1), 
#                         benefit   = benefit * 12, # convert to annual benefit
#                         age_cell  = ifelse(is.na(age_l), age_u, age_l + 2),
#                         age = NULL)
# 
# init_beneficiaries %<>% select(age = age_cell, n.R0S1, benefit)
# 
# init_beneficiaries



#*************************************************************************************************************
#                                       Importing Summary data                                     #####                  
#*************************************************************************************************************

summary_actives <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Sum_Active", skip = 1) %>% 
                   gather(Tier, n, -Type, -Sex) %>% 
                   filter(!Tier %in% c("Tier2", "All"), Type != "Safety") %>% 
                   mutate(n    = as.numeric(n) %>% na2zero,
                          # Tier = levels(Tier)[Tier], # no longer needed with the new version of tidyr
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





