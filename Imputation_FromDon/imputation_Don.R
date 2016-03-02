





fillin.actives.spreadyos.splineage <- function(lactives) {
  # salary:
  #   first spread uniformly within age.cell-yos.cell group (same salary for all)
  #   then for every yos, estimate salary for each age using a spline - adjust endpoints first for plausibility
  #   finally, adjust resulting salary within each age.cell-yos.cell proportionately to hit total payroll values from grouped data
  #   then add ea to the data
  # nactives: spread uniformly within age.cell-yos.cell group (same nactives for all), then add ea to the data
  adf <- lactives$actives.yos
  agecuts <- lactives$agecuts
  yoscuts <- lactives$yoscuts
  eacuts <- lactives$eacuts
  minage <- min(agecuts$lb)
  maxage <- max(agecuts$ub)
  minyos <- min(yoscuts$yoslb)
  maxyos <- max(yoscuts$yosub)
  
  planname <- paste0(adf$planname[1], ".fillin.yos")
  
  # adf %>% select(age, ea, salary) %>% spread(ea, salary)
  # adf %>% select(age, ea, nactives) %>% spread(ea, nactives)
  
  # create a master grouped data frame
  adf.g <- adf %>% select(-planname, -age, -yos, nactives.cell=nactives, salary.cell=salary) %>%
    mutate(pay.cell=nactives.cell * salary.cell) %>%
    mutate(ageidx=findInterval(age.cell, agecuts$lb),
           age.lb=agecuts$lb[ageidx],
           age.ub=agecuts$ub[ageidx],
           yosidx=findInterval(yos.cell, yoscuts$yoslb),
           yos.lb=yoscuts$yoslb[yosidx],
           yos.ub=yoscuts$yosub[yosidx]) %>%
    select(age.cell, yos.cell, age.lb, age.ub, yos.lb, yos.ub, nactives.cell, salary.cell, pay.cell)
  
  # expand the grouped data frame to all allowable age-yos combinations ####
  xpnd <- function(df) {
    # expand to all age-yos combinations but only keep those where ea>=15 or, if there are no such records,
    # keep the recrods with max ea
    df2 <- expand.grid(age=df$age.lb:df$age.ub, yos=df$yos.lb:df$yos.ub) %>%
      mutate(ea=age - yos) %>%
      filter((ea >= 15) | (ea<15 & ea==max(ea))) %>%
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
           ea=age - yos,
           ea.cell=eacuts$stub[findInterval(ea, eacuts$lb)])
  
  return(adf.x3)
}