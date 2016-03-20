



load("./Data/UCRP.PlanInfo.RData")

salgrowth_w.fac <- 0.15
salgrowth_w.stf <- 1 - salgrowth_w.fac
salgrowth %<>%  mutate(salgrowth_w = salgrowth_w.stf * salgrowth.stf + salgrowth_w.fac * salgrowth.fac)

# Assumed initial salary of faculty and staff

init.sal.fac <- 100000
init.sal.stf <- 75000


salgrowth %>% mutate(sal.fac = init.sal.fac *  cumprod(ifelse(yos == 0, 1, 1 + lag(salgrowth.fac))),
                     sal.stf = init.sal.stf *  cumprod(ifelse(yos == 0, 1, 1 + lag(salgrowth.stf))),
                     sal.sum = sal.fac * salgrowth_w.stf + sal.stf *salgrowth_w.stf,
                     salgrowth.sum = lead(sal.sum)/sal.sum - 1
                     )