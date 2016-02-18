# This script explores how to implement the amortization method in UCRP

# 

## Amortization before 7/1/2015 AV
# - Initial surpuls is amortized as a level dollar amount over 3 years.
# - UAAL identified prior to 7/1/2015 valuation:(before and not including, 14-15 plan year): any intial UAAL (after a period of surplus) or 
#   change in UAAL due to gains and losses(including contribution gains and losses) is amortized over 30 years. 
#
# - UAAL as of 2010:
#   Effective 7/1/2010， all UAAL amortization bases as of 7/1/2010 were combined and the combined bases is amortized as a level dollar amount 
#   over 30 years. 
# 
# - Implication for modeling: find the latest year when the plan had a surplus)


## Amortization after 7/1/2015 AV(including 2014-15 plan year)
#
# - Any initial UAAL (after a period of surplus) or change  in UAAL due to gains/losses(including contribution gains/losses) is amortized 
#   over 20 years.
# 
# Surplus:
#   - For any year in which UCRP has a surplus, such surplus would be amortized as a level dollar amount over 30 years, and all prior UAAL 
#     amortization bases would be considered fully amortized.
#   - (intial surplus amortized over 30 years accroding to (11)?)
#   - Changes in surplus after the effective date due to actuarial gains/losses(including contribution gains and losses) is amortized as a 
#     level dollar amount over 15 years. 



# Amortization rule in the model  
# 1. UAAL    -> Surplus: All prior amortization basis are considered fully amortized. Initial surplus is amortized over 30 years.
# 2. Surplus -> Surplus: change in surplus is amortized over 15 years.
# 3. Surplus -> UAAL:    All prior amortization basis are considered fully amortized. Initial UAAL is amortized over 20 years. 
# 4. UAAL    -> UAAL:    change in UAAL is amortized over 20 years.



i <- 0.0725
m.UAAL0 <- 20
m.UAAL1 <- 20
m.surplus0 <- 30
m.surplus1 <- 15

init.AA <- 100

df <- data.frame(year = 2015:2064) %>% 
  mutate(AL = 100 * (1 + i)^(year - 2015),
         AA = ifelse(year == 2015, init.AA, 0),
         UAAL  = AA - AL,
         EUAAL = 0,
         i.r   = c(rep(0.06,5), rep(0.15,10),rep(0.0725,35)),
         Basis = 0,
         SC    = 0,
         switch = "")

SC_amort <- matrix(0, 50, 80)

for (j in 1:nrow(df)){
  
  
  if(j == 1)  df$EUAAL[j] <- 0 else
              df$EUAAL[j] <- with(df, (UAAL[j - 1] - SC[j - 1]) * (1 + i))
  
  if(j == 1)  df$AA[j] <- init.AA else
              df$AA[j] <- with(df, (AA[j - 1] + SC[j - 1]) * (1 + i.r[j - 1]))
  
  df$UAAL[j] <- with(df, AL[j] -  AA[j])
  
  # Set up mode-switching indicator for amortization basis. 
  
  if(j == 1) df$switch[j] <-  with(df, ifelse(UAAL[j]<0, "surplus0", "UAAL0")) else
             df$switch[j] <-  with(df, ifelse(UAAL[j]<0 & UAAL[j - 1]>=0, "surplus0",
                                              ifelse(UAAL[j]<0 & UAAL[j - 1]<0, "surplus1",
                                                     ifelse(UAAL[j]>=0 & UAAL[j - 1]<0, "UAAL0",
                                                            ifelse(UAAL[j]>=0 & UAAL[j - 1]>=0, "UAAL1", ""))))) 
  
  
  # if(j == 1) df$Basis[j] <- with(df, UAAL[j]) else
  #            df$Basis[j] <- with(df, UAAL[j] - EUAAL[j])
  # 
  # SC_amort[j, j:(j + m.UAAL - 1)] <- amort_cd(df$Basis[j], i, m.UAAL)
  # 
  # df$SC[j] <- sum(SC_amort[,j])

  if(j == 1) df$Basis[j] <- with(df, UAAL[j]) else
             df$Basis[j] <- with(df, ifelse(switch[j] %in% c("Surplus1", "UAAL1"), UAAL[j] - EUAAL[j], UAAL[j]))
  
  m <- switch(df$switch[j],
              surplus0 = m.surplus0,
              surplus1 = m.surplus0,
              UAAL0    = m.UAAL0,
              UAAL1    = m.UAAL1)
         
  SC_amort[j, j:(j + m - 1)] <- amort_cd(df$Basis[j], i, m)

  if(df$switch[j] %in% c("Surplus0", "UAAL0")) SC_amort[1:(j-1),] <- 0
  
  df$SC[j] <- sum(SC_amort[,j])
  
}
  

SC_amort




