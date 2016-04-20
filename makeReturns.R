# calculate compound mean returns for return series provided by Susie. 
# 4/19/2016
# Yimeng Yin

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library(xlsx)
library("btools")

source("Functions.R")

######################################################

create_returns <- function(r.mean, r.sd, period){
  i.r <- unlist(mapply(rnorm, period, r.mean, r.sd))
}


#1. 
r.mean <- c(0.05, 0.07, 0.08)
r.sd   <- c(0.1, 0.1, 0.1)
period <- c(10, 5, 15)

#set.seed(1234)
replicate(50000, create_returns(r.mean, r.sd, period) %>% get_geoReturn) %>% mean



#2. 

r.mean <- c(0.0500, 0.05375, 0.05750, 0.06125, 0.06500, 0.06875, 0.07750)
#r.mean <- c(0.0550, 0.05875, 0.06250, 0.06625, 0.0700, 0.07375, 0.07750)
r.sd   <- rep(0.1, 7)
period <- c(1,1,1,1,1,1,24)
replicate(50000, create_returns(r.mean, r.sd, period) %>% get_geoReturn) %>% mean

(1.05*1.05375*1.0575*1.06125*1.065*1.06875*1.0725^24)^(1/30)

# stochastic: 6.91%~6.94
# deterministic: 6.986%


#3. 
r.mean <- c(0.055, 0.065, 0.0775)
r.sd   <- c(0.1, 0.1, 0.1)
period <- c(5, 5, 20)

#set.seed(1234)
replicate(50000, create_returns(r.mean, r.sd, period) %>% get_geoReturn) %>% mean


(1.05^5*1.06^5*1.0725^20)^(1/30)

(1.062^30/(1.045^5*1.055^5))^(1/20)


# stochastic: 6.69~6.72%
# Deterministic: 6.66%


#4. 
r.mean <- c(0.06, 0.08)
r.sd   <- rep(0.1, 2)
period <- c(10,10)
replicate(50000, create_returns(r.mean, r.sd, period) %>% get_geoReturn) %>% mean

(1.055^10*1.075^10)^(1/20)

# stochastic: 6.54%~6.55%
# deterministic: 6.495%

#5. 
r.mean <- c(0.0775)
r.sd   <- rep(0.1, 1)
period <- c(30)
replicate(50000, create_returns(r.mean, r.sd, period) %>% get_geoReturn) %>% mean




r.mean <- c(0.0725)
r.sd   <- rep(0.1, 1)
period <- c(20)
replicate(20000, create_returns(r.mean, r.sd, period) %>% get_geoReturn) %>% mean







