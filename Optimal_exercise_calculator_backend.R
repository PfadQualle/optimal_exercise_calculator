## ---------------------------
##
## Script name: Optimal_exercise_calculator_backend.R
##
## Purpose of script: This script contains the mathematics behind optimal exercise levels
##
## Author: Niklas V. Lehmann
##
## Date Created: 2026-05-21
##
## Copyright (c) Niklas V. Lehmann, 2026
## Email: niklasl.2306@gmail.com
##
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------



## necessary packages 

library(readr)

## Setting and getting values

# --- Parameters ---
HOURS_WK  <- 168



# Function from Garcia et. al. (2023) https://bjsm.bmj.com/content/bjsports/57/15/979.full.pdf


garcia <- read_csv("~/Documents/Projekte/Kurzprojekte/Optimal_Exercise_Calculator/total-population-all-cause-mortality-fatal-q-0.95.csv")  # columns: dose, hr, hr_lo, hr_hi



# ---- The full function ------

optimal_exercise_all <- function(age = 29, 
                             conservativeness = 0.7,
                             alpha = 2.8*10^(-5),
                             beta = 0.085,
                             intensity = 5,
                             r = 0.03,
                             u_rel){
  
  
  HR <- approxfun(garcia$dose, garcia$RR, 
                  method = "linear",
                  yleft  = 1,                    # HR = 1 at zero dose
                  yright = tail(garcia$RR, 1))   # plateau beyond data
  
  HR_adj <- function(x){ 1- (conservativeness * (1-HR(x))) }
  
  
  DLE <- function(hours_exercised){
    
    #get mMET
    x <- hours_exercised*intensity
    
    #integrate over SURVIVAL_discounted function
    integrate(function(t){exp( -( (alpha*exp(beta*age)*HR_adj(x)) / beta) * (exp(beta*t)-1) ) * exp(-r*t)}
              ,lower=0, upper=150)$value
  }
  
  
  u <- function(hours_exercised){ (1*(HOURS_WK - hours_exercised) + hours_exercised*u_rel) / HOURS_WK } #non-exercise hours *1 + exercise hours * utility , divided by hours per week to normalize it
  
  
  optimize( function(hours_exercised){u(hours_exercised) * DLE(hours_exercised)}, c(0, 14), maximum=TRUE)

}


# This function only outputs the optimal exercise load
optimal_exercise_max <- function(age = 29, 
                             conservativeness = 0.7,
                             alpha = 2.8*10^(-5),
                             beta = 0.085,
                             intensity = 5,
                             r = 0.03,
                             u_rel){
  
  
  HR <- approxfun(garcia$dose, garcia$RR, 
                  method = "linear",
                  yleft  = 1,                    # HR = 1 at zero dose
                  yright = tail(garcia$RR, 1))   # plateau beyond data
  
  HR_adj <- function(x){ 1- (conservativeness * (1-HR(x))) }
  
  
  DLE <- function(hours_exercised){
    
    #get mMET
    x <- hours_exercised*intensity
    
    #integrate over SURVIVAL_discounted function
    integrate(function(t){exp( -( (alpha*exp(beta*age)*HR_adj(x)) / beta) * (exp(beta*t)-1) ) * exp(-r*t)}
              ,lower=0, upper=150)$value
  }
  
  
  u <- function(hours_exercised){ (1*(HOURS_WK - hours_exercised) + hours_exercised*u_rel) / HOURS_WK } #non-exercise hours *1 + exercise hours * utility , divided by hours per week to normalize it
  
  
  optimize( function(hours_exercised){u(hours_exercised) * DLE(hours_exercised)}, c(0, 14), maximum=TRUE)$maximum
  
}



# This function only outputs the expected discounted quality-adjusted life years
optimal_exercise_QALY <- function(age = 29, 
                                 conservativeness = 0.7,
                                 alpha = 2.8*10^(-5),
                                 beta = 0.085,
                                 intensity = 5,
                                 r = 0.03,
                                 u_rel){
  
  
  HR <- approxfun(garcia$dose, garcia$RR, 
                  method = "linear",
                  yleft  = 1,                    # HR = 1 at zero dose
                  yright = tail(garcia$RR, 1))   # plateau beyond data
  
  HR_adj <- function(x){ 1- (conservativeness * (1-HR(x))) }
  
  
  DLE <- function(hours_exercised){
    
    #get mMET
    x <- hours_exercised*intensity
    
    #integrate over SURVIVAL_discounted function
    integrate(function(t){exp( -( (alpha*exp(beta*age)*HR_adj(x)) / beta) * (exp(beta*t)-1) ) * exp(-r*t)}
              ,lower=0, upper=150)$value
  }
  
  
  u <- function(hours_exercised){ (1*(HOURS_WK - hours_exercised) + hours_exercised*u_rel) / HOURS_WK } #non-exercise hours *1 + exercise hours * utility , divided by hours per week to normalize it
  
  
  optimize( function(hours_exercised){u(hours_exercised) * DLE(hours_exercised)}, c(0, 14), maximum=TRUE)$objective
  
}


#### Answering some questions


# sweep a variable by trying a lot of values
seq_u <- seq(-1,1,0.01)


# sweep a variable by trying a lot of values
seq_r <- seq(0,0.12,0.001)


# ------ How much exercise should people do as a function of u(), i.e. how much they hate it? ------- #


#make plots
plot(seq_u, 
     sapply(seq_u,function(k) optimal_exercise_max(u_rel = k, r=0.03, age = 40)),
     type = "l",
     xlab = "utility from exercise",
     ylab = "Weekly hours exercised at mMET=5")









# ------ How valuable is it to find a sport that is more fun? ------- #

#make plots
plot(seq_u, 
     sapply(seq_u,function(k) optimal_exercise_QALY(u_rel = k, r=0, age = 40)),
     type = "l",
     xlab = "utility from exercise",
     ylab = "Remaining quality-adjusted Life Expectancy")




# ------ What is the effect of discounting on optimal exercise? ------- #


#make plots
plot(seq_r, 
     sapply(seq_r,function(k) optimal_exercise_max(u_rel = 0.9, r=k, age = 40)),
     type = "l",
     xlab = "discount rate",
     ylab = "Weekly hours of exercise at mMET=5")


