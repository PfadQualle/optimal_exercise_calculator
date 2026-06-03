## ---------------------------
##
## Script name: Optimal_exercise_calculator.R
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
BETA_YR   <- 0.085          # Gompertz mortality doubling rate (~doubles every 8 yr)
BETA_WK   <- BETA_YR / 52   # weekly equivalent
HOURS_YR  <- 8766
HOURS_WK  <- 168
ALPHA <- 2.8*10^(-5)

# --- Inputs ---

AGE <- 29
INTENSITY <- 5 #in mMET
R <- 0.0 #Discount rate: use 6% for typical adult, 0% for long-term focused adult, 12% for short-term focused adult
U_REL <- -1
CONSERVATIVENESS <- 0.7


# Function from Garcia et. al. (2023)


garcia <- read_csv("~/Documents/Projekte/Kurzprojekte/Optimal_Exercise_Calculator/total-population-all-cause-mortality-fatal-q-0.95.csv")  # columns: dose, hr, hr_lo, hr_hi

HR <- approxfun(garcia$dose, garcia$RR, 
                          method = "linear",
                          yleft  = 1,                    # HR = 1 at zero dose
                          yright = tail(garcia$RR, 1))   # plateau beyond data

HR_adj <- function(x){ 1- (CONSERVATIVENESS * (1-HR(x))) }


# --- Function: Survival function, conditional on age and being alive ---

SURVIVAL_discounted <- function(t){exp( -( (ALPHA*exp(BETA_YR*AGE)*HR_adj(x)) / BETA_YR) * (exp(BETA_YR*t)-1) ) * exp(-R*t)}

# --- Function: Discounted Life Expectancy ---

DLE <- function(hours_exercised){
    
    #get mMET
    x <- hours_exercised*INTENSITY
    
    #integrate over SURVIVAL_discounted function
    integrate(function(t){exp( -( (ALPHA*exp(BETA_YR*AGE)*HR_adj(x)) / BETA_YR) * (exp(BETA_YR*t)-1) ) * exp(-R*t)}
,lower=0, upper=150)$value
  }
  


# --- Function: Utility from additional life ---


u <- function(hours_exercised){ (1*(HOURS_WK - hours_exercised) + hours_exercised*U_REL) / HOURS_WK } #non-exercise hours *1 + exercise hours * utility , divided by hours per week to normalize it




# --- Solve for optimal exercise levels ---

optimize( function(hours_exercised){u(hours_exercised) * DLE(hours_exercised)}, c(0, 14), maximum=TRUE)

round(optimize( function(hours_exercised){u(hours_exercised) * DLE(hours_exercised)}, c(0, 14), maximum=TRUE)$maximum, digits = 1)
 
# ---- GET expected utility -----
round(optimize( function(hours_exercised){u(hours_exercised) * DLE(hours_exercised)}, c(0, 14), maximum=TRUE)$maximum, digits = 1)



# ---- The full function ------

optimal_exercise <- function(age = 29, 
                             conservativeness = 0.7,
                             alpha = 2.8^10(-5),
                             beta = 0.085,
                             intensity = 5,
                             r = 0.03,
                             u_rel){
  
  DLE <- function(hours_exercised){
    
    #get mMET
    x <- hours_exercised*intensity
    
    #integrate over SURVIVAL_discounted function
    integrate(function(t){exp( -( (alpha*exp(beta*age)*HR_adj(x)) / beta) * (exp(beta*t)-1) ) * exp(-R*t)}
              ,lower=0, upper=150)$value
  }
  
  
  u <- function(hours_exercised){ (1*(HOURS_WK - hours_exercised) + hours_exercised*u_rel) / HOURS_WK } #non-exercise hours *1 + exercise hours * utility , divided by hours per week to normalize it
  
  
  optimize( function(hours_exercised){u(hours_exercised) * DLE(hours_exercised)}, c(0, 14), maximum=TRUE)

}


