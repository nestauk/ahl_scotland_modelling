rm(list = ls())

require("bw")
require("survey")
library(tidyverse)
library(here)
library(reshape2)
library(furrr)
library(tictoc)
library(beepr)

find_EI <- function(id, bw, ht, age, sex,weight_goal, days, ei_limit, pal){
  
  deltaNA <- rep(0,days)
  
  #Build a function to uniroot solve
  weight_solve_energy <- function(x){
    
    #Matrix of energy intake change for 365 days
    deltaEI <- rep(x, days)
    
    #Estimate weight change. 
    wtrajectory  <- adult_weight(bw, ht, age, sex, deltaEI, deltaNA, days = days, PAL = pal)
    
    #Return weight by the end
    wtrajectory$Body_Weight[days] -weight_goal
    
  }
  
  #Energy interval for the solution
  EI              <- c(-ei_limit, ei_limit)
  return(data.frame(id, days, ei = uniroot(weight_solve_energy, interval = EI)$root))
}
