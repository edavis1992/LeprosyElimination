library(tidyverse)
library(zoo)
library(tictoc)
library(arm)
library(gridExtra)

load('no_mi_tpd.Rd')
load("Functions.R")

######################################################
# Set parameters
######################################################

n_years <- 35
burn_in <- 40
index_year <- 2020 - burn_in - n_years
max_cases <- 20
n_runs <- 5000
ts <- (burn_in+1):(n_years+100)

# Thresholds for elimination stages
years_no_child <- 5
years_no_adult <- 3
years_nonendemic <- 10