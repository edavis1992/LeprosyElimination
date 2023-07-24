######################################################
# Scenario: Exponential Decline to Zero
# Contents of Table 1
# Calculates n-year sensitivity of the proposed 
# Phases for detecting true elimination 
######################################################

# First: set working direction to source file location
# setwd("...")
source("SetUp.R")

######################################################
# Read simulation outputs
######################################################

filename <- "Results/exp_decline.RDS" #change to select relevant file
data_list <- readRDS(filename)

max_cases <- data_list[[1]]
n_runs <- data_list[[2]]
incidence <- data_list[[3]]
diagnosed <- data_list[[4]]
diagnosed_child <- data_list[[5]]
last_infection <- data_list[[14]]

######################################################
# Read in situations for calculating sensitivities
######################################################

filename <- "Results/situation_exp_decline.RDS" #match to simulation file
situ_list <- readRDS(filename)

situ_5_vec <- situ_list[[1]]
situ_10_vec <- situ_list[[2]]
situ_15_vec <- situ_list[[3]]
situ_20_vec <- situ_list[[4]]
revert_4_to_2 <- situ_list[[5]]
revert_4_to_1 <- situ_list[[6]]
revert_3_to_2 <- situ_list[[7]]
revert_3_to_1 <- situ_list[[8]]
revert_2_to_1 <- situ_list[[9]]
situation <- situ_list[[10]]

######################################################
# Calculate n year sensitivities of:
# Phase 1 (>=2) - 5 years no child cases
# Phase 2 (>=3) - 3 years no cases
# Phase 3 (==4) - 10 years only sporadic cases
# i.e. what proportion achieve Phase n years after
# final transmission event
######################################################

sensitivities <- tibble(year = c(0, 5, 10, 15, 20),
                        phase1 = rep(NA, 5),
                        phase2 = rep(NA, 5),
                        phase3 = rep(NA, 5))

# 5 year
sensitivities$phase1[2] <- sum(situ_5_vec>=2)/n_runs # Phase 1
sensitivities$phase2[2] <- sum(situ_5_vec>=3)/n_runs # Phase 2
sensitivities$phase3[2] <- sum(situ_5_vec==4)/n_runs # Phase 3

# 10 year
sensitivities$phase1[3] <- sum(situ_10_vec>=2)/n_runs # Phase 1
sensitivities$phase2[3] <- sum(situ_10_vec>=3)/n_runs # Phase 2
sensitivities$phase3[3] <- sum(situ_10_vec==4)/n_runs # Phase 3

# 15 year
sensitivities$phase1[4] <- sum(situ_15_vec>=2)/n_runs # Phase 1
sensitivities$phase2[4] <- sum(situ_15_vec>=3)/n_runs # Phase 2
sensitivities$phase3[4] <- sum(situ_15_vec==4)/n_runs # Phase 3

# 20 year
sensitivities$phase1[5] <- sum(situ_20_vec>=2)/n_runs # Phase 1
sensitivities$phase2[5] <- sum(situ_20_vec>=3)/n_runs # Phase 2
sensitivities$phase3[5] <- sum(situ_20_vec==4)/n_runs # Phase 3

######################################################
# Calculate 1-specificity:
# i.e. proportion achieving a Phase whilst transmission
# is still ongoing (not yet eliminated)
# Phase 1 (>=2) - 5 years no child cases
# Phase 2 (>=3) - 3 years no cases
# Phase 3 (==4) - 10 years only sporadic cases
######################################################

first_1_to_2 <- c()
first_2_to_3 <- c()
first_3_to_4 <- c()

for(n in 1:n_runs){
  first_1_to_2[n] <- 1985+min(which(situation[,n]>=2))-1
  first_2_to_3[n] <-1985+min(which(situation[,n]>=3))-1
  first_3_to_4[n] <-1985+min(which(situation[,n]==4))-1
}

# <0 year sensitivity (= 1 - specificity)
sensitivities$phase1[1] <- sum((first_1_to_2-last_infection)<0)/5000 # Phase 1
sensitivities$phase2[1] <- sum((first_2_to_3-last_infection)<0)/5000 # Phase 2
sensitivities$phase3[1] <- sum((first_3_to_4-last_infection)<0)/5000 # Phase 3

saveRDS(sensitivities, "Results/sensitivities.RDS")
