######################################################
# Scenario: Low-level persistence
# Contents of Table 2
# Calculates n-year sensitivity of the proposed 
# Phases for detecting true elimination 
######################################################

# First: set working direction to source file location
# setwd("...")
load("SetUp.R")

######################################################
# Read simulation outputs
######################################################

filename <- "Results/mid_persistence.RDS" #change to select relevant file
data_list <- readRDS(filename)

max_cases <- data_list[[1]]
n_runs <- data_list[[2]]
incidence <- data_list[[3]]
diagnosed <- data_list[[4]]
diagnosed_child <- data_list[[5]]
last_infection <- data_list[[14]] 

######################################################
# Read in situations for calculating specificities
######################################################

filename <- "Results/specificities.RDS" #match to simulation file
situation <- readRDS(filename)

years <- 1985-burn_in+ts
situation <- as_tibble(situation) %>% mutate(year = years)

######################################################
# Table 2: n-year specificities
# Assume start observation in 2020 (after decline and
# during low-level persistence).
######################################################

# Time of first passing a Phase
first_1_to_2 <- c()
first_2_to_3 <- c()
first_3_to_4 <- c()

for(n in 1:n_runs){
  first_1_to_2[n] <- 1985+min(which(situation[,n]>=2))-1
  first_2_to_3[n] <-1985+min(which(situation[,n]>=3))-1
  first_3_to_4[n] <-1985+min(which(situation[,n]==4))-1
}

# Five year specificity
1-(sum(first_1_to_2<2025)/n_runs) #Phase 1
1-sum(first_2_to_3<2025)/n_runs #Phase 2
1-sum(first_3_to_4<2025)/n_runs #Phase 3

# Ten year specificity
1-sum(first_1_to_2<2030)/n_runs #Phase 1
1-sum(first_2_to_3<2030)/n_runs #Phase 2
1-sum(first_3_to_4<2030)/n_runs #Phase 3

# Fifteen year specificity
1-sum(first_1_to_2<2035)/n_runs #Phase 1
1-sum(first_2_to_3<2035)/n_runs #Phase 2
1-sum(first_3_to_4<2035)/n_runs #Phase 3

# Twenty year specificity
1-sum(first_1_to_2<2040)/n_runs #Phase 1
1-sum(first_2_to_3<2040)/n_runs #Phase 2
1-sum(first_3_to_4<2040)/n_runs #Phase 3
