######################################################
# Scenario: Low-level persistence
# Contents of Table 2
# Calculates n-year sensitivity of the proposed 
# Phases for detecting true elimination 
######################################################

# First: set working direction to source file location
# setwd("...")
source("SetUp.R")

######################################################
# Read simulation outputs
######################################################

filename <- "Results/persistence10.RDS" 
data_list <- readRDS(filename)

max_cases <- data_list[[1]]
n_runs <- data_list[[2]]
incidence <- data_list[[3]]
diagnosed <- data_list[[4]]
diagnosed_child <- data_list[[5]]
last_infection <- data_list[[14]] 

######################################################
# Read in situations for calculating specificities
# Low-level persistence (2 new infections/year)
######################################################

filename <- "Results/situation10.RDS" #match to simulation file
situation <- readRDS(filename)

years <- 1985-burn_in+ts
situation <- as_tibble(situation) %>% mutate(year = years)

######################################################
# Table 2: n-year specificities
# Assume start observation in 2020
# Allow back-dating of phases based on historic data
# (after decline and during low-level persistence).
######################################################

situation <- situation %>% filter(year >= 2020)

# Time of first passing a Phase after 2020
first_1_to_2 <- c()
first_2_to_3 <- c()
first_3_to_4 <- c()

for(n in 1:n_runs){
  first_1_to_2[n] <- situation$year[min(which(situation[,n]>=2))]
  first_2_to_3[n] <-situation$year[min(which(situation[,n]>=3))]
  first_3_to_4[n] <-situation$year[min(which(situation[,n]==4))]
}

first_1_to_2[which(is.na(first_1_to_2))] <- 2080
first_2_to_3[which(is.na(first_2_to_3))] <- 2080
first_3_to_4[which(is.na(first_3_to_4))] <- 2080

specificities <- tibble(period = c(5, 10, 15, 20),
                        phase1 = rep(NA,4),
                        phase2 = rep(NA,4),
                        phase3 = rep(NA,4))

# Five year specificity
specificities$phase1[1] <- 1-(sum(first_1_to_2<2025)/n_runs) #Phase 1
specificities$phase2[1] <- 1-sum(first_2_to_3<2025)/n_runs #Phase 2
specificities$phase3[1] <- 1-sum(first_3_to_4<2025)/n_runs #Phase 3

# Ten year specificity
specificities$phase1[2] <- 1-sum(first_1_to_2<2030)/n_runs #Phase 1
specificities$phase2[2] <- 1-sum(first_2_to_3<2030)/n_runs #Phase 2
specificities$phase3[2] <- 1-sum(first_3_to_4<2030)/n_runs #Phase 3

# Fifteen year specificity
specificities$phase1[3] <- 1-sum(first_1_to_2<2035)/n_runs #Phase 1
specificities$phase2[3] <- 1-sum(first_2_to_3<2035)/n_runs #Phase 2
specificities$phase3[3] <- 1-sum(first_3_to_4<2035)/n_runs #Phase 3

# Twenty year specificity
specificities$phase1[4] <- 1-sum(first_1_to_2<2040)/n_runs #Phase 1
specificities$phase2[4] <- 1-sum(first_2_to_3<2040)/n_runs #Phase 2
specificities$phase3[4] <- 1-sum(first_3_to_4<2040)/n_runs #Phase 3

saveRDS(specificities, "Results/specificities.RDS")
