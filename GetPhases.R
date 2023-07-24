# First: set working directory to source file location
# setwd("...")
source("SetUp.R")

######################################################
# PHASES FOR LOW LEVEL PERSISTENCE / SPECIFICITIES
# Read simulation outputs
######################################################

filename <- "Results/persistence10.RDS" #change to select relevant file
data_list <- readRDS(filename)

max_cases <- data_list[[1]]
n_runs <- data_list[[2]]
incidence <- data_list[[3]]
diagnosed <- data_list[[4]]
diagnosed_child <- data_list[[5]]
last_infection <- data_list[[14]]
ts <- (burn_in+1):(n_years+100)

######################################################
# Calculate transititions and reversions
# through the Phases.
######################################################

out <- GetSituation(n_runs, ts, diagnosed, diagnosed_child,
                    years_no_child, years_no_adult, 
                    years_nonendemic)

years <- 1985-burn_in+ts
situation <- as_tibble(out[[6]]) %>% mutate(year = years)

######################################################
# Save outputs: Persistence/specificities
######################################################

filename <- "Results/situation10.RDS"
#saveRDS(situation, file = filename)





