# First: set working directory to source file location
# setwd("...")
source("SetUp.R")

######################################################
# GET PHASES FOR EXPONENTIAL DECLINE / SENSITIVITIES
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
# Calculate what Phase has been achieved by
# n years after the final infection event
######################################################

# reference for n years after last infection event
rel_to_last_5 <- last_infection+5
rel_to_last_10 <- last_infection+10
rel_to_last_15 <- last_infection+15
rel_to_last_20 <- last_infection+20

# situation (i.e. Phase) n years after last infection event
situ_5 <- c()
situ_10 <- c()
situ_15 <- c()
situ_20 <- c()

# Calculate (takes ~11 minutes)
tic()
for(i in 1:n_runs){
  year_5 <- situation %>% filter(year==rel_to_last_5[i])
  situ_5[i] <- year_5[i]
  year_10 <- situation %>% filter(year==rel_to_last_10[i])
  situ_10[i] <- year_10[i]
  year_15 <- situation %>% filter(year==rel_to_last_15[i])
  situ_15[i] <- year_15[i]
  year_20 <- situation %>% filter(year==rel_to_last_20[i])
  situ_20[i] <- year_20[i]
}
toc()

######################################################
# Save outputs: Exponential decline/sensitivities
######################################################

filename <- "Results/situation_exp_decline.RDS"
# saveRDS(c(list(situ_5_vec, situ_10_vec, situ_15_vec,
#              situ_20_vec), out),
#         file = filename)
