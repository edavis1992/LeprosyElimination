# First: set working direction to source file location
# setwd("...")
load("SetUp.R")

######################################################
# Load in fitted incubation and delay distributions
######################################################

tpd_pars <- list( "ipd_shape"=ipd.fit$estimate["shape"],
                  "ipd_rate"=ipd.fit$estimate["rate"],
                  "ddd_shape"=ddd.fit$estimate["shape"],
                  "ddd_rate"=ddd.fit$estimate["rate"] )

######################################################
# Set up population age structure
# Data/counts are in 5 year intervals
# want to extend to 1 year intervals
# and the normalise to get frequency
######################################################
age_count <- c(199911,181492,159428,138264,120031,104678,91535,78352,64362,52212,
               42201,34235,26802,19782,13372,8018,4040,1496,337)
grads <- c()
age_cats <- seq(2.5,92.5,5)
ages <- c()
intercepts <- c()
ys <- c()
for(i in seq_along(age_count)){
  grads[i] <- ifelse(i<length(age_count),(age_count[i+1]-age_count[i])/5,-age_count[i]/5)
  intercepts[i] <- age_count[i]-grads[i]*age_cats[i]
  ys <- c(ys,grads[i]*(age_cats[i]-0.5+(0:4))+intercepts[i])
  ages <- c(ages,age_cats[i]-0.5+(0:4))
}
ys <- c(grads[1]*(0:1)+intercepts[1],ys)
ages <- c(0:1,ages)

# normalise to proportion of population
age_dist <- ys/sum(ys)

# probability die during each year
death_prob <- c((ys[1:96]-ys[2:97])/ys[1:96],0.4)
survive_prob <- 1-death_prob
prob_die_age_n <- death_prob*cumprod(survive_prob)

prop_child <- sum(age_dist[1:15])/sum(age_dist) #under 15
prop_adult <- sum(age_dist[16:length(age_dist)])/sum(age_dist) #15 and over

######################################################
# Calculate true incidence and case detection incidence
# for n_runs simulations
######################################################

# Low-level persistence
# Average annual incidence = max_cases x persist_level
# Run for values in seq(0,0.2,0.01)
# i.e. a mean of 0 to 4 new cases per year
persist_level <- 0
filename <- "Results/exp_decline.RDS" # match file name to persistence level

incidence <- matrix(0,n_years+burn_in+100,n_runs)
diagnosed <- matrix(0,n_years+100,n_runs)
diagnosed_adult <- matrix(0,n_years+100,n_runs)
diagnosed_child <- matrix(0,n_years+100,n_runs)
last_infection <- c()

set.seed(20220804)

tic()
for(iter in 1:n_runs){
  
  # Calculate true incidence
  incidence[,iter] <- rbinom(n_years+burn_in+100,max_cases,
                             c(rep(0.5,burn_in),
                               pmin(exp(-((3:43)/5))+persist_level,0.5),
                               rep(persist_level,100)))
  last_infection[iter] <- (1985-burn_in-1)+max(which(incidence[,iter]>0))
  n_cases <- sum(incidence[,iter])
  
  #generate tibble "cases" with year of birth (dob) and year infected
  cases <- c()
  cases <- tibble(dob=NA,year_infected=NA)
  dates_of_death <- c()
  for(i in seq_along(incidence[,iter])){
    age_of_new_cases <- rmultinom(1,incidence[i,iter],age_dist)
    this_year <- index_year-1+i
    for(age_index in seq_along(age_of_new_cases)){
      num_cases <- age_of_new_cases[age_index]
      if(num_cases==0) next
      age_infected = age_index-1
      dob = this_year - age_infected
      for(j in 1:num_cases){
        dates_of_death <- c(dates_of_death,
                            this_year + (which(rmultinom(1,1,prob_die_age_n[age_index:length(prob_die_age_n)])!=0)-1))
      }
      while(num_cases>0){
        
        cases <- cases %>% add_row(dob=dob,year_infected=this_year,)
        num_cases <- num_cases-1
      }
    }
    
  }
  cases <- cases %>% filter(!is.na(dob)) %>% mutate(dod = dates_of_death)
  
  # Calculate incubation periods and detection delays for each case
  ipd <- rgamma(n_cases,shape=tpd_pars$ipd_shape,rate=tpd_pars$ipd_rate)
  ddd <- rgamma(n_cases,shape=tpd_pars$ddd_shape,rate=tpd_pars$ddd_rate)
  
  # Calculate year detected (if die before detection, then never detected)
  # and age detected (adult/child case for >=/<15 years)
  cases <- cases %>% mutate(year_symptoms=year_infected+ipd,
                            year_detect=year_symptoms+ddd)
  cases <- cases %>% mutate(observed = (dod-year_detect)>=0)
  cases <- cases %>% filter(observed==TRUE)
  cases <- cases %>% mutate(age_detected = year_detect - dob)
  cases <- cases %>% mutate(adult = age_detected >= 15)
  
  # Go through each year and work out how many cases detected that year
  for (i in 1:nrow(diagnosed_adult)){
    
    this_year <- index_year-1+i
    #infected <- cases %>% filter(year_infected==this_year)
    detected <- cases %>% filter(year_detect > this_year & year_detect < (this_year+1))
    detected_adult <- detected %>% filter(adult==TRUE)
    detected_child <- detected %>% filter(adult==FALSE)
    
    annual_adult <- nrow(detected_adult)
    annual_child <- nrow(detected_child)
    
    diagnosed_adult[i,iter] <- diagnosed_adult[i,iter]+annual_adult
    diagnosed_child[i,iter] <- diagnosed_child[i,iter]+annual_child
    
  }
}
toc()

# total diagnosed cases for each year in each simulation
diagnosed <- diagnosed_adult+diagnosed_child

######################################################
# Save outputs
######################################################

# saveRDS(list(max_cases, n_runs, incidence,
#              diagnosed, diagnosed_child,
#              last_infection), filename)


