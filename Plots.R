# Plots for Leprosy paper
load("SetUp.R")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

######################################################
# Figure 1: Incubation and Detection Delay Distributions
######################################################

set.seed(20221129)
N <- 10000000
delays <- rgamma(N,shape=ddd.fit$estimate[1],rate=ddd.fit$estimate[2])
delays <- tibble(delays=delays)
incubation <- rgamma(N,shape=ipd.fit$estimate[1],rate=ipd.fit$estimate[2])
incubation <- tibble(inc=incubation)

# Figure 1a
incubation %>% ggplot(aes(x=inc,y = ..density..)) + geom_histogram(bins=1000,colour=cbPalette[4]) + geom_density(size=1.1) +
  labs(x="incubation period (years)") + theme_minimal(base_size=14)

# Figure 1b
delays %>% ggplot(aes(x=delays,y = ..density..)) + geom_histogram(bins=1000,colour=cbPalette[3]) + geom_density(size=1.1) +
           labs(x="detection delay (years)") + theme_minimal(base_size=14)


######################################################
# Figure 3: 10 year specificity by mean annual incidence
######################################################

persist <- seq(0.01,0.2,0.01)
persist_names1 <- paste("Results/persistence0",c(1:9),".RDS",sep="")
persist_names2 <- paste("Results/persistence",10:20,".RDS",sep="")
persist_names <- c(persist_names1,persist_names2)

# Proportion that achieve: 2, 3, 4 in a 20 year period
achieve2 <- matrix(NA,20,n_runs)
achieve3 <- matrix(NA,20,n_runs)
achieve4 <- matrix(NA,20,n_runs)
prob_achieve2 <- c()
prob_achieve3 <- c()
prob_achieve4 <- c()

for(per in 1:20){
  data_list <- readRDS(persist_names[per])
  incidence <- data_list[[3]]
  diagnosed <- data_list[[4]]
  diagnosed_child <- data_list[[5]]
  first_zero <- data_list[[10]]
  last_case <- data_list[[11]]
  first_child_zero <- data_list[[12]]
  last_child_case <- data_list[[13]]
  last_infection <- data_list[[14]]
  
  out <- GetSituation(n_runs, ts, diagnosed, diagnosed_child,
                      years_no_child, years_no_adult, 
                      years_nonendemic)
  
  revert_4_to_2 <- out[[1]]
  revert_4_to_1 <- out[[2]]
  revert_3_to_2 <- out[[3]]
  revert_3_to_1 <- out[[4]]
  revert_2_to_1 <- out[[5]]
  situation <- out[[6]]
  
  # Proportion that achieve: 2, 3, 4 in a 20 year period
  for(m in 1:n_runs){
    achieve2[per,m] <- any(situation[37:56,m]>=2)
    achieve3[per,m] <- any(situation[37:56,m]>=3)
    achieve4[per,m] <- any(situation[37:56,m]>=4)
  }
  
  prob_achieve2[per] <- mean(achieve2[per,])
  prob_achieve3[per] <- mean(achieve3[per,])
  prob_achieve4[per] <- mean(achieve4[per,])
  
}

df <- tibble(persistence = persist, prob2 = prob_achieve2, prob3 = prob_achieve3, prob4 = prob_achieve4)

df %>% ggplot(aes(x=persist*max_cases,y=1-prob2)) + geom_line(aes(col="Phase 1",lty="Phase 1"),lwd=1.5) +
  geom_line(aes(y=1-prob3, col="Phase 2",lty="Phase 2"),lwd=1.5) +
  geom_line(aes(y=1-prob4, col="Phase 3",lty="Phase 3"),lwd=1.5) +
  labs(x = "Persistence level (mean annual incidence)", y = "10 year specificity", color = "Phase:",linetype="Phase:") + 
  scale_color_manual(values=cbPalette[c(7,2,4)]) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom",legend.key.width = unit(1.5, 'cm')) +
  guides(color = guide_legend(ncol=1))


