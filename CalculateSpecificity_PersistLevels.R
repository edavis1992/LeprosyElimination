######################################################
# Scenario: Range of persistence
# Data for Figure 3
# Calculates n-year sensitivity of the proposed 
# Phases for detecting true elimination 
######################################################

inds <- c("01","02","03","04","05","06","07","08","09",10:20)
specificities_range <- tibble(index = 1:20,
                              incidence = max_cases*(1:20)/100,
                              phase1 = rep(NA,20),
                              phase2 = rep(NA,20),
                              phase3 = rep(NA,20))

for(i in 1:20){
  
  filename <- paste("Results/situation",inds[i],".RDS",sep="")
  situation <- readRDS(filename)
  situation <- as_tibble(situation) %>% mutate(year = 1985-burn_in+ts)
  
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
  
  # Ten year specificity
  specificities_range$phase1[i] <- 1-sum(first_1_to_2<2030)/n_runs #Phase 1
  specificities_range$phase2[i] <- 1-sum(first_2_to_3<2030)/n_runs #Phase 2
  specificities_range$phase3[i] <- 1-sum(first_3_to_4<2030)/n_runs #Phase 3
  
}

saveRDS(specificities_range, "Results/Figure4.RDS")