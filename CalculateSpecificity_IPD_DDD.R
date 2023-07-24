######################################################
# Scenario: Sensitivity analysis of distributions
# Data for Figure S2
# Calculates n-year sensitivity of the proposed 
# Phases for detecting true elimination 
######################################################

ipds <- c("lowipd", "midipd", "highipd")
ddds <- c("lowddd", "midddd", "highddd")
filenames <- matrix(NA, 3, 3)
for(i in 1:3) {
  for(j in 1:3){
    filenames[i,j] <- paste("Results/situation_",
                            ipds[i], "_", ddds[j], ".RDS", 
                            sep = "")
  }
}

filenames <- as.vector(filenames)

specificities_sens10 <- tibble(index = 1:9,
                               ipd = rep(c("low","mid","high"),3),
                               ddd = c(rep("low",3), rep("mid", 3), rep("high", 3)),
                               phase1 = rep(NA, 9),
                               phase2 = rep(NA, 9),
                               phase3 = rep(NA, 9))

specificities_sens20 <- tibble(index = 1:9,
                               ipd = rep(c("low","mid","high"),3),
                               ddd = c(rep("low",3), rep("mid", 3), rep("high", 3)),
                               phase1 = rep(NA, 9),
                               phase2 = rep(NA, 9),
                               phase3 = rep(NA, 9))

for(i in seq_along(filenames)){
  
  filename <- filenames[i]
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
  specificities_sens10$phase1[i] <- 1-sum(first_1_to_2<2030)/n_runs #Phase 1
  specificities_sens10$phase2[i] <- 1-sum(first_2_to_3<2030)/n_runs #Phase 2
  specificities_sens10$phase3[i] <- 1-sum(first_3_to_4<2030)/n_runs #Phase 3
  
  # Twenty year specificity
  specificities_sens20$phase1[i] <- 1-sum(first_1_to_2<2040)/n_runs #Phase 1
  specificities_sens20$phase2[i] <- 1-sum(first_2_to_3<2040)/n_runs #Phase 2
  specificities_sens20$phase3[i] <- 1-sum(first_3_to_4<2040)/n_runs #Phase 3
  
}

saveRDS(specificities_sens10, "Results/FigureS2.RDS")
saveRDS(specificities_sens20, "Results/FigureS3.RDS")

