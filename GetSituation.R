######################################################
# Function to derive Phases from detected cases
######################################################

GetSituation <- function(n_runs, ts,
                         diagnosed, diagnosed_child,
                         years_no_child,
                         years_no_adult,
                         years_nonendemic){
  
  last_case <- c()
  last_child_case <- c()
  first_zero <- c()
  first_child_zero <- c()
  situation <- matrix(1,nrow=length(ts),ncol=n_runs)
  revert_4_to_2 <- rep(0,n_runs)
  revert_4_to_1 <- rep(0,n_runs)
  revert_3_to_2 <- rep(0,n_runs)
  revert_3_to_1 <- rep(0,n_runs)
  revert_2_to_1 <- rep(0,n_runs)
  time_4_to_2 <- rep(0,n_runs)
  time_4_to_1 <- rep(0,n_runs)
  time_3_to_2 <- rep(0,n_runs)
  time_3_to_1 <- rep(0,n_runs)
  time_2_to_1 <- rep(0,n_runs)
  
  for (j in 1:n_runs){
    
    obs_cases <- diagnosed[ts,j]
    obs_child <- diagnosed_child[ts,j]
    
    first_zero[j] <- 1985+min(which(obs_cases==0))-1 #first zero
    last_case[j] <- 1985+max(which(obs_cases>0))-1 #last case detection
    first_child_zero[j] <- 1985+min(which(obs_child==0))-1 #first zero
    last_child_case[j] <- 1985+max(which(obs_child>0))-1 #last case detection
    
    rollmean_child <- rollmean(obs_child, k = years_no_child, fill = NA, align="right")
    rollmean_all <- rollmean(obs_cases, k = years_no_adult, fill = NA, align="right")
    rollmean_10 <- rollmean(obs_cases, k = years_nonendemic, fill = NA, align="right")
    
    for(year in seq_along(ts)){
      
      if(year %in% 1:4) next
      if(year == length(ts)) next
      
      if(situation[year,j]==4) {#either stay put or revert (>2 cases in 1 year)
        if((obs_cases[year]>2) & (obs_child[year]>2)){
          situation[(year):nrow(situation),j] <- 1 #revert to phase 2 the same year if more than 2 cases total
          revert_4_to_1[j] <- revert_4_to_1[j]+1
        }
        if((obs_cases[year]>2) & (obs_child[year]<=2)){
          situation[(year):nrow(situation),j] <- 1 #revert to phase 2 the same year if more than 2 cases total
          revert_4_to_2[j] <- revert_4_to_2[j]+1
        }
      }
      
      if(situation[year,j]==3) {#either progress (10 years no cases) or revert (>2 cases in 1 year)
        if(mean(situation[(year-9):year,j]) == 3){ #check been in phase 3 for at least 10 years
          situation[(year+1):nrow(situation),j] <- 4 #progress the following year
        }
        if((obs_cases[year]>2) & (obs_child[year]>2)){
          situation[(year):nrow(situation),j] <- 1 #revert the same year if more than 2 cases total
          revert_3_to_1[j] <- revert_3_to_1[j] + 1
        }
        if((obs_cases[year]>2) & (obs_child[year]<=2)){
          situation[(year):nrow(situation),j] <- 2 #revert the same year if more than 2 cases total
          revert_3_to_2[j] <- revert_3_to_2[j] + 1
        }
      }
      
      if(situation[year,j]==2) { #either progress (3 years no cases) or revert (>2 child cases in 1 year)
        if(rollmean_all[year]==0) {
          situation[(year+1):nrow(situation),j] <- 3 #progress the following year if three years no adult cases
        }
        if(obs_child[year]>2){
          situation[(year):nrow(situation),j] <- 1 #revert the same year if more than 2 child cases
          revert_2_to_1[j] <- revert_2_to_1[j]+1
        }
      }
      
      if(situation[year,j]==1) { #either progress (5 years no child cases) or stay put:  
        if(rollmean_child[year]==0) situation[(year+1):nrow(situation),j] <- 2 #progress the following year if five years no child cases
        if(rollmean_all[year]==0) situation[(year+1):nrow(situation),j] <- 3 #progress to phase 3 instead if also three years no adult cases
      }      
      
    }
    
  }
  
  return(list(revert_4_to_2, revert_4_to_1,
              revert_3_to_2, revert_3_to_1,
              revert_2_to_1, situation))
}
