# Plots for Leprosy paper
source("SetUp.R")

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
F1a <- incubation %>% ggplot(aes(x=inc,y = ..density..)) + geom_histogram(bins=1000,colour=cbPalette[4]) + geom_density(size=1.1) +
  labs(x="incubation period (years)") + theme_minimal(base_size=14)

# Figure 1b
F1b <- delays %>% ggplot(aes(x=delays,y = ..density..)) + geom_histogram(bins=1000,colour=cbPalette[3]) + geom_density(size=1.1) +
           labs(x="detection delay (years)") + theme_minimal(base_size=14)


plot_grid(F1a,F1b, ncol = 2, labels = c("(a)","(b)"))

######################################################
# Figure 4: 10 year specificity by mean annual incidence
######################################################

specificity <- readRDS("Results/Figure4.RDS")
specificity <- rbind(tibble(index = 0, incidence = 0, 
                            phase1 = 0, phase2 = 0, phase3 = 0),
                     specificity)
df <- tibble(persist_level = rep(specificity$incidence, 3),
             phase = c(rep("Phase 1",21),rep("Phase 2",21),rep("Phase 3",21)),
             specificity = c(specificity$phase1,
                              specificity$phase2,
                              specificity$phase3))

F3 <- df %>% ggplot(aes(x = persist_level,y = specificity, col = phase, lty = phase)) + 
  geom_line(lwd = 1.5) +
  labs(x = "Persistence level (mean annual incidence)", y = "10-year specificity", color = "Phase:",linetype="Phase:") + 
  scale_color_manual(values=cbPalette[c(7,2,4)]) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom",legend.key.width = unit(1.5, 'cm')) +
  guides(color = guide_legend(ncol=1))



######################################################
# Figure S2: 10 year specificity 
# (sensitivity analysis of ipd and ddd)
######################################################

df_sens <- readRDS("Results/FigureS2.RDS")
df_sens <- df_sens %>% mutate(ipd = factor(ipd, levels = c("low","mid","high")))
df_sens <- df_sens %>% mutate(ddd = factor(ddd, levels = c("low","mid","high")))


df <- tibble(ipd = rep(df_sens$ipd, 3),
             ddd = rep(df_sens$ddd, 3),
             phase = c(rep("Phase 1",9),rep("Phase 2",9),rep("Phase 3",9)),
             specificity = c(df_sens$phase1,
                             df_sens$phase2,
                             df_sens$phase3))

df <- df %>% mutate(ipd = factor(df$ipd,labels=c("Short IPD","Mid IPD","Long IPD")))
df <- df %>% mutate(ddd = factor(df$ddd,labels=c("Short DDD","Mid DDD","Long DDD")))


g10 <- df %>% ggplot() + geom_col(aes(x=phase,y=1-specificity,col=phase,fill=phase)) +
  scale_color_manual(values=cbPalette[c(7,2,4)]) +  
  scale_fill_manual(values=cbPalette[c(7,2,4)]) +  
  labs(x = "", y = "Probability of falsely passing phase (10 year period)", color = "Phase:",fill="Phase:") + 
  theme_minimal(base_size = 16) +
  theme(legend.position = "none") +
  guides(color = guide_legend(ncol=1)) +
  facet_grid(ipd ~ ddd)


######################################################
# Figure S3: 20 year specificity 
# (sensitivity analysis of ipd and ddd)
######################################################

df_sens20 <- readRDS("Results/FigureS3.RDS")
df_sens20 <- df_sens20 %>% mutate(ipd = factor(ipd, levels = c("low","mid","high")))
df_sens20 <- df_sens20 %>% mutate(ddd = factor(ddd, levels = c("low","mid","high")))


df <- tibble(ipd = rep(df_sens20$ipd, 3),
             ddd = rep(df_sens20$ddd, 3),
             phase = c(rep("Phase 1",9),rep("Phase 2",9),rep("Phase 3",9)),
             specificity = c(df_sens20$phase1,
                             df_sens20$phase2,
                             df_sens20$phase3))

df <- df %>% mutate(ipd = factor(df$ipd,labels=c("Short IPD","Mid IPD","Long IPD")))
df <- df %>% mutate(ddd = factor(df$ddd,labels=c("Short DDD","Mid DDD","Long DDD")))


g20 <- df %>% ggplot() + geom_col(aes(x=phase,y=1-specificity,col=phase,fill=phase)) +
  scale_color_manual(values=cbPalette[c(7,2,4)]) +  
  scale_fill_manual(values=cbPalette[c(7,2,4)]) +  
  labs(x = "", y = "Probability of falsely passing phase (20 year period)", color = "Phase:",fill="Phase:") + 
  theme_minimal(base_size = 16) +
  theme(legend.position = "none") +
  guides(color = guide_legend(ncol=1)) +
  facet_grid(ipd ~ ddd)




