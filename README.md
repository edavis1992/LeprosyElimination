# LeprosyElimination
Code for Davis et al. "A modelling analysis of potential strategies for declaring local elimination of leprosy" (2023)

Version of R used: 4.2.2 (2022-10-31)

Required packages:
- tidyverse
- zoo
- tictoc
- arm
- gridExtra
- cowplot

To run the code and generate the results for a specific scenario from the publication, please follow the following steps.

* Task: Using RunSims.R to run simulations for a particular scenario
* R files used: SetUp.R, RunSims.R
* Output files: exp_decline.RDS or persistenceXX.RDS (where persist_level = 0.XX)
1. Ensure your working directory is set to the root folder containing the R files
2. Decide which scenario to run:
    Scenario 1. True interruption: persist_level = 0
    Scenario 2. Low-level persistence: persist_level = 0.1
    Other choice of persistence level: range 0 to 0.2
      - Representing 0 to 4 new infections per year.
      - Used to generate results for Figure 3
3. Set persist_level (line 55 of RunSims.R)
4. Set output filename (line 61 of RunSims.R)
5. Uncomment lines 148-150 to save outputs to chosen filename
6. Source RunSims.R

Table 1
* Task: Processing simulation outputs 
* Scenario: Exponential decline
* R files used: SetUp.R, GetPhases_ExpDecline.R, CalculateSensitivity.R, GetSituation.R
* Results files used: exp_decline.RDS
* Output files: situation_exp_decline.RDS, sensitivities.RDS
1. Uncomment lines 69-71 in GetPhases_ExpDecline.R to save outputs
    Outputs saved in Results/situation_exp_decline.RDS
2. Run GetPhases_ExpDecline.R
3. To calculate sensitivities run CalculateSensitivity.R
    Outputs saved in Results/sensitivities.RDS
    (Outputs are results presented in Table 1)
    
Table 2
* Task: Processing simulation outputs
* Scenario: Low-level persistence (2 cases/year)
* R files used: SetUp.R, GetPhases.R, CalculateSpecificity.R, GetSituation.R
* Results files used: persistence10.RDS
* Output files: situation10.RDS, specificities.RDS
1. In GetPhases.R set filename (line 10) to Results/persistence10.RDS
2. Uncomment line 38 in GetPhases.R to save outputs
    Outputs saved in Results/situation10.RDS
3. Run GetPhases.R
4. To calculate specificities run CalculateSpecificity.R
    Outputs saved in Results/specificities.RDS
    (Outputs are results presented in Table 2)

Figure 4
* Task: Processing and plotting simulation outputs
* Scenario: Varying low-level persistence
* R files used: SetUp.R, GetPhases.R, CalculateSpecificity_PersistLevels.R, GetSituation.R
* Results files used: persistenceXX.RDS (XX from 01 to 20)
* Output files: situationXX.RDS (XX from 01 to 20), Figure3.RDS
1. In GetPhases.R set filename (line 10) to Results/persistenceXX.RDS
2. Uncomment line 38 in GetPhases.R to save outputs
    Outputs saved in Results/situationXX.RDS
3. Run GetPhases.R
4. Repeat 1-3 for all values of XX in 01 to 20
5. To calculate specificities run CalculateSpecificity_PersistLevels.R
    Outputs saved in Results/Figure4.RDS
6. To plot Figure 4, use lines 2-4 and 32-48 of Plots.R

Figure 1
* Task: Plotting incubation period and detection delay distributions
* R files used: Plot.R
* Input files used: no_mi_tpd.Rd
1. Run lines 1-26 of Plot.R

Figures S2 and S3
* Task: Processing and plotting simulation outputs
* Scenario: Low-level persistence (2 cases/year), varying IPD/DDD
* R files used: SetUp.R, GetPhases.R, CalculateSpecificity_IPD_DDD.R, GetSituation.R
* Results files used: mid_persist_Xipd_Yddd.RDS (X, Y = low, mid, high)
* Output files: situation_Xipd_Yddd.RDS (XX from 01 to 20), FigureS2.RDS, FigureS3.RDS
1. In GetPhases.R set filename (line 10) to Results/mid_persist_Xipd_Yddd.RDS
2. Uncomment line 38 in GetPhases.R to save outputs
    Outputs saved in Results/situation_Xipd_Yddd.RDS
3. Run GetPhases.R
4. Repeat 1-3 for all combinations of X and Y
5. To calculate specificities run CalculateSpecificity_IPD_DDD.R
    Outputs saved in Results/FigureS2.RDS and Results/FigureS3.RDS
6. To plot Figure S2 and S3, use lines 2-4 and 57-111 of Plots.R


File naming conventions:
- persistenceXX.RDS represents persist_level = 0.XX
- exp_decline refers to Scenario 1: Interruption of transmission

