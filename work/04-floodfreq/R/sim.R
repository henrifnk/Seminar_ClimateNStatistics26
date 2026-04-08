# Simulation Structure ----------------------------------------------------
# >> Simulation: 
# Repeat B times: 
#   For each sample size (n), each copula family, each dependence structure (AC, NAC, Vine), tau:
#     Draw random sample  
#   For each sample, fit AC, NAC and Vine copula and evaluate
# (Note: I only consider positive correlation bc 1. data only contains positive and 2. negative correlation can be modelled wlog by 1-u)

# -- Considered Copula Families
#   Gumbel, Clayton, Joe

# -- Considered Copula Structures
#   AC (3: symmetric Gumbel, Clayton, Joe) 
#   NAC (3: M4, M5, M6) 
#   Vines (3+: Same bivariate dependence: Gumbel, Clayton, Frank 

# >> Evaluation
# 1) Retrieval of correct structure
#   Fit all 3 copula types (using their implemented selection method)
#   Choose among these 3 dependence structures using AIC
#   How of (%) is the correct copula selected 

# 2) MSE and Biases
#   Issue: MSE only defined between the true and estimated structure
#
#   Using the true parameters and the copula fitted in step 1, calc MSE for: 
#   tau, copula param, tail dependence
#     (Added tail dep. bc it also should be a non-linear function of tau / theta and thus be biased?)
#   Interesting: 
#   - How does sample size affect MSE (Bias and Variance)?
#   - How reliable are small sample estimates (variance for small samples)?
#   - Is there a bias in estimates due to Jensen's inequality

# 2.1) MSE in those cases, where the correct structure is selected
# 2.2) MSE for all cases

# 3) Performance (under misspecification)
#   - Compare each of the 3 fitted copulas using the KL (allows to compare whole distribution)

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(doParallel)
source("functions.R")
# Parameters --------------------------------------------------------------
# Sample size of simulated sample
sample_sizes = c(15, 30, 50, 1000)

# Observed dependencies
taus = list(
  # Requirement: Order taus in INCREASING order when adding new one (Ensures NAC sim works)
  # How simulations use taus:
  # NACs: tau_outer = mean(tau1, tau2) and tau_inner = tau3
  # Vine: c("1-3", "2-3", "1-2") with: 1,3|2 - 2,3 - 1,2, i.e. 1 - 2 - 3
  # "Donau" = c(.144, .423, .615),
  # "Isar" = c(.147, .466, .568),
  "Isar" = c(.147, .568, .466)
  # Extreme cases:
  # Two similar dependence structure, but both HIGH, so only one of them can be nested. 
  #   But outer cannot model two different degrees of dependence between the nested variables
  # "LowHighHigh" = c(0.1, 0.8, 0.8),
  # "LowMedHigh" = c(0.1, 0.5, 0.85)
)

# Cores used during parallelization
n_cores = parallel::detectCores() - 2

# a) Copula ---------------------------------------------------------------
# NACs ----
# This code works perfectly fine. However, our analysis focuses on vine DGPs. 
# Thus, we leave this code commented out as the interested reader may run it.
# 
# # Number of simulations per setting
# B = 4000
# 
# # Copula families considered during the simulation
# copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5)
# 
# # Start simulation
# cluster <- parallel::makeCluster(n_cores, outfile = "nac.log")
# doParallel::registerDoParallel(cluster)
# 
# # Ensure cluster stops after execution
# on.exit(parallel::stopCluster(cluster))
# 
# for (n in sample_sizes){
#   for (cop in names(copula_families)){
#       # Run parallelized
#       res <- foreach(
#         seed = 1:B,
#         .combine = "rbind"
#       ) %dopar% {
#         # Simulate
#         tryCatch(
#           run_one_nac(
#             seed = seed,
#             n = n,
#             cop = cop,
#             taus = taus
#           ),
#           error = function(e) {
#             message(e)
#             message(paste("NAC  -- Error at seed:", seed, " - Cop:", cop, " - n:", n, " - msg:", e$message))
#             NULL
#           })
#       }
# 
#       res = res |> dplyr::bind_rows(.id = "seed")
#       res$seed = as.numeric(res$seed)
#       attr(res, "n") = n
#       attr(res, "cop") = cop
#       attr(res, "dep") = "nac"
# 
#       attr(res, "B") = B
#       attr(res, "sample sizes") = sample_sizes
#       attr(res, "copula families") = copula_families
# 
#       # Save simulation results in file
#       filename = paste("../data/simulation/simulation_n", n, "_cop", cop, "_depnac.Rdata", sep = "")
#       save(res, file = filename)
#   }
# }
# 
# stopCluster(cluster)

# Vines ----
# For drawing vines, we have 3^3 possible vine structures. Thus, draw 27000 so that every vine structure has 1k (on average bc I draw them randomly)
B = 27 * 1000
# Also, the copula families have a different encoding
copula_families = list("Clayton" = 3, "Gumbel" = 4, "Frank" = 5)

cluster <- parallel::makeCluster(n_cores, outfile = "vine.log")
doParallel::registerDoParallel(cluster)

# Ensure cluster stops after execution
on.exit(parallel::stopCluster(cluster))

for (n in sample_sizes){
    # Run parallelized
    res <- foreach(
      seed = 1:B
    ) %dopar% {
      tryCatch(
      run_one_vine(
        seed = seed,
        n = n,
        taus = taus
      ),
        error = function(e){
          message(paste("Vine -- Error at seed:", seed, " - ", e$message))
          NULL
        })
    }

    res = res |> dplyr::bind_rows(.id = "seed")
    attr(res, "n") = n
    attr(res, "dep") = "vine"

    attr(res, "B") = B
    attr(res, "sample sizes") = sample_sizes
    attr(res, "copula families") = copula_families

    # Save simulation results in file
    filename = paste("../data/simulation/simulation_n", n, "_depvine.Rdata", sep = "")
    save(res, file = filename)
}

stopCluster(cluster)
