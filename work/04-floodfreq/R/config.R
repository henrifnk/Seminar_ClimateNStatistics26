# Parameters: Data Generation ----------------------------------------------------
rawdata_input_path = "../data/0 input data/"
extended_dfs_path = "../data/output/rdata/extended_dfs/"
threshold_dfs_path = "../data/output/rdata/threshold_dfs/"
hydrograph_path = "../data/output/hydrographs/"
copula_dfs_path = "../data/output/rdata/copula_dfs/"


# Parameters: Context Data ------------------------------------------------
# Parameters, that are not directly connected to our analysis itself, but e.g. found in the literature
paper_tau = data.frame(
  value = rep(c(0.295, 0.462, 0.776), 2),
  tau = rep(c("Duration - Peak", "Volume - Duration", "Volume - Peak"), 2),
  river = c(rep("Isar", 3), rep("Donau", 3))
)

# Parameters: Loading Data Files ------------------------------------------
# The rivers our analysis is based on 
considered = c("Isar", "Donau")
# Minimum number of years / observations required to fit a copula
min_num_obs = 30

# Parameters: Plotting ----------------------------------------------------

# Parameters: Model Fitting -----------------------------------------------
assumed_vine_structure =  matrix(
  # 1 - 2 - 3 where: 1:dur, 2:peak, 3:vol
    c(
      1, 0, 0,
      3, 2, 0,
      2, 3, 3
    ),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )


# Parameters: Analysis ----------------------------------------------------
# Considered HQs
HQs = c(2, 5, 10, 20, 50) # Return periods
HQ_probs = 1/HQs # Corresponding probability for return period
# Number of synthetic data
n_syn = 1000



# gg themes ---------------------------------------------------------------
title_size = 24
ggtheme = theme(
  plot.title = element_text(size = title_size),
  axis.title = element_text(size = 18),
  axis.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 18),
  strip.text = element_text(size = 14)  
)
