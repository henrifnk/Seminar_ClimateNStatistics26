source("functions.R")
source("config.R")

# Parameters --------------------------------------------------------------
# In case this code is run for the first time, the data files need to be created --> Set to TRUE
create_datafiles = FALSE
# Save the plots displayed during this session
save_plots = TRUE
# Analysis focuses on some parts on one specific station. Define this station here:
station = "München"
# Reference year: The one year I consider in more detail
ref_year = 2024
# The bavaria plot takes quite some time to load.. Thus, control if it supposed to be loaded at all
create_bavaria_plot = TRUE

# Create Data Files (if necessary) ----------------------------------------
if (create_datafiles) create_dfs(
  # Path where the .csv files are
  data_path = rawdata_input_path, 
  # Path where interim-step files are saved
  extended_dfs_path = extended_dfs_path,
  # Path where interim-step files are saved
  threshold_dfs_path = threshold_dfs_path,
  # Path where the hydrographs are saved (If created)
  hydrograph_path = hydrograph_path,
  # Path where the data for the copula model is saved
  copula_dfs_path = copula_dfs_path,
  # Pattern of the Rdata-files that are read in. Can be adjusted to focus only on a specific station
  pattern = "*.Rdata",
  # Do you want to save all possible hydrographs?
  hydros = FALSE
)

# Load Data Files ---------------------------------------------------------
# Load data.frame containing the data for the copula model
  # During the loading process, remove stations with only few observations
  # Default threshold is 30 observations
cop_df = get_copula_df() |>  
  dplyr::filter(river %in% considered) |>   
  filter_cop_df(min_num_obs)
cor_table = get_cor_table(cop_df)
df = get_long_df(threshold_dfs_path)
# Extract station specific data frames
# id = (cop_df |> dplyr::filter(unit == station))[1, "id"]
# ref_yeardf = df |> dplyr::filter(year == ref_year)
# peak_info = cop_df |> dplyr::filter(year == ref_year, unit == station)
scop_df = cop_df |> dplyr::filter(unit == station)
ref_flood = scop_df |> dplyr::filter(year == ref_year)

# Fit models --------------------------------------------------------------
# Copula Models
nacs = fit_nacs(cop_df, cop_df$unit |> unique())
vines = fit_vines(cop_df, cop_df$unit |> unique())
# Station specific copula 
vine = vines[[station]]
# Marginal Distributions
gev_peak = marginal_fit(scop_df$peak, type = "GEV")
gev_vol = marginal_fit(scop_df$volume, type = "GEV")
gev_dur = marginal_fit(scop_df$duration_days, type = "GEV")

taildep_df = lapply(
  names(vines),
  function(name) grab_taildeps(name, vines, cop_df)
) |> dplyr::bind_rows()  



# Data Section ------------------------------------------------------------
## Bavaria Plot ----------------------------------------------------------
if (create_bavaria_plot) bavaria_params = get_bavaria_plot(cop_df, save_plot = save_plots, save_plotname = "data_bavaria_plot")

## Hydro Plot ------------------------------------------------------------
get_hydrograph(df |> dplyr::filter(unit == station, year == ref_year), save_plot = save_plots, plotname = "data_hydrograph")

## Correlation Plot ------------------------------------------------------
get_cor_plot(cor_table, save_plot = save_plots, plotname = "data_cor_plot")
cor_table |> dplyr::summarise(n = dplyr::n(), .by = c(river, tau_order))

# Cond boxplots -----------------------------------------------------------
get_cond_boxplots(save_plot = save_plots)

## Further Descriptives --------------------------------------------------
# Further descriptives for each river
# Number of observations
table((cop_df |> dplyr::summarise(n = dplyr::n(), .by = id))$n)
cop_df |> dplyr::filter(year == min(cop_df$year))
cop_df |> dplyr::summarise(.by = c(unit, river)) |> dplyr::summarise(n = dplyr::n(), .by = river)
print(paste("Total number of flood events:", cop_df |> nrow()))
# Some characteristics of variable distribution
summary_df = cop_df |> 
  dplyr::summarise(
    max_peak = max(peak),
    max_vol = max(volume),
    max_dur = max(duration_days),
    .by = river
  ) 
summary_df
# Danube events
get_events(cop_df, summary_df, rivername = "Donau")
# Isar events
get_events(cop_df, summary_df, rivername = "Isar")


# Simulation Section ------------------------------------------------------
# IMPORTANT: Assumes that the simulation is already run and simulation data is available
# Display correlation average to confirm used correlation values in simulation
cor_table |> dplyr::summarise(mean_vd = mean(tau_vd), mean_vp = mean(tau_vp), mean_dp = mean(tau_dp), .by = river) 
get_nac_bias_in_vine_dgp(save_plot = save_plots, plotname = "sim_NAC_if_Vine_DPG")

# Application Section -----------------------------------------------------
# Visual GOF
visualGOF(vine = vine, scop_df = scop_df, station = station, n_syn = n_syn, save_plot = save_plots, plotname = "app_visualGOF")
# Tail Dependencies
if (create_bavaria_plot) plot_bavaria_taildep(taildep_df, bavaria_params, save_plot = save_plots, plotname = "app_bavaria_taildep")
# Comparison univariate vs. copula approach
# München flood 2024
cop_df |> dplyr::filter(unit == "München", year == 2024)
get_univariate_HQ_plot(scop_df, ref_flood, gev_peak, save_plot = save_plots, plotname = "app_univariate_hq")
get_trivariate_HQ_plot(scop_df, ref_flood, vine, n_syn = 1e6, save_plot = save_plots, plotname = "app_multivariate_hq") 
# Most likely Volume - Duration pairs for HQ values 
model_evaluation(
  cop_df = cop_df |> dplyr::left_join(cor_table |> dplyr::select(id, tau_order) |> unique(), by = "id"), 
  nacs = nacs, 
  vines = vines, 
  hq_probs = hq_probs,
  save_plot = save_plots,
  plotname = "app_modeleval"
)




