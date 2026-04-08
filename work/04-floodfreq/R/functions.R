# Load Data ---------------------------------------------------------------
# Sections contains all functions needed to go from the GKD input data to the data used for our analysis
source("functions_load_data.R")
source("functions_sim.R")

# Load Data into Session --------------------------------------------------
get_copula_df = function(
    in_dir = "../data/output/rdata/copula_dfs/"
  ){
  "
  Read all copula dfs and join them to one large copula df
  "
  filenames = paste(in_dir, list.files(in_dir), sep = "")
  
  cop_df = purrr::map_dfr(filenames, load_rdata)|> 
    dplyr::mutate(
      pobs_peak = copula::pobs(peak),
      pobs_dur = copula::pobs(duration_days),
      pobs_vol= copula::pobs(volume),
      .by = unit
    ) 
  
  return(cop_df)
}

filter_cop_df = function(cop_df, n_minobs){
  "
  This functions removes the stations that have too little information to fit a copula on.
  "
  obs_status = cop_df |> 
  dplyr::summarise(
    n = dplyr::n(),
    .by = c(river, unit)
  ) |> 
  dplyr::mutate(
    nlarge = n > min_num_obs 
  ) 
  considered_stations = obs_status |> dplyr::filter(nlarge == TRUE)
  removed_stations = obs_status |> dplyr::filter(nlarge == FALSE)
  # cop_df only contains stations with more than threshold number of observations
  message(
    paste(length(considered_stations$unit), "stations considered")
  )
  message(
    paste(length(removed_stations$unit), "stations removed")
  )
  message("Removed station table:")
  table_details = data.frame(table(removed_stations$n))
  colnames(table_details) = c("n", "count")
  message(
    paste0(
      capture.output(table_details),
      collapse = "\n"
    )
  )
      
  return(cop_df |> dplyr::filter(unit %in% considered_stations$unit))
}

get_long_df = function(
    in_dir = "../data/output/rdata/threshold_dfs/"
  ){
  "
  Read any non-copula data frames and join to one large one
  "
  filenames = paste(in_dir, list.files(in_dir, pattern = "*.Rdata"), sep = "")

  return(purrr::map_dfr(filenames, load_rdata))
}

load_rdata = function(filepath){
  # IMPORTANT! Assumes only 1 object / df within the rdata file
  # Note: Use new environment for each load to prevent overwriting within lapply
  env = new.env()
  load(filepath, envir = env)
  get(ls(env)[1], envir = env)
}


# Re-shape Data Frames ----------------------------------------------------
get_cor_table = function(cop_df){
  return(
    cop_df |> 
      dplyr::summarise(
        n = dplyr::n(),
        tau_vd = cor(volume, duration_days, method = "kendall"),
        tau_vp = cor(volume, peak, method = "kendall"),
        tau_dp = cor(duration_days, peak, method = "kendall"),
        .by = c(river, unit, id)
      ) |> 
      add_tau_order_col() 
  )
}

add_tau_order_col = function(df){
  df$tau_order = unlist(
    lapply(
      1:nrow(df), 
      function(i) with(df, get_tau_order(tau_vec = c(tau_vd = tau_vd[i], tau_vp = tau_vp[i], tau_dp = tau_dp[i])))
    )
  )
  return(df)
}
get_tau_order = function(tau_vec){
  return(paste0(names(sort(tau_vec)), collapse = "<"))
}

# Model Fitting -----------------------------------------------------------
fit_nacs = function(cop_df, all_units){
  nac_fams = list("1" = "Gumbel", "3" = "Clayton", "5" = "Frank")
  
  nacs = lapply(
    all_units,
    function(name){
      fit_nac(mat = cop_df |> dplyr::filter(unit == name) |> dplyr::select(contains("pobs")) |> as.matrix(), families = c(1, 3, 5))
    }
  )
  names(nacs) = all_units
  
  return(nacs)
}

fit_nac = function(mat, families){
  # Estimate copula using implemented selection method
  # HAC package does not offer way to select the copula family
  # Solution: Fit all considered families and use AIC to select accordingly
  
  # Allowed copula families
  types = unlist(unname(families))
  
  # Select one with smallest AIC. NOTE: p const --> use logLik
  nac_fits = lapply(types, function(type) HAC::estimate.copula(mat, type = type))
  # If any of the NACs suggests a non-nested structure, remove it. 
  # Two reasons: 
  # 1) I already fit a non-nested structure
  # 2) The HAC logLik function breaks down for these functions. And since I already fit one anyway, I use the simplest way to deal with this issue
  # Non-nested NACs have a tree structure of length 4. Use this to filter them out:
  nac_fits = nac_fits[!lapply(nac_fits, function(nac) length(nac$tree)) == 4]
  
  nac_lls = lapply(nac_fits, function(nac) get_nac_loglik(mat, nac))
  nac_best_fit = which.max(nac_lls)
  
  # Best fit
  nac = nac_fits[[nac_best_fit]]
  
  attr(nac, "logLik") = nac_lls[nac_best_fit]
  attr(nac, "theta") = get_nac_estimates(nac)
  attr(nac, "tau") = nac_theta2tau(nac$type, attr(nac, "theta"))
  
  return(nac)
}

get_nac_loglik = function(mat, nac_mdl){
  return(HAC::to.logLik(X = mat, hac = nac_mdl, eval = TRUE))
}

get_nac_estimates = function(nac_mdl){
  return(c(nac_mdl$tree[[3]], nac_mdl$tree[[1]][[3]]))
}

nac_theta2tau = function(family_index, theta){
  theta = unname(theta)
  return(
    c(
      HAC::theta2tau(theta = theta[1], type = family_index),
      HAC::theta2tau(theta = theta[2], type = family_index)
    )
  )
}

fit_vines = function(
    cop_df, 
    all_units,
    # Vine structure we assumed (Conditional copula is dur|peak - vol|peak)
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
  ){
  vines = lapply(
    all_units,
    function(name) {
      mat = cop_df |> 
        dplyr::filter(unit == name) |> 
        dplyr::select(contains("pobs")) |> 
        dplyr::select("pobs_dur", "pobs_peak", "pobs_vol") |> # Ensure the order is as expected
        as.matrix() 
      
      vine = VineCopula::RVineCopSelect(data = mat, Matrix = assumed_vine_structure, familyset = c(3, 4, 5))
    }
  )
  names(vines) = all_units
  
  return(vines)
}

fit_vine = function(mat, families){
  # The VineCopula package is faster and applicable as long as we do not also estimate covariates
  # Families: 3, 4, 5 (see docu)
  vine = VineCopula::RVineStructureSelect(data = mat, rotations = FALSE, familyset = families)
  return(vine)
}

marginal_fit = function(vec, type){
  return(extRemes::fevd(vec, type = type))
}

dmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  shape = FALSE
  if (type != "Gumbel") shape = mle[["shape"]]
  
  dens = extRemes::devd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = shape,
      type = type 
    )
  if (length(vec) == 1) return(dens[[1]])
  return(dens)
}

pmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  shape = FALSE
  if (type != "Gumbel") shape = mle[["shape"]]
  
  return(
    extRemes::pevd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = shape,
      type = type 
    )
  )
}
qmarginal = function(vec, obj, type = "GEV"){
  mle = obj$results$par
  
  return(
    extRemes::qevd(
      vec, 
      loc = mle[["location"]],
      scale = mle[["scale"]], 
      shape = mle[["shape"]], 
      type = type 
    )
  )
  
}

# Plots -------------------------------------------------------------------
# Generic function used to save plots
savegg = function(
    filename, 
    ending = ".png", out_path = "../figures/", width = 15, height = 5, dpi = 300)
  {
  ggsave(
    paste(out_path, filename, ending, sep = ""),
    width = width,
    height = height,
    dpi = dpi
  )
}

get_bavaria_plot = function(
    cop_df,
    save_plot = FALSE,
    save_plotname = "bayern_rivers"
    ){
  # This plot requires a river-file from osmdata. The query itself takes very long. 
  # Thus, run the following function to check, if the river-file has already been saved in the expected path
  # In case it has not been saved yet, the query is run taking quite some time. But the output will then be saved for future runs
  rivers = get_river_file()
  
  germany = rnaturalearth::ne_states(country = "Germany", returnclass = "sf")
  bayern = germany |> dplyr::filter(name == "Bayern")
  # Create bounding box for openstreetmap
  bayern_bbx = sf::st_bbox(bayern) 
  
  pos_sf = cop_df |> 
    dplyr::select(unit, east, north, river) |> 
    unique() |> 
    gkd2gg(coord_cols = c("east", "north")) |> 
    dplyr::filter(river %in% considered) |> 
    dplyr::mutate(
      river_station = dplyr::case_when(
        river == "Isar" ~ "Isar_Station",
        river == "Donau" ~ "Donau_Station"
      )
    )
   
  # Create sf object based on the queried osm object
  rivers_sf = rivers$osm_lines
  
  # Cut rivers so they fit into Bayern
  rivers_st = sf::st_transform(rivers_sf, sf::st_crs(bayern))
  rivers_bayern = sf::st_intersection(rivers_st, bayern)
  # save(rivers_bayern, file = "../data/output/presentation/rivers_bayern.Rdata")
  
  # Add identificator for selection of relevant rivers
  rivers_bayern = rivers_bayern |> 
    dplyr::mutate(
      river = dplyr::case_when(
        name == "Isar" ~ "Isar",
        name == "Donau" ~ "Donau",
        TRUE ~ "Other"
      )
    )
  
  plot(
    ggplot(rivers_bayern) + 
    # Displaying the actual data
    # 1) Make background look like map
    ggspatial::annotation_map_tile(type = "osm", zoom = 8) + 
    # 2) Add the rivers twice. 
    #   1st in black to give a outline for each river
    #   (combination of fill and color does not work for geom_sf)
    #   2nd the actual rivers in desired colors
    geom_sf(color = "black", linewidth = 2) + # Outlines for the rivers
    geom_sf(aes(color = river, fill = river), linewidth = 1.5) +
    # 3) Add the shape of Bayern on top to highlight the relevant area
    geom_sf(data = bayern, fill = "white", alpha = 0.4, linewidth = .8) + 
    # 4) Add station
    geom_sf(data = pos_sf, aes(color = river_station, shape = river_station, fill = river_station), size = 4) +
    # Formatiing of Data
    theme_void() + 
    theme(
      # Remove the x and y ticks / labels
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      # Adjust the legend position and font size
      legend.position = c(1, 1),  # Move legend to top-right
      legend.justification= c(1, 1),  # Align legend inside the top-right
      legend.background = element_rect(fill = "white", color = "black", linewidth = .5),
      legend.box = "vertical",
      legend.margin = margin(1, 1, 1, 1),  # (top, right, bottom, left) padding
      legend.text = element_text(size = 20),  # Adjust size of legend labels
      legend.title = element_text(size = 24)
    ) + 
    # The color aes is actually what colors the rivers and stations, but the legend is super ugly
    # So I use the fill legend and do not display the color legend
    scale_color_manual(
      name = "River",
      values = c(
        "Donau" = "#7CAE00", "Isar" = "#F8766D", "Other" = "#00BfC4", # River colors
        "Donau_Station" = "darkgreen", "Isar_Station" = "darkred" # Station colors
      ), 
      guide = "none" # Do not show color legend
    ) +
    scale_fill_manual(
      name = "River",
      values = c("Donau" = "#7CAE00", "Isar" = "#F8766D", "Other" = "#00BfC4"),
      labels = c("Donau" = "Donau", "Isar" = "Isar", "Other" = "Other")
    ) +
    scale_shape_manual(
      name = "Station",
      values = c("Donau_Station" = 16, "Isar_Station" = 17),
      labels = c("Donau_Station" = "Donau", "Isar_Station" = "Isar")
    ) +
    guides(
      fill = guide_legend(order = 1),
      shape = guide_legend(override.aes = list(
        color = c("darkgreen", "darkred")
        ),
        order = 2
      )
    ) 
  )
  
  if(save_plot) savegg(save_plotname)
  
  return(
    list(
      pos_sf = pos_sf,
      rivers_bayern = rivers_bayern,
      bayern = bayern
    )
  )
}

get_hydrograph = function(df, save_plot = FALSE, plotname = "selected_hydrograph"){
  plot(
    create_hydrograph(df)
    )
  if(save_plot) savegg(plotname)
}


get_cor_plot= function(cor_table, save_plot = FALSE, plotname = "cor_plot"){
  plot(
    cor_table |> 
    tidyr::pivot_longer(
      cols = c(tau_vd, tau_vp, tau_dp),
      names_to = "tau",
      values_to = "val"
    ) |> 
    dplyr::mutate(
      tau = dplyr::case_when(
        tau == "tau_dp" ~ "Duration - Peak",
        tau == "tau_vd" ~ "Volume - Duration",
        tau == "tau_vp" ~ "Volume - Peak"
      ),
      tau = as.factor(tau)
    ) |> 
    ggplot() + 
    geom_boxplot(aes(y = val, x = tau, color = tau)) + 
    geom_point(data = paper_tau, aes(y = value, x = tau), color = "black", alpha = 0.7) + # Add paper taus
    geom_hline(yintercept = 0, linetype = 2) + 
    theme(legend.position = "none") + 
    labs(
      # title = "Correlation across Stations",
      y = latex2exp::TeX("$ \\widehat{\\tau}$"),
      x = ""
    ) + 
    ggtheme + 
    ylim(-0.12, 1) + 
    facet_wrap(~river) 
  )
  if(save_plot) savegg(plotname)
}

get_events = function(cop_df, summary_df, rivername){
  current_summary = summary_df |> dplyr::filter(river == rivername)
  list(
    max_peak = cop_df |> dplyr::filter(peak == current_summary$max_peak),
    max_vol = cop_df |> dplyr::filter(volume == current_summary$max_vol),
    max_dur = cop_df |> dplyr::filter(duration_days == current_summary$max_dur)
  )
}

visualGOF = function(
    vine, 
    scop_df, 
    station = "München", 
    save_plot = FALSE,
    plotname = "visualGOF",
    n_syn = 1000,
    title = "TITLE", 
    contour_binwidth = 0.1,
    contour_alpha = 1
  ){
  dens_df = get_density_values(vine, get_grid(), station)
  syn_df = get_synthetic_data(vine, n_syn, station)
  fitted_tau = list("Duration - Peak" = vine$tau[3, 1], "Duration - Volume" = vine$tau[3, 2], "Peak - Volume" = vine$tau[2, 1])
  emp_tau = list("Duration - Peak" = vine$emptau[3, 1], "Duration - Volume" = vine$emptau[3, 2], "Peak - Volume" = vine$emptau[2, 1])

  # Print out the fitted copula families
  message("Fitted copula details:")
  for (var in dens_df$vars |> unique()){
    fam = dens_df |> dplyr::summarise(.by = c(vars, fam)) |> dplyr::filter(vars == var) |> dplyr::select(fam)
    fam_name = get_copname(fam$fam)
    message(paste("-- ", var, ": ", fam_name, sep = ""))
    message(paste("-- -- Empirical tau: ", emp_tau[[var]]))
    message("-- -- Fitted tau: ", fitted_tau[[var]])
    message("-- -- -- Abs tau diff: ", abs(fitted_tau[[var]] - emp_tau[[var]]))
  } 
  
  # Plot
  cont_dur_peak = get_contour(
    rel = "Duration - Peak", 
    splot_df = dens_df, 
    sdf = scop_df, 
    vary = "pobs_dur", 
    varx = "pobs_peak", 
    title = "Duration - Peak", 
    y_lab = "P.Obs. Duration",
    x_lab = "",
    bwidth = 0.1,
    contour_alpha = 0.7
  ) + ggtheme
  cont_peak_vol = get_contour(
    rel = "Peak - Volume", 
    splot_df = dens_df, 
    sdf = scop_df, 
    vary = "pobs_peak", 
    varx = "pobs_vol", 
    title = "Peak - Volume", 
    y_lab = "P.Obs. Peak",
    x_lab = "",
    bwidth = 0.1,
    contour_alpha = 0.7
  ) + ggtheme
  cont_dur_vol = get_contour(
    rel = "Duration - Volume", 
    splot_df = dens_df, 
    sdf = scop_df, 
    vary = "pobs_dur", 
    varx = "pobs_vol", 
    title = "Duration - Volume | Peak", 
    y_lab = "P.Obs. Duration",
    x_lab = "",
    bwidth = 0.2,
    contour_alpha = 0.7
  ) + ggtheme
  
  syn_dur_peak = get_syn_scatter(
    ssyn_df = syn_df, 
    vary_syn = "pobs_dur", 
    varx_syn = "pobs_peak", 
    sdf = scop_df, 
    vary_df = "pobs_dur", 
    varx_df = "pobs_peak", 
    y_lab = "P.Obs. Duration",
    x_lab = "P.Obs. Peak",
    syn_alpha = 0.8,
    x_min = 0,
    x_max = 1,
    y_min = 0,
    y_max = 1
  )+ ggtheme
  syn_peak_vol = get_syn_scatter(
    ssyn_df = syn_df, 
    vary_syn = "pobs_peak", 
    varx_syn = "pobs_vol", 
    sdf = scop_df, 
    vary_df = "pobs_peak", 
    varx_df = "pobs_vol", 
    y_lab = "P.Obs. Peak",
    x_lab = "P.Obs. Volume",
    syn_alpha = 0.8,
    x_min = 0,
    x_max = 1,
    y_min = 0,
    y_max = 1
  )+ ggtheme
  syn_dur_vol = get_syn_scatter(
    ssyn_df = syn_df, 
    vary_syn = "pobs_dur", 
    varx_syn = "pobs_vol", 
    sdf = scop_df, 
    vary_df = "pobs_dur", 
    varx_df = "pobs_vol", 
    y_lab = "P.Obs. Duration",
    x_lab = "P.Obs. Volume",
    syn_alpha = 0.8,
    x_min = 0,
    x_max = 1,
    y_min = 0,
    y_max = 1
  ) + ggtheme
  
  plt = (cont_dur_peak | cont_peak_vol| cont_dur_vol) / 
      (syn_dur_peak| syn_peak_vol| syn_dur_vol) 
  plot(plt)
  if (save_plot) savegg(plotname)
}

grab_taildeps = function(station, vines, cop_df){
  vine = vines[[station]]
  df = cop_df |> dplyr::filter(unit == station)
  river = df$river[1]
  north = df$north[1]
  east = df$east[1]
  
  data.frame(
    variables = c(
      "D - P",
      "P - V",
      "D - V"
    ),
    cop_fam = c(
      get_copname(vine$family[3, 1]),
      get_copname(vine$family[3, 2]),
      get_copname(vine$family[2, 1])
    ),
    upper = c(
      vine$taildep$upper[3, 1],
      vine$taildep$upper[3, 2],
      vine$taildep$upper[2, 1]
    ),
    lower = c(
      vine$taildep$lower[3, 1],
      vine$taildep$lower[3, 2],
      vine$taildep$lower[2, 1]
    )
  ) |> 
    dplyr::mutate(
      river = river,
      unit = station,
      north = north,
      east = east
    )
}

plot_bavaria_taildep = function(taildep_df, bavaria_params, considered = c("Isar", "Donau"), save_plot = FALSE, plotname = "Taildep_Bayern"){
  rivers_bayern = bavaria_params$rivers_bayern
  pos_sf = bavaria_params$pos_sf
  bayern = bavaria_params$bayern
  
  taildep_pos_sf = gkd2gg(taildep_df, coord_cols = c("east", "north")) |> 
    dplyr::filter(river %in% considered) |> 
    dplyr::mutate(
      river_station = dplyr::case_when(
        river == "Isar" ~ "Isar_Station",
        river == "Donau" ~ "Donau_Station"
      )
    ) |> 
    # Because I only used Gumbel, Frank and Clayton: A station has either upper or lower or neigher taildependence, but never both
    dplyr::mutate(
      # Let lower taildependence have negative sign FOR DISPLAY PURPSE
      # Let upper tp have positive sign FOR DISPLAY PURPOSE
      signed_taildep =  upper - lower  
    ) 


  taildep_bayern = ggplot(rivers_bayern) + 
    # Displaying the actual data
    # 1) Make background look like map
    ggspatial::annotation_map_tile(type = "osm", zoom = 8) + 
    # 2) Add the rivers twice. 
    #   1st in black to give a outline for each river
    #   (combination of fill and color does not work for geom_sf)
    #   2nd the actual rivers in desired colors
    # geom_sf(color = "black", linewidth = 2, alpha = 0.5) + # Outlines for the rivers
    # geom_sf(linewidth = 1.5, color = "#00BfC4", alpha = 0.5) +
    geom_sf(data = rivers_bayern |> dplyr::filter(river == "Isar"), linewidth = 2, color = "black") +
    geom_sf(data = rivers_bayern |> dplyr::filter(river == "Isar"), linewidth = 1.5, color = "#F8766D") +
    geom_sf(data = rivers_bayern |> dplyr::filter(river == "Donau"), linewidth = 2, color = "black") +
    geom_sf(data = rivers_bayern |> dplyr::filter(river == "Donau"), linewidth = 1.5, color = "#7CAE00") +
    # 3) Add the shape of Bayern on top to highlight the relevant area
    geom_sf(data = bayern, fill = "white", alpha = 0.6, linewidth = .8) + 
    # 4) Add station
    # geom_sf(data = pos_sf, aes(color = river_station, shape = river_station, fill = river_station), size = 4) + 
    # Formatiing of Data
    theme_void() + 
    theme(
      # Remove the x and y ticks / labels
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      # Adjust the legend position and font size
      legend.position = "bottom",  # Move legend to top-right
      legend.margin = margin(1, 1, 1, 1),  # (top, right, bottom, left) padding
      # legend.text = element_text(size = 20),  # Adjust size of legend labels
      legend.title = element_text(size = 15),
      strip.text.x = element_text(size = 18)  # Adjust size of facet labels
    ) 
  
  plot( taildep_bayern + 
    geom_sf(data = taildep_pos_sf, aes(color = signed_taildep), size = 4) + 
    scale_color_gradient2(
      low = "black",   
      mid = "white",    
      high = "#7B3294",  
      midpoint = 0,
      limits = c(-1, 1),
      name = "Signed Tail.Dep."
    ) + 
    # scale_shape_manual(
    #   name = "Copula",
    #   values = c("Gumbel" = 15, "Clayton" = 16, "Frank" = 13, "rotated Gumbel" = 18, "rotated Clayton" = 17),
    # ) + 
    guides(
      fill = guide_legend(order = 1),
      color = guide_colorbar(barwidth = 10, barheight = 0.8, title.position = "top"),
      shape = guide_legend(nrow = 2, byrow = TRUE)  # Arrange legend neatly in 2 rows
    ) + 
    theme(strip.text = element_text(size = 10)) + 
    facet_wrap(~variables, labeller = 
                 as_labeller(c(
                   "D - P" = "Duration - Peak",
                   "D - V" = "Duration- Volume",
                   "P - V" = "Peak - Volume"
                 )))
  )
  if (save_plot) savegg(plotname)
}

get_univariate_HQ_plot = function(scop_df, ref_flood, gev_peak, save_plot = FALSE, plotname = "Univariate_HQ"){
  plot(ggplot(
      data = auc_plot_df <- data.frame(
        x = seq(from = 0, to = max(scop_df$peak)),
        y = dmarginal(seq(from = 0, to = max(scop_df$peak)), gev_peak)
      ),
      mapping = aes(x = x, y = y)
    ) + 
    geom_histogram(data = scop_df, mapping = aes(x = peak, y = after_stat(density)), fill = "white", alpha = 0.4, color = "black") + 
    geom_line(color = "blue", linewidth = 1) + 
    geom_area(data = auc_plot_df |> dplyr::filter(x >= ref_flood$peak), fill = "red", alpha = 0.4) + 
    geom_vline(xintercept = ref_flood$peak, color = "red") +
    labs(
      x = latex2exp::TeX("Peak ($m^3/s$)"),
      y = "Density",
      # title = "Munich station - GEV Fit on Variable 'Peak'"
    ) + ggtheme
  )
  message(paste("MESSAGE: Estimated event probability:", round(1 - pmarginal(ref_flood$peak, gev_peak), 2)))
  
  if (save_plot) savegg(plotname)
}


get_trivariate_HQ_plot = function(scop_df, ref_flood, vine, n_syn = 1e6, save_plot = FALSE, plotname = "Trivariate_HQ"){
  dens_df = get_density_values(vine, get_grid(), station)
  syn_df = get_synthetic_data(vine, n_syn, station)
  
  cont_dur_peak = get_contour(
    rel = "Duration - Peak", 
    splot_df = dens_df, 
    sdf = scop_df, 
    vary = "pobs_dur", 
    varx = "pobs_peak", 
    title = "Duration - Peak", 
    x_lab = "P.Obs. Peak",
    y_lab = "P.Obs. Duration",
    bwidth = 0.1,
    contour_alpha = 0.7
  ) +ggtheme+
    geom_point(data = ref_flood, aes(y = pobs_dur, x = pobs_peak), color = "red") +
    geom_segment(data = ref_flood, aes(y = 0, yend = 1, x = pobs_peak, xend = pobs_peak), color = "red") +
    geom_segment(data = ref_flood, aes(y = pobs_dur, yend = pobs_dur, x = 0, xend = 1), color = "red") +
    geom_rect(data = ref_flood, aes(xmin = pobs_peak, xmax = 1, ymin = pobs_dur, ymax = 1), fill = "red", alpha = 0.3)
  
  cont_peak_vol = get_contour(
    rel = "Peak - Volume", 
    splot_df = dens_df, 
    sdf = scop_df, 
    vary = "pobs_peak", 
    varx = "pobs_vol", 
    title = "Peak - Peak", 
    x_lab = "P.Obs. Volume",
    y_lab = "P.Obs. Peak",
    bwidth = 0.1,
    contour_alpha = 0.7
  )  + ggtheme+
     geom_point(data = ref_flood, aes(y = pobs_peak, x = pobs_vol), color = "red") +
    geom_segment(data = ref_flood, aes(y = 0, yend = 1, x = pobs_vol, xend = pobs_vol), color = "red") +
    geom_segment(data = ref_flood, aes(y = pobs_peak, yend = pobs_peak, x = 0, xend = 1), color = "red") +
    geom_rect(data = ref_flood, aes(ymin = pobs_peak, ymax = 1, xmin = pobs_vol, xmax = 1), fill = "red", alpha = 0.3)
    
  cont_dur_vol = get_contour(
    rel = "Duration - Volume", 
    splot_df = dens_df, 
    sdf = scop_df, 
    vary = "pobs_dur", 
    varx = "pobs_vol", 
    title = "Volume - Duration | Peak", 
    x_lab = "P.Obs. Volume",
    y_lab = "P.Obs. Duration",
    bwidth = 0.1,
    contour_alpha = 0.7
  )  + ggtheme+
     geom_point(data = ref_flood, aes(y = pobs_dur, x = pobs_vol), color = "red") +
    geom_segment(data = ref_flood, aes(y = 0, yend = 1, x = pobs_vol, xend = pobs_vol), color = "red") +
    geom_segment(data = ref_flood, aes(y = pobs_dur, yend = pobs_dur, x = 0, xend = 1), color = "red") +
    geom_rect(data = ref_flood, aes(ymin = pobs_dur, ymax = 1, xmin = pobs_vol, xmax = 1), fill = "red", alpha = 0.3)
  plot(
    (cont_dur_peak | cont_dur_vol | cont_peak_vol)
  )
  
  message(
    paste(
      "MESSAGE: Estimated event probability:", 
      VineCopula::RVineCDF(1 - ref_flood |> dplyr::select(vine$names), vine, N = 1e6)
    )
  )
  if (save_plot) savegg(plotname)
}

# Model Evaluation --------------------------------------------------------
model_evaluation = function(cop_df, nacs, vines, hq_probs, grid_size = 25, save_plot = F, plotname = "model_eval",optimization_plots = F, debug = F){
  res = lapply(
    cop_df$unit |> unique(),
    # c("Landshut Flutmulde"),
    function(stat) get_most_probable_voldur_by_unit(
      station = stat, 
      scop_df = cop_df |> dplyr::filter(unit == stat),
      nac = nacs[[stat]],
      vine = vines[[stat]],
      grid_size = grid_size, 
      hq_probs = hq_probs,
      optimization_plots = optimization_plots,
      debug = debug
    )
  ) |> 
    dplyr::bind_rows() |> 
    dplyr::left_join(cop_df |> dplyr::select(unit, river) |> unique(), by = "unit")
    
  emp_hq_df = cop_df |>
    dplyr::group_by(unit) |>
    dplyr::arrange(peak) |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    dplyr::mutate(
      avg_discharge = get_avg_discharge_from_vol_dur_pair(vol = volume, dur = duration_days),
      mean_avg_discharge = mean(avg_discharge),
      sd_avg_discharge = sd(avg_discharge),
      std_discharge = (avg_discharge - mean_avg_discharge) / sd_avg_discharge
    ) |>
    dplyr::filter(idx %in% c(nrow(scop_df) - floor(HQ_probs * nrow(scop_df)))) |>
    dplyr::select(-c(east, north, pobs_peak, pobs_dur, pobs_vol)) |>
    dplyr::ungroup() |>
    dplyr::arrange(id, idx) |>
    dplyr::group_by(unit) |>
    dplyr::mutate(
      HQ = HQs[1:dplyr::n()]
    ) |> 
    dplyr::ungroup()
  
  res = res |> dplyr::left_join(emp_hq_df |> dplyr::select(unit, tau_order) |> unique(), by = "unit")
  plot(
    emp_hq_df |>
    ggplot() +
    geom_boxplot(aes(x = as.factor(HQ), y = std_discharge)) +
    geom_line(data = res, aes(x = as.factor(hq), y = std_discharge, group = unit, color = tau_order), alpha = 0.5) + 
    theme(legend.position = "top") + 
    ggtheme + 
    facet_grid(type~river) + 
      scale_color_manual(
        name = "Largest τ",  # legend title
        values = c(
          "tau_dp<tau_vd<tau_vp" = "darkred",
          "tau_dp<tau_vp<tau_vd" = "darkblue"
        ),
        labels = c(
          "tau_dp<tau_vd<tau_vp" = "Volume–Peak",
          "tau_dp<tau_vp<tau_vd" = "Volume–Duration"
        )
      ) + 
      labs(y = "Standardized Average Discharge", x = "Return Period")
  )
  if (save_plot) savegg(plotname)

  # Calc error and print them as message by river and model
  calc_error(fit = res, emp = emp_hq_df)
}

calc_error = function(fit, emp) {
  # Calculate the mean absolute distance to the empirical median of every HQ-group 
  meds = emp |> 
    dplyr::rename(hq = HQ) |> 
    dplyr::summarise(med = median(std_discharge), .by = c(hq, river)) |> 
    dplyr::mutate(key = paste(hq, river, sep = "-")) |> 
    dplyr::select(-river, -hq)
  errors = fit |> 
    dplyr::mutate(key = paste(hq, river, sep = "-")) |> 
    dplyr::left_join(meds, by = "key") |>
    dplyr::mutate(error = std_discharge - med, abs_error = abs(error))
  
  msg_df = errors |> 
    dplyr::summarise(
      mad = mean(abs_error),
      .by = c(river, type)
    )
  message(
    paste(
      msg_df$river, "---", msg_df$type, "--- MAD:", msg_df$mad, "(mean absolute distance to hq median)\n"
    )
  )
}

get_most_probable_voldur_by_unit = function(station, scop_df, nac, vine, grid_size, hq_probs, optimization_plots = F, debug = F){
  if (debug) browser()
  
  gev_peak = marginal_fit(scop_df$peak, type = "GEV")
  gev_vol = marginal_fit(scop_df$volume, type = "GEV")
  gev_dur = marginal_fit(scop_df$duration_days, type = "GEV")
  
  con_nac = lapply(
    HQ_probs,
    function(hq_prob) get_most_probable_voldur(
        hq_prob = hq_prob,
        grid_size = grid_size,
        gev_vol = gev_vol,
        gev_dur = gev_dur,
        gev_peak = gev_peak,
        mdl = nac,
        mdl_type = "nac",
        optimization_plots = optimization_plots,
        station = station,
        trace = 0
      )
  ) 
  con_nac_grid = lapply(1:length(con_nac), function(i) con_nac[[i]]$grid) |> dplyr::bind_rows() |> dplyr::mutate(type = "nac")
  con_nac_estimates = lapply(1:length(con_nac), function(i) con_nac[[i]]$estimates) |> dplyr::bind_rows() |> dplyr::mutate(type = "nac")
  
  con_vine = lapply(
    HQ_probs,
    function(hq_prob) get_most_probable_voldur(
        hq_prob = hq_prob,
        grid_size = grid_size,
        gev_vol = gev_vol,
        gev_dur = gev_dur,   
        gev_peak = gev_peak,
        mdl = vine,
        mdl_type = "vine",
        optimization_plots = optimization_plots,
        station = station,
        trace = 0
      )
  ) 
  con_vine_grid = lapply(1:length(con_vine), function(i) con_vine[[i]]$grid) |> dplyr::bind_rows() |> dplyr::mutate(type = "vine")
  con_vine_estimates = lapply(1:length(con_vine), function(i) con_vine[[i]]$estimates) |> dplyr::bind_rows() |> dplyr::mutate(type = "vine")
  
  con_estimates = rbind(
    con_nac_estimates,
    con_vine_estimates
  ) |> dplyr::mutate(hq = 1 / hq_prob, .after = hq_prob)
  
  con_grids = rbind(
    con_nac_grid,
    con_vine_grid
  ) |> dplyr::mutate(hq = as.factor(1 / hq_prob))
  
  empirical_hqs = scop_df |> dplyr::arrange(peak) |> 
    dplyr::mutate(idx = dplyr::row_number()) |> 
    dplyr::filter(idx %in% c(nrow(scop_df) - floor(HQ_probs * nrow(scop_df)))) |> 
    dplyr::select(year, id, idx, duration_days, peak, volume) |> 
    dplyr::mutate(avg_discharge = get_avg_discharge_from_vol_dur_pair(vol = volume, dur = duration_days))
  empirical_hqs$hq = HQs[1:length(HQs)] 
  scop_df = scop_df |> dplyr::arrange(peak) |> 
    dplyr::mutate(idx = dplyr::row_number()) |> 
    # Use idx as start index to assign data points 
    add_hq_cat_col(empirical_hqs |> head(length(HQs) - 1 ) |> dplyr::mutate(hq = HQs[2:length(HQs)])) |> 
    dplyr::arrange(year)
  # First, check data with ellipses around PUT THIS INTO DATA PART?
  if (debug) plot(ggplot(con_estimates) + 
    geom_point(data = scop_df, aes(y = duration_days, x = volume, color = hq_cat, shape = hq_cat), size = 3) +
    geom_line(aes(x = vol, y = dur)) +
    geom_point(aes(x = vol, y = dur)) +
    geom_label(aes(x = vol, y = dur, label = 1 / hq_prob), size = 5) +
    geom_contour(data = con_grids, aes(y = dur, x = vol, z = z, group = hq, color = as.factor(hq)) , bins = 2) + 
    facet_wrap(~type) + 
    theme(
      title = element_text(size = 20),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      strip.text = element_text(size = 15)
    ) + 
    labs(
      # title = paste(station, "station - Most Likely Conditional Vol - Dur Pairs"),
      x = latex2exp::TeX("Volume (Mio. $m^3$)"),
      y = "Duration (days)"
    ) + 
    ggthemes::scale_color_colorblind()
  )
  con_estimates = con_estimates |> 
  dplyr::mutate(
    # Average discharge during flood event
    avg_discharge = get_avg_discharge_from_vol_dur_pair(vol = vol, dur = dur)
  )
  
  station_details = scop_df |> 
    dplyr::mutate(avg_discharge = get_avg_discharge_from_vol_dur_pair(vol = volume, dur = duration_days)) |> 
    dplyr::summarise(
      mean = mean(avg_discharge),
      sd = sd(avg_discharge),
      river = unique(river)
    )
  con_estimates$std_discharge = (con_estimates$avg_discharge - station_details$mean) / station_details$sd
  return(con_estimates |> dplyr::mutate(unit = station))
}




# Helper Functions --------------------------------------------------------
get_grid = function(min_x = 0.01, max_x = 0.99, min_y = 0.01, max_y = 0.999, size = 50, name_x = "x", name_y = "y"){
  x = seq(from = min_x, to = max_x, length.out = size)
  y = seq(from = min_y, to = max_y, length.out = size)
  xygrid = expand.grid(x = x, y = y) |> 
    as.data.frame() 
  colnames(xygrid) = c(name_x, name_y)
  
  return(xygrid)
}

get_syn_scatter = function(
  ssyn_df, 
  varx_syn,  
  vary_syn, 
  sdf,  
  varx_df, 
  vary_df,  
  syn_color = "lightblue", 
  syn_alpha = 0.3,
  x_lab = "x",
  y_lab = "y",
  x_min = NULL,
  x_max = NULL,
  y_min = NULL,
  y_max = NULL
){
p = ggplot() + 
  geom_point(data = ssyn_df, mapping = aes_string(x = varx_syn, y = vary_syn), color = syn_color, alpha = syn_alpha) + 
  geom_point(data = sdf, mapping = aes_string(x = varx_df, y = vary_df)) +
  labs(
    x = x_lab,
    y = y_lab
  ) 
  if (!is.null(c(x_min, x_max))) p = p + scale_x_continuous(breaks = c(x_min, x_max)) 
  if (!is.null(c(y_min, y_max))) p = p + scale_y_continuous(breaks = c(y_min, y_max))
return(p)
}

get_contour = function(
    rel, 
    splot_df, 
    sdf, 
    varx, 
    vary, 
    bwidth = 0.1, 
    plt_pts = TRUE, 
    title = "TITLE",
    x_lab = "x",
    y_lab = "y",
    contour_alpha = 1,
    z = "density"
  ){
  p = ggplot() +
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    geom_contour(data = splot_df |> dplyr::filter(vars == rel), aes_string(x = "x", y = "y", z = z), binwidth = bwidth, alpha = contour_alpha) + 
    labs(
      title = title,
      y = y_lab,
      x = x_lab 
    ) + 
    scale_x_continuous(breaks = c(0, 1)) + 
    scale_y_continuous(breaks = c(0, 1))
  if (plt_pts) p = p + geom_point(data = sdf, mapping = aes_string(x = varx, y = vary), alpha = .8)
  return(p)
}


get_density_values = function(vine, dgrid, unit_name){

  plot_df = as.data.frame(dgrid) |>
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    # 1-2: index [3, 1]
    # 1-3: index [2, 1]
    # 2-3: index [3, 2]
    dplyr::mutate(
      "Duration - Peak" = VineCopula::BiCopPDF(x, y, family = vine$family[3, 1], par = vine$par[3, 1]),
      "Duration - Volume" = VineCopula::BiCopPDF(x, y, family = vine$family[2, 1], par = vine$par[2, 1]),
      "Peak - Volume" = VineCopula::BiCopPDF(x, y, family = vine$family[3, 2], par = vine$par[3, 2])
    ) |>
    tidyr::pivot_longer(
      cols = contains("-"),
      names_to = "vars",
      values_to = "dens"
    ) |>
    # Standardize density values so scale does not matter
    dplyr::group_by(vars) |>
    dplyr::mutate(
      density = (dens - mean(dens)) / sd(dens)
    ) |> 
    dplyr::ungroup()
  # Add family 
  plot_df$fam = rep(c(f12 = vine$family[3, 1], f13 = vine$family[2, 1], f23 = vine$family[3, 2]), nrow(plot_df) / 3)
  plot_df$unit = unit_name
  return(plot_df)
}

get_synthetic_data = function(vine, n, unit_name){
  as.data.frame(VineCopula::RVineSim(n, RVM = vine)) |> dplyr::mutate(unit = unit_name)
}

get_contour = function(
    rel, 
    splot_df, 
    sdf, 
    varx, 
    vary, 
    bwidth = 0.1, 
    plt_pts = TRUE, 
    title = "TITLE",
    x_lab = "x",
    y_lab = "y",
    contour_alpha = 1,
    z = "density"
  ){
  p = ggplot() +
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    geom_contour(data = splot_df |> dplyr::filter(vars == rel), aes_string(x = "x", y = "y", z = z), binwidth = bwidth, alpha = contour_alpha) + 
    labs(
      title = title,
      y = y_lab,
      x = x_lab 
    ) + 
    scale_x_continuous(breaks = c(0, 1)) + 
    scale_y_continuous(breaks = c(0, 1))
  if (plt_pts) p = p + geom_point(data = sdf, mapping = aes_string(x = varx, y = vary), alpha = .8)
  return(p)
}


get_copname = function(int){
  cop_fams = list(
    "3" = "Clayton",
    "4" = "Gumbel",
    "5" = "Frank",
    "13" = "180 rotated Clayton",
    "14" = "180 rotated Gumbel",
    "15" = "180 rotated Frank",
    "33" = "270 rotated Clayton",
    "34" = "270 rotated Gumbel"
  )
  return(cop_fams[[as.character(int)]])
}

gkd2gg = function(
    df, 
    coord_cols, 
    current_crs = 25832, # ETRS25832
    into_crs = 4326 # EPSG4326
  ){
  "
  Takes positions given by GKD website (CooRdinateSystem[crs] = ETRS25832) and returns a coordinate system ggplot can work with (crs = EPSG4326).
  "
  return(
    sf::st_transform(
      sf::st_as_sf(df, coords = coord_cols, crs = current_crs),
      crs = into_crs
    )
  )
}

get_river_file = function(filepath = "../data/rivers.Rdata"){
  if (!file.exists(filepath)) {
  # Help: See 2.1 at https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
    rivers = osmdata::opq(bayern_bbx, timeout = 180) |>
      osmdata::add_osm_feature(key = "waterway", value = "river") |>
      osmdata::osmdata_sf()
    save(rivers, file = filepath)
  } else load(filepath)
  
  return(rivers)
}

get_cdf_values = function(vine, dgrid, unit_name){
  plot_df = as.data.frame(dgrid) |>
    # 1: pobs_dur, 2: pobs_peak, 3: pobs_vol
    # 1-2: index [3, 1]
    # 1-3: index [2, 1]
    # 2-3: index [3, 2]
    dplyr::mutate(
      "Duration - Peak" = VineCopula::BiCopCDF(x, y, family = vine$family[3, 1], par = vine$par[3, 1]),
      "Duration - Volume" = VineCopula::BiCopCDF(x, y, family = vine$family[2, 1], par = vine$par[2, 1]),
      "Peak - Volume" = VineCopula::BiCopCDF(x, y, family = vine$family[3, 2], par = vine$par[3, 2])
    ) |>
    tidyr::pivot_longer(
      cols = contains("-"),
      names_to = "vars",
      values_to = "cdf"
    ) 
  plot_df$unit = unit_name
  return(plot_df)
}

get_most_probable_voldur = function(
    hq_prob, station,
    mdl, mdl_type,
    gev_vol, gev_dur, gev_peak, 
    grid_size = 10, grid_pobs_min = 0.01, grid_pobs_max = 0.999,
    optimizer = "L-BFGS-B", 
    optimization_plots = F,
    trace = 1
    ){
  # 1) Create a grid to
    # 1a) Plot the contours of the conditional density
    # 1b) Select an initial value for the algorithm (i.e. that grid-point for which the density is max)
  # 2) Run the optimization algorithm using the previously found grid-maximizer as initial value
  # grid_df = get_grid(min = .01, max = .999, size = grid_size) |> as.data.frame() |> dplyr::rename(vol = x, dur = y)
  initial_vol_pobs = .5
  initial_dur_pobs = .5
  initial_vol = qmarginal(initial_vol_pobs, gev_vol)
  initial_dur = qmarginal(initial_dur_pobs, gev_dur)
  
  lower = c(
    qmarginal(1e-3, gev_vol),
    qmarginal(1e-3, gev_dur)
  )
  upper = c(
    qmarginal(0.999, gev_vol),
    qmarginal(0.999, gev_dur)
  )
  
  # Find most probable combination of vol and dur given a peak value
  # by maximizing the conditional density (using numerical approach)
  out = optim(
    par = c(vol = initial_vol, dur = initial_dur), # Initial values
    fn = cond_marginal_dens,
    peak = qmarginal(1 - hq_prob, gev_peak),
    marginal_vol = gev_vol,
    marginal_dur = gev_dur,
    marginal_peak = gev_peak,
    mdl = mdl, 
    mdl_type = mdl_type,
    min = T,
    method = optimizer,
    # hessian = T,
    lower = lower,
    # upper = upper,
    control = list(
      # Controls to improve numerical stability
      trace = trace,
      factr = 1e0,
      pgtol = 1e-16,
      parscale = pmax(upper - lower, 1e-8),
      max
    )
  )
  vol = out$par[1]
  dur = out$par[2]
  
  
  marginal_grid_df = data.frame()
  if (optimization_plots) {
    marginal_grid_df = get_grid(
      min_x = 0,  
      max_x = qmarginal(grid_pobs_max, gev_vol), 
      name_x = "vol",
      min_y = 0,
      max_y = qmarginal(grid_pobs_max, gev_dur),
      name_y = "dur",
      size = grid_size
    )
    marginal_grid_df$z = lapply(
      1:nrow(marginal_grid_df),
      function(i) cond_marginal_dens(
        vol_dur_vec = marginal_grid_df[i, ],
        peak = qmarginal(1 - hq_prob, gev_peak),
        marginal_vol = gev_vol,
        marginal_dur = gev_dur,
        marginal_peak = gev_peak,
        mdl = mdl,
        mdl_type = mdl_type
      )
    ) |> unlist() 
    marginal_grid_df = marginal_grid_df |> dplyr::mutate(z = (z - mean(z, na.rm = T)) / sd(z, na.rm = T))
    
     # Density contours
    marginal_contours = ggplot(marginal_grid_df, aes(x = vol, y = dur, z = z)) +
      geom_contour_filled() +
      geom_point(data = data.frame(vol = initial_vol, dur = initial_dur, z = 0)) +
      geom_point(data = data.frame(vol = vol, dur = dur, z = 0), color = "red") + 
      theme(legend.position = "none") + 
      # labs(title = paste(station, mdl_type, 1/hq_prob))
    plot(marginal_contours)
  }
  
  return(
    list(
      estimates = data.frame(vol = vol, dur = dur, hq_prob = hq_prob),
      grid = marginal_grid_df |> dplyr::mutate(hq_prob)
    )
  )
}

cond_marginal_dens = function(vol_dur_vec, peak, marginal_vol, marginal_dur, marginal_peak, mdl, mdl_type, min = F, factor = 1e0){
  vol = vol_dur_vec["vol"]
  dur = vol_dur_vec["dur"]
  
  uMatrix = cbind(
    pmarginal(dur, marginal_dur),
    pmarginal(peak, marginal_peak),
    pmarginal(vol, marginal_vol)
  )
  colnames(uMatrix) = c("pobs_dur", "pobs_peak", "pobs_vol")
  
  if (mdl_type == "nac"){
    copula_density = HAC::dHAC(X = as.matrix(uMatrix), hac = mdl) 
  } else if (mdl_type == "vine") {
    copula_density = VineCopula::RVinePDF(uMatrix, mdl)
  }
  
  # f(vol, dur | peak) = c(vol, dur, peak) f(vol) f(dur)
  f_vol = dmarginal(vol, marginal_vol)
  f_dur = dmarginal(dur, marginal_dur)
  conditional_marginal_density =  copula_density *  f_vol * f_dur
  
  if (min) conditional_marginal_density = -factor * log(conditional_marginal_density)
  return(conditional_marginal_density)
}

get_avg_discharge_from_vol_dur_pair = function(vol, dur){
  # convert duration days to seconds:
  dur_s = dur * 24 * 60 * 60 # days * h/day * min/h * s/min
  # Convert vol (in mio m^3) into vol (m^3)
  vol_mio = vol * 1e6
  return(vol_mio / dur_s)
}

add_hq_cat_col = function(df, hqs, init_hq_value = 2){
  df$hq_cat = init_hq_value
  
  for (hq_idx in 1:length(hqs$hq)) { #eq(from = length(hqs$hq), to = 1, by = -1)){
    current_hq = hqs$hq[hq_idx]
    hq_start_idx = hqs |> dplyr::filter(hq == current_hq) |> dplyr::select(idx) |> unlist()
    
    df = df |> dplyr::mutate(
      hq_cat = ifelse(idx >= hq_start_idx, current_hq, hq_cat)
    )
  }
  df$hq_cat = as.factor(df$hq_cat)
  return(df)
}


get_cond_boxplots = function(save_plot = F, plotname = "data_condBoxplots"){
  emp_hq_df = cop_df |>
    dplyr::group_by(unit) |>
    dplyr::arrange(peak) |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    dplyr::mutate(
      avg_discharge = get_avg_discharge_from_vol_dur_pair(vol = volume, dur = duration_days),
      mean_avg_discharge = mean(avg_discharge),
      sd_avg_discharge = sd(avg_discharge),
      std_discharge = (avg_discharge - mean_avg_discharge) / sd_avg_discharge
    ) |>
    dplyr::filter(idx %in% c(nrow(scop_df) - floor(HQ_probs * nrow(scop_df)))) |>
    dplyr::select(-c(east, north, pobs_peak, pobs_dur, pobs_vol)) |>
    dplyr::ungroup() |>
    dplyr::arrange(id, idx) |>
    dplyr::group_by(unit) |>
    dplyr::mutate(
      HQ = HQs[1:dplyr::n()]
    ) |> 
    dplyr::ungroup()
  
  plot(emp_hq_df |>
    ggplot() +
    geom_boxplot(aes(x = as.factor(HQ), y = std_discharge)) +
    ggtheme + 
    facet_wrap(~river) + 
      labs(y = "Standardized Average Discharge", x = "Return Period")
  )
  if(save_plot) savegg(plotname)
}
