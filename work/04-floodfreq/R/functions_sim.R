"
Functions used during the simulation process. 
File does not need to be loaded separately as it is loaded when functions.R is. 
"

get_nac_tau = function(tauvec){
  # Based on 3 taus, return largest and build average above the other two
  # Assumption: 3rd is largest tau
  tau = c(mean(tauvec[1:2]), tauvec[3])
  return(tau)
}

nac_tau2theta = function(family_index, tau){
  return(
    c(
      HAC::tau2theta(tau = tau[1], type = family_index),
      HAC::tau2theta(tau = tau[2], type = family_index)
    )
  )
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

fit_ac = function(mat, cops, debug = FALSE){
  # HAC does not deal well with non-nested ACs, i.e. symmetric ACs. The fit seems to work, but then the logLikelihood cannot be evaluated
  # Solution: copula Package: Can fit ONLY non-nested ACs for more than 2 variables
  # Copula package uses bbmle to fit (see: https://cran.r-project.org/web/packages/bbmle/bbmle.pdf)
  # Due to the implementation in this package: 1) fit possible models 2) Use AIC to select the best one 
  if (debug) browser()
  ac_fits = lapply(cops, function(cop) copula::emle(u = mat, cop = copula::onacopula(family = cop, nacStructure = C(1, 1:3))))
  
  # Select the one with smallest AIC
  # NOTE HERE: Since p identical, we can just select smallest negative loglikelihood
  ac_lls = lapply(ac_fits, function(fit) - fit@min) # min gives the NEGATIVE loglikelihood
  ac_best_fit = which.max(ac_lls)
  ac_mle = ac_fits[[ac_best_fit]]@coef[[1]]
  
  # Actual copula
  ac = copula::onacopulaL(family = cops[ac_best_fit], nacList = list(ac_fits[[ac_best_fit]]@coef, 1:3))
  attr(ac, "logLik") = ac_lls[ac_best_fit]
  
  return(ac)
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

fit_vine = function(mat, families){
  # Families: 3, 4, 5 (see doku)
  vine = VineCopula::RVineStructureSelect(data = mat, rotations = FALSE, familyset = families)
  return(vine)
}

get_nac_name = function(idx, inverse_copula_families = list("1" = "Gumbel", "3" = "Clayton", "5" = "Frank")){
  return(inverse_copula_families[as.character(idx)][[1]])
}

get_ac_estimate = function(ac) unname(ac@copula@tau( ac@copula@theta ))

run_one_nac = function(
    seed,
    n,
    cop,
    taus,
    copula_families = list("Gumbel" = 1, "Clayton" = 3, "Frank" = 5) # Copula families contained in the package
    ){
  # Simulate from true model ------------------------------------------------
  # For reproducibility
  set.seed(seed)
  
  # Draw random tau vector and determine nac structure from this
  rtau = sample(taus, 1)
  river = names(rtau)
  tau = get_nac_tau(tauvec = rtau[[1]])
  theta = nac_tau2theta(family_index = copula_families[cop], tau = tau)
  
  # Create HAC-object
  mdl = HAC::hac.full(type = copula_families[cop], y = c("v3", "v2", "v1"), theta = theta)
  # Draw sample according to true_mdl (simulated sample)
  mat = HAC::rHAC(n, mdl) 
  
  # Fit models --------------------------------------------------------------
  ac = fit_ac(mat, names(copula_families))
  
  nac = fit_nac(mat, copula_families)
 
  vine = fit_vine(mat, families = c(3, 4, 5))
  
  # Save results ------------------------------------------------------------
  res = data.frame(
    list(
      # True values
      seed = seed,
      river = river,
      true_tau_outer = tau[1],
      true_theta_outer = theta[1],
      true_tau_inner = tau[2],
      true_theta_inner = theta[2],
      # Estimated values
      fit_cop = get_nac_name(nac),
      fit_theta_outer = attr(nac, "theta")[1],
      fit_theta_inner = attr(nac, "theta")[2],
      fit_tau_outer = attr(nac, "tau")[1],
      fit_tau_inner = attr(nac, "tau")[2],
      nac_aic = ll2aic(ll = attr(nac, "logLik")[[1]], p = 2),
      nac_kl = klMonteCarlo(mdl, nac),
      # Other copulas / Misspecifications
      # Results of AC fit
      ac_tau = get_ac_estimate(ac),
      ac_aic = ll2aic(ll = attr(ac, "logLik")[[1]], p = 1),
      ac_kl = klMonteCarlo(mdl, HAC::nacopula2hac(ac), est_mdl_AC = TRUE),
      # Results for Vine
      vine_aic = vine$AIC,
      vine_kl = klMonteCarlo(mdl, vine, est_mdl_vine = TRUE)
    )
  )
  return(res)
}

run_one_vine = function(
    seed,
    n,
    taus,
    vine_copula_families = list("Gumbel" = 4, "Clayton" = 3, "Frank" = 5), # Copula families contained in the VineCopula package
    hac_copula_families = list("Gumbel" = 2, "Clayton" = 3, "Frank" = 5), # Copula families contained in HAC copula package
    vine_colnames = c("1-3", "2-3", "1-2")
    ){
  # Simulate from true model ------------------------------------------------
  # For reproducibility
  set.seed(seed)

  # Draw random tau vector and determine nac structure from this
  rtau = sample(taus, 1)
  river = names(rtau)
  tau = unlist(rtau)
  names(tau) = vine_colnames
  
  # Vine matrix defining the (un)conditional copulas in the model
  # 1 - 2 - 3 
  vine_matrix = matrix(
    c(
      1, 0, 0,
      3, 2, 0,
      2, 3, 3
    ),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )

  # Randomly drawing the combination of copula families
  fams = unlist(sample(vine_copula_families, size = 3, replace = T))
  names(fams) = vine_colnames
  # Save families in family copula matrix
  family_matrix = matrix(0, nrow = 3, ncol = 3)
  family_matrix[2, 1] = fams["1-3"]
  family_matrix[3, 1:2] = fams[c("1-2", "2-3")]

  params = matrix(0, nrow = 3, ncol = 3)
  theta = c(
    "1-3" = VineCopula::BiCopTau2Par(fams["1-3"], tau["1-3"]),
    "1-2" = VineCopula::BiCopTau2Par(fams["1-2"], tau["1-2"]),
    "2-3" = VineCopula::BiCopTau2Par(fams["2-3"], tau["2-3"])
  )
  params[2, 1] = theta["1-3"]
  params[3, 1:2] = c(theta["1-2"], theta["2-3"])

  rvmat = VineCopula::RVineMatrix(Matrix = vine_matrix, family = family_matrix, par = params)
  mat = VineCopula::RVineSim(n, RVM = rvmat)

  attr(mat, "rvm") = rvmat
  colnames(mat) = c("v1", "v2", "v3")
  
  # Fit models --------------------------------------------------------------
  ac = fit_ac(mat, names(hac_copula_families))

  nac = fit_nac(mat, hac_copula_families)

  vine = fit_vine(mat, families = c(3, 4, 5))

  # Save results ------------------------------------------------------------
  res = data.frame(
    list(
      # True values
      seed = seed,
      river = river,
      true_tau_12 = tau["1-2"],
      true_fam_12 = fams["1-2"],
      true_theta_12 = theta["1-2"],
      true_tau_23 = tau["2-3"],
      true_fam_23 = fams["2-3"],
      true_theta_23 = theta["2-3"],
      true_tau_13 = tau["1-3"],
      true_fam_13 = fams["1-3"],
      true_theta_13 = theta["1-3"],
      # Estimated values
      fit_tau_12 = vine$tau[3, 1],
      fit_fam_12 = vine$family[3, 1],
      fit_theta_12 = vine$par[3, 1],
      fit_tau_23 = vine$tau[3, 2],
      fit_fam_23 = vine$family[3, 2],
      fit_theta_23 = vine$par[3, 2],
      fit_tau_13 = vine$tau[2, 1],
      fit_fam_13 = vine$family[2, 1],
      fit_theta_13 = vine$par[2, 1],
      vine_aic = vine$AIC,
      vine_kl = vine_klMonteCarlo(rvmat, vine, est = "vine"),
      # Other copulas / Misspecifications
      # NAC fit
      nac_tau_outer = attr(nac, "tau")[1],
      nac_tau_inner = attr(nac, "tau")[2], 
      nac_aic = ll2aic(ll = attr(nac, "logLik")[[1]], p = 2),
      nac_kl = vine_klMonteCarlo(rvmat, nac, est = "nac"),
      # AC fit
      ac_aic = ll2aic(ll = attr(ac, "logLik")[[1]], p = 1),
      ac_kl = vine_klMonteCarlo(rvmat, HAC::nacopula2hac(ac), est = "ac")
    )
  )
  return(res)
}

vine_klMonteCarlo = function(
    rvmat,
    est_mdl, 
    est = "vine", 
    values =  seq(from = 0.01, to = 0.99, length.out = 25) # Keep from (to) relatively high (low) simmplifying numerical stability. Downside: KL not considering full support..........lol.
  ){
  grid = expand.grid(v1 = values, v2 = values, v3 = values)
  
  true_d = VineCopula::RVinePDF(grid, rvmat)
  
  # ifelse cannot return matrices, aparently. Thus, keep it in two if-statements...
  if (est == "vine") est_d = VineCopula::RVinePDF(grid, est_mdl)
  if (est == "ac") for (i in 1:3) est_mdl$tree[[i]] = paste("v", i, sep = "")
  if (est != "vine") est_d = HAC::dHAC(as.matrix(grid), est_mdl)

  return(mean(log(true_d / est_d) ))
}

get_vine_famname = function(idx){
  vine_famlist = list("4" = "Gumbel", "3" = "Clayton", "5" = "Frank")
  return(unname(unlist(lapply(idx, function(i) vine_famlist[as.character(i)]))))
}

ll2aic = function(ll, p){
  return(-2 * ll + 2 * p)
}

klMonteCarlo = function(
    true_mdl, 
    est_mdl, 
    est_mdl_AC = FALSE, 
    est_mdl_vine = FALSE, 
    values =  seq(from = 0.01, to = 0.99, length.out = 25) # Keep from (to) relatively high (low) simmplifying numerical stability. Downside: KL not considering full support..........lol.
                                                           # This implies that I am "numerically blind" for differences below or above these thresholds. 
  ){
  "
  Function to numerically approximate the KL divergence.
  Not fully reliable tbh... have to work on it. But not now, I postpone this until later.
  "
  # Ensure correct names (Is an issue when dealing with copula to HAC transformed copula. Cannot deal with it any other way aparently)
  if (est_mdl_AC) for (i in 1:3) est_mdl$tree[[i]] = paste("v", i, sep = "")
  
  # Grid points where the copula density is evaluated on
  grid = expand.grid(v1 = values, v2 = values, v3 = values)
  
  # Densities
  true_d = HAC::dHAC(as.matrix(grid), true_mdl)
  # ifelse cannot return matrices, aparently. Thus, keep it in two if-statements...
  if (est_mdl_vine) est_d = VineCopula::RVinePDF(newdata = grid, RVM = est_mdl)
  if (!est_mdl_vine) est_d = HAC::dHAC(as.matrix(grid), est_mdl)

  # Numerically estimate E[log(p/q)] by mean(log(p/q)) 
  # The way I calculate KL now lead to NAs for:
  # tau_inner = 0.2011794
  # tau_outer = 0.1005897
  # Not sure why tho...
  return(mean(log(true_d / est_d) ))
}

# Evaluate Sim ------------------------------------------------------------
read_dep_files = function(
    true_dep ,
    in_dir = "../data/simulation/"
  ){
  filenames = paste(
    in_dir, 
    list.files(in_dir, pattern = paste("dep", true_dep, sep = "")),
    sep = ""
  )
  
  return(purrr::map_dfr(filenames, load_depdata))
}

load_depdata = function(filepath){
  # IMPORTANT! Assumes only 1 object / df within the rdata file
  # Note: Use new environment for each load to prevent overwriting within lapply
  env = new.env()
  load(filepath, envir = env)
  get(ls(env)[1], envir = env)
  
  # Append attributes to df. Necessary before merging them into one big df
  n = attr(env$res, "n")
  dep = attr(env$res, "dep")
  cop = attr(env$res, "cop")
  if (dep == "vine") cop = "vine"
  
  # Sanity messages
  message(paste("n:", n, "-- cop:", cop, "-- dep:", dep,"-- #seeds:", nrow(env$res)))
  
  return(env$res <- env$res |> dplyr::mutate(n = n, cop = cop, dep = dep, .after = seed))
}

get_nac_bias_in_vine_dgp = function(in_dir = "../data/simulation/", save_plot = F, plotname = "sim_nac_if_vine"){
  plot(read_dep_files(true_dep = "vine", in_dir = in_dir) |>
    dplyr::mutate(
      true_famname_12 = get_vine_famname(true_fam_12),
      true_famname_13 = get_vine_famname(true_fam_13),
      true_famname_23 = get_vine_famname(true_fam_23),
      famcomb = as.factor(
        paste(
          substr(true_famname_12, start = 1, stop = 1),
          substr(true_famname_23, 1, 1),
          substr(true_famname_13, 1, 1),
          sep = "-"
        )
      )
    ) |> 
    dplyr::mutate(
      x = dplyr::row_number(),
      .by = c(river, n)
    ) |> 
    ggplot() +
    geom_line(aes(y = true_tau_12, x = x)) + 
    geom_line(aes(y = true_tau_23, x = x)) + 
    geom_line(aes(y = true_tau_13, x = x)) + 
    geom_line(aes(y = nac_tau_outer, x = x), color = "blue", alpha = 0.4) + 
    geom_line(aes(y = nac_tau_inner, x = x), color = "red", alpha = 0.4) + 
    labs(x = "Iteration", y = "Correlation") + 
    scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) + 
    ggtheme + 
    facet_grid(
      river ~ n, 
      scale = "free_x", 
      labeller = labeller(
        river = c(
          "LowMedHigh"  = "LMH",
          "LowHighHigh" = "LHH"
          )
        )
      ) 
    )
  if (save_plot) savegg(plotname)
}

