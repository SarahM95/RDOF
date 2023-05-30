##### Data Generation followed by p-value calculation -----

# Core Detection -----
if(!"numCores" %in% ls()) {
  numCores <- detectCores() - 1
}
cl <- makeCluster(numCores)
registerDoParallel(cl)


# Number of iterations -----
if(!"n_sim" %in% ls()){
  n_sim <- 10000
}


# Setting parameters -----
if(!"params_stroke" %in% ls()){
  params_stroke <-
    c("uniform",
      "linear increase",
      "increase of intermediate category",
      "geometric",
      "stroke")
}
prob_params_stroke <- asplit(expand.grid(params_stroke, params_stroke), 1)
names(prob_params_stroke) <- sapply(1:length(prob_params_stroke), function(x) paste(
  prob_params_stroke[[x]][1],
  prob_params_stroke[[x]][2],
  sep = " - "
))

if(!"categories" %in% ls()){
  categories <- list(3, 5, 7, 9) 
}
names(categories) <- c(3, 5, 7, 9)

if(!"n_obs" %in% ls()){
  n_obs <- list(30, 50, 100, 200, 300, 500)
}
names(n_obs) <- c(30, 50, 100, 200, 300, 500)


# Data Generation with proper seed setting followed by the calculation of the performance measures -----
## set seeds 
rng <- RNGseq(n_sim, 11177000)

## Simulation
sim_methods <- foreach::foreach (
  nobs = n_obs,
  .final = function(nobs)
    setNames(nobs, names(n_obs))
) %:%
  foreach::foreach (
    k = categories,
    .final = function(k)
      setNames(k, names(categories))
  ) %:%
  foreach::foreach (
    setting = prob_params_stroke,
    .final = function(setting)
      setNames(setting, names(prob_params_stroke))
  ) %:%
  # Simulation
  foreach::foreach(i = 1:n_sim, .combine = rbind) %dopar% {
    ## set seed
    rngtools::setRNG(rng[[i]])
    
    ## data generation and method calculation
    method_performance(data_generation(
      n = nobs,
      K = k,
      param = setting[1],
      param_control = setting[2]
    ))[, c("K", "treat_control_param", "n_sim", "n_obs") := list(k, paste(setting[1], "-", setting[2]), i, nobs)]
  }

stopCluster(cl)

saveRDS(sim_methods, "results/sim_methods.RDS")

rm(categories,
   cl,
   n_obs,
   prob_params_stroke,
   rng,
   n_sim,
   numCores,
   params_stroke)
