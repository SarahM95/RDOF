##### Calculating rejection rate and aggregating all simulations in one dataset -----

# Generate simulated dataset if it does not exist in the environment -----
if(!"sim_methods" %in% ls()){
  source("code/data_generation.R")
}


# Calculating rejection rate and aggregation -----
simulation <- foreach::foreach(nobs = as.list(names(sim_methods)),
                         .combine = rbind) %:%
  foreach::foreach(k = as.list(names(sim_methods[[nobs]])),
                   .combine = rbind) %:%
  foreach::foreach(methods = sim_methods[[nobs]][[k]],
                   .combine = rbind) %dopar% {
                     data.table::setDT(methods)
                     
                     for (m in levels(as.factor(methods$method))) {
                       methods[method == m, rejection := p_value <= 0.05]
                       methods[method == m, cum_rejection := cumsum(rejection) / seq_along(rejection)]
                       methods[method == m, rejection_rate := mean(p_value <= 0.05, na.rm = TRUE)]
                       methods[method == m, monte_carlo_se := sqrt((rejection_rate * (1 - rejection_rate)) / .N)]
                     }
                     
                     return(methods)
                   }

saveRDS(simulation, "results/simulation.RDS")

rm(numCores, cl, sim_methods)

