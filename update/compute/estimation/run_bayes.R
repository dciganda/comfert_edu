run_bayes <- function(global_path, res_path, 
                      pop, iniY, endY, ini_c,
                      n0, nsim, ne, N,
                      weights, priors, fix_p){
  
  
  source(file.path("..","estimation","get_new_points.R"))
  source(file.path("..","estimation","parallel_comfert.R"))
  source(file.path("..","estimation","optimize_comfert.R"))
  source(file.path("..","estimation","save_res.R"))
  
  
  params <- get_new_points(priors, n=n0) # Initial sample of parameter combinations  
  
  
  # Compute model at initial parameter set 
  output <- parallel_comfert(params = params[rep(seq_len(nrow(params)), each = nsim),],
                             pop = pop,
                             ini_c = ini_c,
                             iniY = iniY,
                             endY = endY) 
  
  # Save Results
  save_res(results = output,
           pars = params,
           seq = 1:n0, 
           res_path = res_path,
           delete = TRUE)
  
  dir.create(file.path(global_path, "post"), recursive = T)
  dir.create(file.path(global_path, "params"), recursive = T)
  dir.create(file.path(global_path, "sim_trajectories"), recursive = T)
  
  # Bayesian optimization
  optimize_comfert(res_path = res_path,
                   global_path = global_path,
                   pop = pop,
                   iniY = iniY,
                   endY = endY,
                   ini_c = ini_c,
                   n0 = n0,
                   nsim = nsim,
                   N = N,
                   ne = ne,
                   params = params,
                   priors = priors,
                   weights = weights) 
}