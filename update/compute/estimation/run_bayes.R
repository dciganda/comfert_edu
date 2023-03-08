run_bayes <- function(pop, iniY, endY, ini_c,
                               n0, nsim, ne, iter, N,
                               weights, priors, fix_p){
  
  
  source(file.path("..","estimation","get_new_points.R"))
  source(file.path("..","estimation","parallel_comfert.R"))
  source(file.path("..","estimation","optimize_comfert.R"))
  source(file.path("..","estimation","save_res.R"))
  
  
  # directory to store results
  global_path <- file.path("results", paste(country),
                           paste0("n_sim_", nsim),
                           paste0("ini_c_", ini_c),
                           paste(weights, collapse = "_"))
  
  res_path <- file.path(global_path,"results")
  
  
  params <- get_new_points(priors, n=n0) # Initial sample of parameter combinations  
  
  
  # Compute model at initial parameter set 
  output <- parallel_comfert(params = params[rep(seq_len(nrow(params)), each = nsim),],
                             country = country,
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
  optimize_comfert(res_path,
                   global_path,
                   country, 
                   iniY,
                   endY,
                   ini_c,
                   n0,
                   nsim,
                   N, ne,
                   params,
                   priors,
                   weights) 
}