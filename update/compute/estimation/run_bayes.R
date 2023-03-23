run_bayes <- function(global_path, res_path, hparam,
                      priors, fix_p, id, analysis = F){
  
  hparam_list <- names(hparam)
  for(i in hparam_list){assign(i, hparam[[i]], envir = globalenv())}
  
  source(file.path("..","estimation","get_new_points.R"))
  source(file.path("..","estimation","parallel_comfert.R"))
  source(file.path("..","estimation","optimize_comfert.R"))
  source(file.path("..","estimation","save_res.R"))
  source(file.path("..","estimation","get_known_param.R"))
  source(file.path("..","estimation","compute_fake_obs.R"))
  source(file.path("..","estimation","log.R"))
  
  if(analysis){
    
    known_param <- get_known_param(priors, random = T, mid = F, given = F)   
    
    compute_fake_obs(param = known_param[rep(seq_len(nrow(known_param)), each = nsim),],
                     fixed_param = fixed_param,
                     pop = pop,
                     ini_c = ini_c,
                     iniY = iniY,
                     endY = endY)
    
    dir.create(file.path(global_path, "known_param"), recursive = T)
    saveRDS(known_param, file.path(global_path,"known_param","known_param.rds"))
    
  }
  
  param <- get_new_points(priors, n=n0) # Initial sample of parameter combinations  
  
  
  # Compute model at initial parameter set 
  output <- parallel_comfert(param = param[rep(seq_len(nrow(param)), each = nsim),],
                             hparam) 
  
  # Save Results
  save_res(results = output,
           pars = param,
           seq = 1:n0, 
           res_path = res_path,
           nsim = nsim)

  # Bayesian optimization
  optimize_comfert(res_path = res_path,
                   global_path = global_path,
                   param = param,
                   hparam = hparam,
                   priors = priors,
                   fake_obs = analysis) 
  
  # Log ####
  log(hparam, id)
  
}