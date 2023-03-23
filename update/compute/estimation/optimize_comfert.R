optimize_comfert <- function(res_path, global_path, param, 
                             hparam, priors, fake_obs = F){
  # OS
  switch(Sys.info()[['sysname']],
         Windows= {sa <- "NUL"},
         Linux  = {sa <- "/dev/null"},
         Darwin = {sa <- "/dev/null"})
  
  source(file.path("..","estimation","get_obs.R"))
  source(file.path("..","estimation","get_sim.R"))
  source(file.path("..","estimation","get_new_points.R"))
  source(file.path("..","estimation","parallel_comfert.R"))
  source(file.path("..","estimation","compute_mse.R"))
  source(file.path("..","estimation","save_res.R"))
  
  
  # estimate gp, eval new points and repeat  
  n <- n0

  obs <- get_obs(pop, ysd = 1960) # observed data
  
  while (n < (n0+N)) {
    
    # main dir
    param_dir <- dir(res_path, pattern = "param_set", full.names = T)
    
    # reorder results in numerical order (1-nrpops)
    order_res <- sapply(X = strsplit(param_dir, split = "param_set_"), 
                        FUN = function(x){rev(x)[1]})
    res_paths <- param_dir[match(1:length(param_dir), order_res)]
    
    sim <- get_sim(res_paths, iniY, endY, nsim, obs,  
                   unplanned = length(obs$obs_unplanned)>0, 
                   unwanted = length(obs$obs_unwanted)>0,
                   desired = length(obs$obs_desired)>0,
                   ccf = length(obs$obs_ccf)>0,
                   ccf_edu = length(obs$obs_ccf_edu)>0) # simulated data
    
    # compute mse
    param$mse <- sapply(sim, compute_mse, obs, weights)  
    
    # training gp
    cat("\n training gp...\n")
    gpe <- mlegp::mlegp(param[!is.na(param$mse),!names(param) == "mse"], 
                        param[!is.na(param$mse), "mse"], 
                        nugget = T, verbose = 0, parallel = T)
    
    gp <- list(param, gpe)
    
    new_set <- get_new_points(priors, n = n0*2)
    
    pred <- predict.gp(gp[[2]], newData = new_set, se.fit = T)
    
    mu <- pred$fit
    sigma <- pred$se.fit
    
    kappa_af <- 2
    
    lcb <- mu - kappa_af * sigma
    
    if(nrow(new_set[which(lcb %in% sort(lcb)[1:ne]),]) > ne){
      cat("\n warning: multiple identical predictions")
      cat("\n printing prediction object: \n")
      
      print(lcb)
    }
    
    new_eval <- new_set[which(lcb %in% sort(lcb)[1:ne]),][1:ne,]
    
    # compute the model at new points
    cat("\n computing new evaluations...\n")
    output <- parallel_comfert(param = new_eval[rep(1:nrow(new_eval), each = nsim),],
                     hparam)
    
    sink(sa)
    
    # Save Results
    save_res(results = output,
             pars = new_eval,
             seq = (n+1):(n+ne),
             res_path = res_path,
             nsim = nsim)
    
    sink()
    
    res_paths_ne <- lapply((n+1):(n+ne), function(x) file.path(res_path, paste0("param_set_", x)))
    
    # simulated rates
    sim <- get_sim(res_paths_ne, iniY, endY, nsim, obs,  
                   unplanned = length(obs$obs_unplanned)>0, 
                   unwanted = length(obs$obs_unwanted)>0,
                   desired = length(obs$obs_desired)>0,
                   ccf = length(obs$obs_ccf)>0,
                   ccf_edu = length(obs$obs_ccf_edu)>0)
    
    new_eval <- as.data.table(new_eval)
    new_eval[, mse := sapply(sim, compute_mse, obs, weights)]  
    
    params_mse <- rbind(param, new_eval)
    
    param <- rbind(param[, !names(param) == "mse"],
                    new_eval[, !"mse"])
    
    n <- n+ne
    print(n)
    
  }
  
  sink(sa)
  
  dir.create(file.path(global_path, "post"), recursive = T)
  dir.create(file.path(global_path, "param"), recursive = T)
  dir.create(file.path(global_path, "gp"), recursive = T)
  dir.create(file.path(global_path, "sim_trajectories"), recursive = T)
  
  saveRDS(params_mse, file.path(global_path,"post","posterior.rds"))
  saveRDS(param, file.path(global_path,"param","parameters.rds"))
  saveRDS(gpe, file.path(global_path,"gp","gp.rds"))
  
  sink()
}
