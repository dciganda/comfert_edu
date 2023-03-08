optimize_comfert <- function(res_path, global_path, country, iniY, endY, ini_c, n0, nsim,
                             N, ne, params, priors, weights, nr_new_evals = 100){
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

  obs <- get_obs(country, ysd = 1960) # observed data
  
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
    params$mse <- sapply(sim, compute_mse, obs, weights)  
    
    # training gp
    cat("\n training gp...\n")
    gpe <- mlegp::mlegp(params[!is.na(params$mse),!names(params) == "mse"], 
                        params[!is.na(params$mse), "mse"], 
                        nugget = T, verbose = 0)
    
    gp <- list(params, gpe)
    
    new_set <- get_new_points(priors, n = nr_new_evals)
    
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
    output <- parallel_comfert(params = new_eval[rep(1:nrow(new_eval), each = nsim),],
                               country,
                               ini_c,
                               iniY,
                               endY)

    sink(sa)
    
    # Save Results
    save_res(results = output,
             pars = new_eval,
             seq = (n+1):(n+ne),
             res_path = res_path)
    
    sink()
    
    res_paths_ne <- lapply((n+1):(n+ne), function(x) file.path(paste0(res_path, "/param_set_", x)))
    
    # simulated rates
    sim <- get_sim(res_paths_ne, iniY, endY, nsim, obs,  
                   unplanned = length(obs$obs_unplanned)>0, 
                   unwanted = length(obs$obs_unwanted)>0,
                   desired = length(obs$obs_desired)>0,
                   ccf = length(obs$obs_ccf)>0,
                   ccf_edu = length(obs$obs_ccf_edu)>0)
    
    new_eval <- as.data.table(new_eval)
    new_eval[, mse := sapply(sim, compute_mse, obs, weights)]  
    
    params_mse <- rbind(params, new_eval)
    
    params <- rbind(params[, !names(params) == "mse"],
                    new_eval[, !"mse"])
    
    n <- n+ne
    print(n)
    
  }
  
  sink(sa)
  saveRDS(params_mse, paste0(paste0(global_path,"/post/posterior.rds")))
  saveRDS(params, paste0(paste0(global_path,"/params/parameters.rds")))
  sink()
}
