

compute_mse <-  function(sim, obs, weights){
  
    sse <- list()
    scale_01 <- function(x, max, min){return((x-min)/(max-min))}
    
    for(i in sub(".*obs_", "", names(obs))){
      
      sim_ls <- lapply(sim[paste0("sim_", i)][[1]], function(x) x)
      print(sim_ls)
      obs_ls <- obs[paste0("obs_", i)][[1]] 
      
      all <- c(unlist(sim_ls),unlist(obs_ls))
      scaled_sim <- lapply(sim_ls, scale_01, max(all, na.rm = T), min(all, na.rm = T))
      scaled_obs <- scale_01(obs_ls, max(all, na.rm = T), min(all, na.rm = T))
      
      
      if(i == "asfr"){
        
      sse[[i]] <- sapply(scaled_sim, function(x){sum((x - scaled_obs)^2) / ncol(x)}) # years not individual rates
        
      }else{
      
      sse[[i]] <- sapply(scaled_sim, function(x){sum((x - scaled_obs)^2) / length(x)})
      }
      
    }
    
    mse <- sapply(sse, mean)
    
    mses <- cbind(mse, weights = weights[names(mse)])
    
    mse <- sum(mses[,"mse"] * mses[,"weights"])
  
  return(mse)
  
}



