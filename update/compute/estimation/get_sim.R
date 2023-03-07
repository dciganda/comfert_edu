
# get simulated rates
get_sim <- function(res_dir, iniY, endY, nsim, obs, 
                    asfr = T, unplanned = F, unwanted = F,
                    desired = F, ccf_edu = F, all_sim = F){
  
  get_sim_results <- function(res_dir, iniY, endY, obs){ 
    
    res_names <- list.files(res_dir, "RData", full.names = TRUE)
    
    if(class(obs) == "matrix"){
      obs_rnames <- rownames(obs)
      obs_cnames <- colnames(obs)
    }else{
      obs_rnames <- rownames(obs$obs_asfr)
      obs_cnames <- colnames(obs$obs_asfr)
    }
    
    # asfr
    asfr <- sapply(res_names, function(x) readRDS(x)["asfrt"])
    names(asfr) <- rep("asfr", length(asfr))
    
    sim_asfr_0 <- lapply(asfr,
                         function(x){colnames(x) <- paste0("age", 14:50);
                         x <- x[-1,];
                         x <- x[1:length(iniY:(endY-1)),];
                         rownames(x) <- iniY:(endY-1);
                         return(as.matrix(x))})
    
    
    sim_asfr <- lapply(sim_asfr_0,
                       function(x){x[rownames(x) %in% obs_rnames,
                                     colnames(x) %in% obs_cnames]})
    sim_data <- list()
    sim_data[["sim_asfr"]] <- sim_asfr
    
    # unplanned births
    if(unplanned){
      if(length(obs$obs_unplanned)>0){
        c_names <- colnames(obs$obs_unplanned) 
      }else{
        c_names <- "proportion.unplanned"  
      }
        unp <- sapply(res_names, function(x) readRDS(x)["ppBirths"])
        names(unp) <- rep("un", length(unp))
        
        sim_unp_0 <- lapply(unp,
                            function(x) {prop <- (x[,1] - x[,2]) / x[,1];
                            return(as.matrix(prop))})
        sim_unp <- lapply(sim_unp_0,
                          function(x){x <- as.matrix(x[1:length(iniY:(endY-1)),]);
                          colnames(x) <- c_names;
                          rownames(x) <- iniY:(endY-1);
                          return(as.matrix(x))})
        
        if(all_sim){
          
          sim_unplanned <- sim_unp
          
        }else{
          
          sim_unplanned <- lapply(sim_unp,
                                  function(x){x <- x[rownames(x) %in% rownames(obs$obs_unplanned),
                                                     colnames(x) %in% colnames(obs$obs_unplanned)];
                                  x <- as.matrix(x);
                                  colnames(x) <- colnames(obs$obs_unplanned);
                                  return(x)})
        }
        
        sim_data[["sim_unplanned"]] <- sim_unplanned
    }
    # unwanted
    if(unwanted){
      if(length(obs$obs_unwanted)>0){
        c_names <- colnames(obs$obs_unwanted) 
      }else{
        c_names <- "prop.unwanted"  
      }
        sim_unw_0 <- lapply(unp,
                            function(x) {prop <- (x[,1] - x[,3]) / x[,1];
                            return(as.matrix(prop))})
        sim_unw <- lapply(sim_unw_0,
                          function(x){x <- as.matrix(x[1:length(iniY:(endY-1)),]);
                          colnames(x) <- c_names;
                          rownames(x) <- iniY:(endY-1);
                          return(as.matrix(x))})
        if(all_sim){
          
          sim_unwanted <- sim_unw
          
        }else{
          sim_unwanted <- lapply(sim_unw,
                                 function(x){x <- x[rownames(x) %in% rownames(obs$obs_unwanted),
                                                    colnames(x) %in% colnames(obs$obs_unwanted)];
                                 x <- as.matrix(x);
                                 colnames(x) <- colnames(obs$obs_unwanted);
                                 return(x)})
        }
        
        sim_data[["sim_unwanted"]] <- sim_unwanted
        
    }
    #desired
    if(desired){
      if(length(obs$obs_desired)>0){
        c_names <- colnames(obs$obs_desired) 
      }else{
        c_names <- "d"  
      }
        dKids <- sapply(res_names, function(x) readRDS(x)["dKids_all"])
      
        names(dKids) <- rep("desired", length(dKids))
        
        sim_dkids_0 <- lapply(dKids, as.matrix)
        sim_dkids <- lapply(sim_dkids_0,
                            function(x){x <- as.matrix(x[1:length(iniY:(endY-1)),]);
                            colnames(x) <- colnames(obs$obs_desired);
                            rownames(x) <- iniY:(endY-1);
                            return(as.matrix(x))})
        
        if(all_sim){
          
          sim_desired <- sim_dkids
          
        }else{
          sim_desired <- lapply(sim_dkids,
                                function(x){x <- x[rownames(x) %in% rownames(obs$obs_desired),
                                                   colnames(x) %in% colnames(obs$obs_desired)];
                                x <- as.matrix(x);
                                colnames(x) <- colnames(obs$obs_desired);
                                return(x)})
        }
        
        sim_data[["sim_desired"]] <- sim_desired
    }
    # ccf by education
    if(ccf_edu){

      ccf_edu <- list()
      sim_ccf_edu <- list()
      sim_cohort_edu <- list()
      for(i in 1:3){
      ccf_edu[[i]] <- sapply(res_names, function(x) readRDS(x)[paste0("cohort",i)])
      
      names(ccf_edu[[i]]) <- rep(paste0("ccf_edu",i), length(ccf_edu[[i]]))
      
      ccf_edu_0 <- lapply(ccf_edu[[i]], as.matrix)
      sim_cohort_edu[[i]] <- lapply(ccf_edu_0,
                          function(x){j <- as.matrix(x[1:length(iniY:(endY-51)),2]);
                          colnames(j) <- paste0("ccf_edu",i);
                          rownames(j) <- x[,1];
                          return(as.matrix(j))})
      }
      sim_cohort_edu_r <- list()
      for(i in 1:length(sim_cohort_edu[[1]])){
      sim_cohort_edu_r[[i]] <- cbind(sim_cohort_edu[[1]][[i]],
                                  sim_cohort_edu[[2]][[i]],
                                  sim_cohort_edu[[3]][[i]])
      }
      if(all_sim){
        
        sim_ccf_edu <- sim_cohort_edu_r
        
      }else{
        sim_ccf_edu <- lapply(sim_cohort_edu_r,
                              function(x){x <- x[rownames(x) %in% rownames(obs$obs_ccf_edu),
                                                 colnames(x) %in% colnames(obs$obs_ccf_edu)];
                              x <- as.matrix(x);
                              colnames(x) <- colnames(obs$obs_ccf_edu);
                              x[is.na(x)] <- 0;
                              return(x)})
      }
      sim_data[["sim_ccf_edu"]] <- sim_ccf_edu
    }
    
    # results
    return(sim_data)
  }
  
  if (length(res_dir)>1){
    nsim_sim_results <- lapply(res_dir, get_sim_results, iniY, endY, obs)
  }else{
    nsim_sim_results <- get_sim_results(res_dir, iniY, endY, obs)
  }
  
  return(nsim_sim_results)
  
}
