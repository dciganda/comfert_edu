intervals_function <- function(opt_res_dir, post_dat, element, year_asfr = 0, ysd = 0,
                               col = 0, iniY, endY,
                               eps = 0.1, alpha_int){
  
  require(dplyr) 
  
  # Inputs 
  
  # opt_res_dir   opt_res_dir will have the path to the combination of parameters with lower mse to observed data
  #               this input is set to match the one given in plot_out
  #               due to the structure of the files, it is necessary to leave this path to look for the params_sets
  
  # post_dat      dataframe with the parameters to choose those with lower mse
  
  # element       which element of the full_results (list) is to be used. default: "cohort"
  
  # year_asfr     what year of asfr is being drawn
  
  # col           some elements are data.frame and others arrays, in case of data.frame this input establishes from which 
  #               column to extract the data. default: 0 (array)
  
  # iniY          initial year of the estimation
  
  # endY          final year of the estimation
  
  # eps           to define the outputs accepted according to the lowest mse. default: 0.1
  
  # alpha_int     significance level to define the intervals. default = 0.05
  
  
  #----------------------------------------------------------------------------------
  
  # keep eps% of values with lowest distance 
  epsilon <- quantile(post_dat$mse,probs = seq(0, 1, eps))[2]
  accepted <- post_dat[post_dat$mse < epsilon,]
  
  
  
  if(element=="asfr"){
    
    obs_df_asfr <- data.frame(matrix(NA,ncol = 35)) # create a data frame for the observed data (35 years between 15-50)
    sim_df_asfr <- data.frame(matrix(NA,ncol = 35)) # create a data frame for the simulated data (35 years between 15-50)
    obs_set <- get_obs(country, ysd) #observed values are taken from ysd
    
    for (i in 1:nrow(accepted)){
      
      ruta <- check_paramset(res_dir = dirname(dirname(opt_res_dir)), rank = i)
      
      aux_obs <- obs_set
      
      aux_sim <- get_sim(ruta, iniY, endY, nsim = 2,
                         aux_obs, asfr = T,  all_sim = F) # to extract the 2 full_results of the corresponding paramset folder
      
      colnames(obs_df_asfr) <- colnames(aux_obs$obs_asfr)
      colnames(sim_df_asfr) <- colnames(aux_sim$sim_asfr$asfr)
      
      obs_df_asfr <- rbind(obs_df_asfr,aux_obs$obs_asfr)
      
      sim_df_asfr <- rbind(sim_df_asfr,aux_sim[[1]][1]$asfr,aux_sim[[1]][2]$asfr ) 
      
      
    }
    
    sim_df_asfr$year <- substr(rownames(sim_df_asfr),1,4) # to filter by years and calculate intervals for each year
    
    # this is the df with years as rownames and age 15:49 as columns
    sim_df_asfr <- sim_df_asfr[-1,] # the first row is NA because of how the df is constructed
    
    aux_asfr <- sim_df_asfr %>% filter(year == year_asfr) # to extract a particular year
    
    apply(X = aux_asfr[,-ncol(aux_asfr)], MARGIN = 2, function(y, alpha = alpha_int){
      
      mediana <- quantile(y, 0.5, na.rm = T)
      
      int_inf <- quantile(y, alpha/2, na.rm = T)
      
      int_sup <- quantile(y, 1-alpha/2, na.rm = T)
      
      
      return(c(mediana,int_inf,int_sup))
    })
    
    
  } else {
    
    
    # credible intervals
    
    years <- seq(iniY,endY) %>% data.table() 
    colnames(years) <- "year"
    
    
    for (i in 1:nrow(accepted)){
      
      # to check which folder has each accepted combination
      ruta <- check_paramset(res_dir = dirname(dirname(opt_res_dir)), rank = i)
      
      # res_names are combination that generate de eps of data with less mse
      res_names <- sapply(ruta, function(x) {list.files(x, "RData", full.names = TRUE)}) 
      
      sim <- sapply(res_names, function(x) readRDS(x)[element]) 
      
      # if the list item is not an array, the column from which to extract the data must be specified
      if(col != 0){
        
        sim <- lapply(sim, function(x) x[,col])
        
      }
      
      years <- cbind(years, sim[[1]], sim[[2]])
      
    }
    
    # function to find the intervals
    quantiles <- apply(years[,-1], 1, function(y, alpha = alpha_int){
      
      mediana <- quantile(y, 0.5, na.rm = T)
      
      int_inf <- quantile(y, alpha/2, na.rm = T)
      
      int_sup <- quantile(y, 1-alpha/2, na.rm = T)
      
      
      return(c(mediana,int_inf,int_sup))
    }) 
    
    # sorted data
    datos <- data.table(years = years[,1], mediana = quantiles[1,],
                        int_inf = quantiles[2,], int_sup = quantiles[3,])
    
    return(datos)
    
  }
  
}
