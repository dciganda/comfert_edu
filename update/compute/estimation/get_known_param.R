get_known_param <- function(priors, random = T, mid = F, given = F, given_param = c(0,0,0)){
  
  if(random){
    
  locations <- priors[1, ]
  multipliers <- priors[2, ] - locations  
  lhs_sample <- as.data.frame(improvedLHS(1, ncol(priors)))
  mapped_sample <- mapply(function(x, multiplier, location) (x*multiplier) + location,
                          lhs_sample, multipliers, locations)
  
  if(is.vector(mapped_sample)){
    mapped_sample <- matrix(mapped_sample, ncol = length(mapped_sample))
  }
  final_sample <- data.frame(mapped_sample)
  colnames(final_sample) <- colnames(priors)
  
  }
  
  if(mid){
    
  middle_grid_point <- (priors[1, ] + priors[2, ]) / 2
  final_sample <- data.frame(middle_grid_point)
  colnames(final_sample) <- colnames(priors)
  
  }
  
  if(given){
    
    final_sample <- data.frame(given_param)
    colnames(final_sample) <- colnames(priors)
    
  }
  
  return(final_sample)
  
  
}

