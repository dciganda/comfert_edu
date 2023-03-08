get_new_points <- function(priors, n){

locations <- priors[1, ]
multipliers <- priors[2, ] - locations

lhs_sample <- as.data.frame(lhs::improvedLHS(n, ncol(priors)))

mapped_sample <- mapply(function(x, multiplier, location) (x*multiplier) + location,
                      lhs_sample, multipliers, locations)

if(is.vector(mapped_sample)){
  mapped_sample <- matrix(mapped_sample, ncol = length(mapped_sample))
  }

final_sample <- data.frame(mapped_sample)

colnames(final_sample) <- colnames(priors)

return(final_sample)

}

