# The first three arguments determine where the sample will be saved
# country sample is asked for
country <- commandArgs(TRUE)[1]
# number of populations
nsim <- as.numeric(commandArgs(TRUE)[2])
# size of initial populations
ini_c <- as.numeric(commandArgs(TRUE)[3])
# number of parameters' combinations that must be generated
n0 <- as.numeric(commandArgs(TRUE)[4])
# weights for the estimation
weights <- commandArgs(TRUE)[5]

# Priors
priors <- readRDS("priors.rds")

get_param_set <- function(priors, country, n){
  
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
  
  final_sample$modName <- "regular"
  
  return(final_sample)
  
}

params <- get_param_set(priors, country, n=n0)

#############################################################################################
#################################### Saving to csv files ####################################

global_path <- file.path("..","results",
                         country,
                         paste0("n_sim_",nsim),
                         paste0("ini_c_",ini_c),
                         weights)

# create dirs
dirs <- list("param_sample", "new_evaluations", "post", "params")
sapply(dirs, function(x){if(!dir.exists(file.path(global_path, x))){dir.create(file.path(global_path, x))}})

posterior <- as.data.frame(matrix(ncol = ncol(params)+1))
names(posterior) <- c(names(params), "mse")

# save
write.csv(priors, file.path(global_path, dirs[[1]], "priors.csv"), row.names=F)
write.csv(params, file.path(global_path, dirs[[1]], "params.csv"), row.names=F)
write.csv(posterior, file.path(global_path, dirs[[3]], "posterior.csv"), row.names = F)

#############################################################################################

