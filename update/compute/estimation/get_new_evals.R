country <- commandArgs(TRUE)[1]
nsim <- as.numeric(commandArgs(TRUE)[2])
ini_c <- as.numeric(commandArgs(TRUE)[3])
weights <- commandArgs(TRUE)[4]
iniY <- as.numeric(commandArgs(TRUE)[5])
endY <- as.numeric(commandArgs(TRUE)[6])
n0 <- as.numeric(commandArgs(TRUE)[7])
ne <- as.numeric(commandArgs(TRUE)[8])
nt <- as.numeric(commandArgs(TRUE)[9])

# set global path
global_path <- file.path("..","results", paste(country),
                         paste0("n_sim_", nsim),
                         paste0("ini_c_", ini_c), weights)

# read files
priors <- read.csv(file.path(global_path,"priors","priors.csv"))
posterior <- read.csv(file.path(global_path,"post","posterior.csv"))

source("get_new_points.R")

if(nt == n0){
  cat("\n first iteration... \n")
  cat("\n computing mse of intial sample... \n")
  
  source("get_obs.R")
  source("get_sim.R")
  source("compute_mse.R") 

  # combinations
  ini_combs <- read.csv(file.path(global_path,"ini_param_sample","params.csv"))
  
  res_dir <- file.path(global_path, "results")
  
  # main dir
  param_dir <- dir(res_dir, pattern = "param_set", full.names = T)
  
  # reorder results in numerical order (1-nrpops)
  order_res <- sapply(X = strsplit(param_dir, split = "param_set_"), 
                      FUN = function(x){rev(x)[1]})
  res_dirs <- param_dir[match(1:length(param_dir), order_res)]
  
  res_list <- lapply(res_dirs, function(x) {list.files(x, "RData", full.names = TRUE)})
  empty <- which(sapply(res_list, length)!= nsim)
  
  if(length(empty)>0){
    cat(paste("No results for combination(s)"), as.vector(empty), 
        "\n removing from list..")
    res_dirs <- res_dirs[-as.numeric(empty)]
    
    if(length(res_dirs)!=nrow(ini_combs)){
      
      ini_combs <- ini_combs[-as.numeric(empty),]
      
    }
  } 
  
  obs <- get_obs(country, ysd = 1960) # observed data
  
  sim <- get_sim(res_dirs, iniY, endY, nsim, obs,  
                 unplanned = length(obs$obs_unplanned)>0, 
                 unwanted = length(obs$obs_unwanted)>0,
                 desired = length(obs$obs_desired)>0,
                 ccf = length(obs$obs_ccf)>0,
                 ccf_edu = length(obs$obs_ccf_edu)>0) # simulated data
  
  weights_aux <- as.numeric(sapply(strsplit(weights, "_"), function(x) x[1:length(x)]))
  weights_num <- c(asfr = weights_aux[1], unplanned = weights_aux[2],
                   unwanted = weights_aux[3], desired = weights_aux[4],
                   ccf = weights_aux[5], ccf_edu = weights_aux[6])
  
  # compute mse
  ini_combs$mse <- sapply(sim, compute_mse, obs, weights_num)  
  
  cat("\n done! \n")
  
  posterior <- rbind(posterior, ini_combs)
  
  cat("\n saving results in 'posterior' object... \n")
  
  write.csv(posterior[-1,], file.path(global_path,"post","posterior.csv"), row.names = F)
  
}else{cat("\n starting new iteration... \n")}

posterior <- read.csv(file.path(global_path,"post","posterior.csv"))

# training gp
cat("\n training gp...\n")
gpe <- mlegp::mlegp(posterior[!is.na(posterior$mse),!names(posterior) == "mse"], 
                    posterior[!is.na(posterior$mse), "mse"], 
                    nugget = T, verbose = 0)

cat("\n geting new points to evaluate...\n")
new_set <- get_new_points(priors, n = n0)

cat("\n getting predictions... \n")
pred <- mlegp::predict.gp(gpe, newData = new_set, se.fit = T)

mu <- pred$fit
sigma <- pred$se.fit

kappa_af <- 2

lcb <- mu - kappa_af * sigma

if(nrow(new_set[which(lcb %in% sort(lcb)[1:ne]),]) > ne){
  cat("\n warning: multiple identical predictions")
  cat("\n printing prediction object: \n")
  
  print(lcb)
}

cat("\n getting new evaluations... \n")
new_evals <- new_set[which(lcb %in% sort(lcb)[1:ne]),][1:ne,]

saveRDS(new_evals, file.path(global_path,"new_evaluations","new_evals.rds"))


cat("\n computing new evaluations... \n")
