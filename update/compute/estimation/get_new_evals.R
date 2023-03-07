country <- commandArgs(TRUE)[1]
nsim <- as.numeric(commandArgs(TRUE)[2])
ini_c <- as.numeric(commandArgs(TRUE)[3])
weights <- commandArgs(TRUE)[4]
iniY <- as.numeric(commandArgs(TRUE)[5])
endY <- as.numeric(commandArgs(TRUE)[6])
n0 <- as.numeric(commandArgs(TRUE)[7])
ne <- as.numeric(commandArgs(TRUE)[8])
nt <- as.numeric(commandArgs(TRUE)[9])


if(nt == n0){cat("\n first iteration... \n")}else{cat("\n starting new iteration... \n")}

global_path <- file.path("..","results", paste(country),
                       paste0("n_sim_", nsim),
                       paste0("ini_c_", ini_c), weights)

params <- read.csv(file.path(global_path,"param_sample","params.csv"))
priors <- read.csv(file.path(global_path,"param_sample","priors.csv"))

res_dir <- file.path(global_path, "results")

source("get_obs.R")
source("get_sim.R")
source("compute_mse.R")
source("get_new_points.R")

# main dir
param_dir <- dir(res_dir, pattern = "param_set", full.names = T)

# combinations
em_data <- params[,!names(params) == "modName"]

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
  
  if(length(res_dirs)!=nrow(em_data)){
    
  em_data <- em_data[-as.numeric(empty),]
  
  }
} 

obs <- get_obs(country, ysd = 1960) # observed data

sim <- get_sim(res_dirs, iniY, endY, nsim, obs,  
               unplanned = length(obs$obs_unplanned)>0, 
               unwanted = length(obs$obs_unwanted)>0,
               desired = length(obs$obs_desired)>0,
               ccf_edu = length(obs$obs_ccf_edu)>0) # simulated data

weights_aux <- as.numeric(sapply(strsplit(weights, "_"), function(x) x[1:length(x)]))
weights_num <- c(asfr = weights_aux[1], unplanned = weights_aux[2],
                 unwanted = weights_aux[3], desired = weights_aux[4],
                 ccf_edu = weights_aux[5])

# compute mse
em_data$mse <- sapply(sim, compute_mse, obs, weights_num)  

# training gp
cat("\n training gp...\n")
gpe <- mlegp::mlegp(em_data[!is.na(em_data$mse),!names(em_data) == "mse"], 
                    em_data[!is.na(em_data$mse), "mse"], 
                    nugget = T, verbose = 0)

gp <- list(em_data, gpe)

cat("\n geting new points to evaluate...\n")
new_set <- get_new_points(priors, n = n0)

cat("\n getting predictions... \n")
pred <- mlegp::predict.gp(gp[[2]], newData = new_set[!names(new_set) %in% "modName"], se.fit = T)

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
