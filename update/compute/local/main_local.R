library(parallel); library(data.table); library(mlegp); library(ggplot2);
library(splines); library(survival)  


start <- Sys.time()

source(file.path("..","estimation","get_new_points.R"))
source(file.path("..","estimation","parallel_comfert.R"))
source(file.path("..","estimation","optimize_comfert.R"))
source(file.path("..","estimation","save_res.R"))
source(file.path("..","analysis","plot_out.R"))
source(file.path("..","analysis","duration_models.R"))
source(file.path("..","analysis","kravdal_coeffs.R"))
source(file.path("..","analysis","data_prep.R"))

country <- "NO" # country on which simulations are performed 
iniY <- 1910 # years for which the simulations are performed
endY <- 2019 # years for which the simulations are performed
ini_c <- 50 # size of the initial birth cohorts of the model (affects computation times - 50 takes 10-15 minutes, but should be closer to 1000 for smooth results)
n0 <- 10 # size of initial sample of param combinations
nsim <- 2 # nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim
ne <- 10 # nr of new evaluations at each iteration of the bayes opt. algorithm
N <- 20 # total nr of evaluations n0+N

weights <- c(asfr = .5,
             unplanned = 0.00,
             unwanted = 0.00,
             desired = .0,
             ccf_edu = .5) # weights for the computation of the MSE

# directory to store results
res_dir <- file.path("results", paste(country),
                     paste0("n_sim_", nsim),
                     paste0("ini_c_", ini_c),
                     "results", paste(weights, collapse = "_"))

priors <- data.frame(psi = c(1972, 1979),           # Year inflection point diffusion of contraception.
                     upsilon = c(0.15, 0.55),       # Maximum Risk Unplanned births
                     rho = c(0.025, 0.050),         # minimum risk of unplanned birth
                     r = c(0.15, 0.27),             # Speed of diffusion contraception
                     eta = c(0.3, 0.8),             # Max effect work
                     xi = c(3, 6),                  # years after end of education for family formation
                     D_0 = c(2.2, 2.6),             # initial value desired family size
                     delta = c(0.005, 0.1),          # delta D
                     tau = c(18, 30),               # effect of edu on intention 
                     epsilon = c(0.005, 0.15),         # rate effect education
                     alpha = c(0.03, 0.16)          # difference in contraceptive use by edu
)

# -- fixed parameters --
fix_p <- list(lambda = 2.5e-08,                     # rate decrease penalty intention after birth
              sd_lnrm = 0.16,                       # stdrd dev lognorm
              gamma = 38,                           # Fecundability age
              kappa = 0.25,                         # Fecundability rate
              A = 0.07,                             # reduction risk unplanned after achieve D
              phi = .22,                            # maximum fecundability
              theta = 0.1,                          # scale of truncated Gamma (D)
              end_mau = 20,
              ini_mau = 32,
              u = c(seq(0.22, 0.09,
                        length.out = 1940-iniY),   # union probability
                    seq(0.09, 0.16,
                        length.out = (endY+1)-1940))
)

params <- get_new_points(priors, n=n0) # Initial sample of parameter combinations  

# Compute model at initial parameter set 
output <- parallel_comfert(params = params[rep(seq_len(nrow(params)), each = nsim),],
                           country = country,
                           ini_c = ini_c,
                           iniY = iniY,
                           endY = endY) 

# Save Results
save_res(output)

# Bayesian optimization
optimize_comfert(res_dir, country, iniY, endY, ini_c, n0, nsim,
                 N, ne, params, priors, weights) 

end <- Sys.time()
print(start-end)

# Get posterior distribution
post <- read.csv(file.path(res_dir, "post", "posterior.csv"))[-1,] 
post[order(post$mse),]
opt_res_dir <- check_paramset(rank = 1)

# Plot outcomes
plot_out(opt_res_dir, country, iniY, endY, nsim, asfr = T, colour = T, save = F)

# compute trajectories
source(file.path("..","sim_trajectories","compute_trajectories.R"))
ini_c <- 2500
param <- post[order(post$mse),!names(post)%in%c("mse", "modName")][1,]
path_to <- function(file){
  file.path("..","data",country,"in", file)}

t_out <- compute_trajectories(param, ini_c)

st_dir <- file.path(res_dir,"sim_trajectories")
dir.create(st_dir, showWarnings = F, recursive = T)
saveRDS(t_out$simDat, file.path(st_dir, "sim_trajectories.rds"))


source(file.path("..","analysis","duration_models.R"))
source(file.path("..","analysis","kravdal_coeffs.R"))
source(file.path("..","analysis","data_prep.R"))
source(file.path("..","analysis","check_paramset.R"))

# read coefficients from Kradval and Rindfuss (2008).
kr_coeffs <- get_coeffs()
kr_coeffs$ind <- rep(1:4,each = 10)

# simulated data 
sim_dat <- readRDS(file.path(st_dir, "sim_trajectories.rds"))

# models second birth
scnd_m0 <- duration_models(data = sim_dat, parity_transition = "second", desired = F, activity = F, se = T)
scnd_m1 <- duration_models(data = sim_dat, parity_transition = "second", desired = F, activity = T, se = T)
scnd_m2 <- duration_models(data = sim_dat, parity_transition = "second", desired = T, activity = T, se = T)
models_scnd <- rbind(scnd_m0, scnd_m1, scnd_m2)
models_scnd$model <- c(rep("m0",nrow(scnd_m0)),rep("m1",nrow(scnd_m1)),rep("m2",nrow(scnd_m2))) 
# models third birth
third_m0 <- duration_models(data = sim_dat, parity_transition = "third", desired = F, activity = F, se = T)
third_m1 <- duration_models(data = sim_dat, parity_transition = "third", desired = F, activity = T, se = T)
third_m2 <- duration_models(data = sim_dat, parity_transition = "third", desired = T, activity = T, se = T)
models_third <- rbind(third_m0, third_m1, third_m2)
models_third$model <- c(rep("m0",nrow(third_m0)),rep("m1",nrow(third_m1)),rep("m2",nrow(third_m2)))

# Plot
source(file.path("..","analysis","plot_models_se.R"))

plot_obs(models_scnd, exclude_sub_level = "none", level = "SEC",
         x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_obs(models_scnd, exclude_sub_level = "none", level = "TER",
         x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_obs_sim(models_scnd, exclude_sub_level = "none", level = "SEC",parity = "second",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_obs_sim(models_scnd, exclude_sub_level = "none", level = "TER", parity = "second",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_obs_sim(models_third, exclude_sub_level = "none", level = "SEC",parity = "third",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_obs_sim(models_third, exclude_sub_level = "none", level = "TER",parity = "third",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)


plot_sim_models(models_scnd, level = "SEC", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_sim_models(models_scnd, level = "TER", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_sim_models(models_third, level = "SEC", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

plot_sim_models(models_third, level = "TER", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = T)

































