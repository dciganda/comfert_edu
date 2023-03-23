# PACKAGES ####
p <- c("parallel", "data.table", "mlegp", "splines", "ggplot2", "survival") 
invisible(lapply(p, library, character.only = TRUE))


# HYPERPARAMETERS ####
hparam <- list(pop = "NO",                 # reference population
               iniY = 1910,                # initial year of the simulations 
               endY = 2019,                # final year of the simulation
               ini_c = 1000,               # size of the initial birth cohort 
               n0 = 40,                    # size of initial sample of param combinations
               nsim = 2,                   # nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim
               ne = 20,                    # nr of new evaluations at each iteration of the bayes opt. algorithm
               iter = 8,                   # nr of iterations
               weights = c(asfr = .5,
                           unplanned = 0.0,
                           unwanted = 0.0,
                           desired = .0,
                           ccf = .0,
                           ccf_edu = .5)   # weights for the computation of the MSE
)
hparam[["N"]] <- hparam[["ne"]]*hparam[["iter"]]   # total nr of evaluations = n0+N

# PRIORS ####
priors <- data.frame(psi = c(1974, 1981),           # Year inflection point diffusion of contraception.
                     upsilon = c(0.2, 0.3),       # Maximum Risk Unplanned births
                     rho = c(0.015, 0.04),         # minimum risk of unplanned birth
                     r = c(0.1, 0.35),             # Speed of diffusion contraception
                     eta = c(0.5, 0.8),             # Max effect work
                     xi = c(3, 4),                  # years after end of education for family formation
                     D_0 = c(2.8, 3),             # initial value desired family size
                     delta = c(0.1, 0.16),         # delta D
                     tau = c(18, 30),               # effect of edu on intention 
                     epsilon = c(0.08, 0.18),      # rate effect education
                     alpha = c(0.06, 0.16)          # difference in contraceptive use by edu
)

# FIXED PARAMETERS ####
fix_p <- list(lambda = 2.5e-08,                     # rate decrease penalty intention after birth
              sd_lnrm = 0.16,                       # stdrd dev lognorm
              gamma = 38,                           # Fecundability age
              kappa = 0.25,                         # Fecundability rate
              A = 0.07,                             # reduction risk unplanned after achieve D
              phi = .22,                            # maximum fecundability
              theta = 0.1,                          # scale of truncated Gamma (D)
              delta_one = 1.05,
              end_mau = 19.7,
              ini_mau = 32,
              u = c(seq(0.22, 0.09,
                        length.out = 1938-hparam[["iniY"]]),   # single probability:
                    seq(0.09, 0.16,
                        length.out = (hparam[["endY"]]+1)-1938))
)

# PATHS ####
source(file.path("..","estimation","gen_id.R"))
id <- gen_id()
global_path <- file.path("results", id$id)
res_path <- file.path(global_path,"results")

# RUN BAYES ####
source(file.path("..","estimation","run_bayes.R"))

start <- Sys.time()
run_bayes(global_path = global_path,
          res_path = res_path,
          hparam = hparam,
          priors = priors,
          fix_p =fix_p,
          id = id,
          analysis = F)
end <- Sys.time()
print(end-start)

# ANALYSIS ####
source(file.path("..","analysis","check_paramset.R"))
source(file.path("..","analysis","plot_out.R"))

# Get posterior distribution
post <- readRDS(file.path(global_path, "post", "posterior.rds"))[-(1:(n0)),]
post[order(post$mse),]
opt_res_dir <- check_paramset(res_dir = global_path, rank = 2)

# Plot outcomes
plot_out(global_path = global_path,
         res_path = opt_res_dir,
         post_dat = post,
         pop = pop,
         iniY = iniY,
         endY = endY,
         nsim = nsim,
         weights = weights,
         unplanned = T,
         unwanted = F,
         desired = T,
         tfr = T,
         ccf = T,
         mab = T,
         ccf_edu_obs = T,
         ccf_compare = F,
         css = T,
         asfr = F,
         gap = F,
         gap_edu = F,
         colour = T, 
         interval = T,
         alpha_int = 0.05,
         last_obs_year = 2019,
         save = F)

# SCENARIOS ####
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

































