# PACKAGES ####
p <- c("parallel", "mlegpFULL","data.table", "splines", "ggplot2", "survival") 
invisible(lapply(p, library, character.only = TRUE))

# GET RESULTS ####
id <- list.files(file.path("..", "local", "results"))[15]
global_path <- file.path("..", "local", "results", id)
res_path <- file.path(global_path,"results")

log <- read.csv(file.path("..", "log", "log.csv"))
hp <- log[which(log$id==id),]
  
n0 <- hp$n0[1]
pop <- hp$pop[1]
iniY <- hp$iniY[1]
endY <- hp$endY[1]
nsim <- hp$nsim[1]

# ANALYSIS ####
source(file.path("..","analysis","check_paramset.R"))
source(file.path("..","analysis","plot_out.R"))


# Get posterior distribution
post <- readRDS(file.path(global_path, "post", "posterior.rds"))[-(1:(n0)),]
post[order(post$mse),]
opt_res_dir <- check_paramset(res_dir = global_path, rank = 1)

# Plot outcomes
plot_out(global_path = global_path,
         res_path = opt_res_dir,
         post_dat = post,
         pop = pop,
         iniY = iniY,
         endY = endY,
         nsim = nsim,
         weights = weights,
         unplanned = F,
         unwanted = F,
         desired = T,
         tfr = T,
         ccf = T,
         mab = T,
         ccf_edu_obs = F,
         ccf_compare = F,
         css = F,
         asfr = F,
         gap = F,
         gap_edu = F,
         colour = T, 
         interval = T,
         alpha_int = 0.05,
         last_obs_year = 2019,
         save = F)


# SENSITIVITY ANALYSIS ####
gp <- readRDS(file.path(global_path, "gp", "gp.rds"))

FANOVADecomposition.gp(gp, verbose = F, Interaction = F)

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

































