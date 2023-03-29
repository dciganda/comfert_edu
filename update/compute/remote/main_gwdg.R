library(parallel); library(data.table); library(mlegp); library(ggplot2); library(ssh);
library(splines); library(survival); library(stargazer) 

# -- parameters of the experiment --
country <- "NO" # country on which simulations are performed 
iniY <- 1910 # years for which the simulations are performed
endY <- 2019 # years for which the simulations are performed
ini_c <- 2500 # size of the initial birth cohorts of the model (affects computation times - 50 takes 10-15 minutes, but should be closer to 1000 for smooth results)
n0 <- 200 # size of initial sample of param combinations
nsim <- 2 # nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim
ne <- 40 # nr of new evaluations at each iteration of the bayes opt. algorithm
N <- 400 # total nr of evaluations n0+N
partition <- "medium"  # computing partition in cluster
c_time <- "20:00:00"   # time requested for each model run in cluster  
weights <- c(asfr = .3,
            unplanned = 0,
            unwanted = 0,
            desired = 0,
            ccf = 0,
            ccf_edu = .7) # weights for the computation of the MSE
ml <- "module load r/4.0.3"

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

saveRDS(priors, file.path("..","estimation","priors.rds")) # save priors

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
                        length.out = 1938-iniY),   # single probability:
                    seq(0.09, 0.16,
                        length.out = (endY+1)-1938))
)

saveRDS(fix_p, file.path("..","estimation","fix_p.rds")) # save fixed params

# -- write files --
source(file.path("..","estimation","write_sh_files.R")) # write sh files

# -- cluster authentication --
user <- "daniel.ciganda@gwdu19.gwdg.de" 
key <- file.path("..","..","..","..","..","gwdg_key","gwdg_key")
pswd <- "Rostock1418." 

# -- connecting to cluster -- 
session <- ssh_connect(host = user, passwd = pswd, keyfile = key)
cluster_path <- "U:/cloud/work/comfert/comfert_edu/update/compute/"
run_dir <- paste0(cluster_path,"run/")
data_dir <- paste0(cluster_path,"data/")
estimation_dir <- paste0(cluster_path,"estimation/")
out_files_dir <- paste0(cluster_path,"out_files/")

# -- Upload files to run the model in cluster --
gwdg_path <- file.path("/home","mpg08","daniel.ciganda","comfert")
scp_upload(session, file.path(run_dir), gwdg_path)
scp_upload(session, file.path(data_dir), gwdg_path)
scp_upload(session, file.path(estimation_dir), gwdg_path)
scp_upload(session, file.path(out_files_dir), gwdg_path)

# -- EXTERNAL: Run computations in cluster --

# -- create directory to store results --
res_dir <- file.path("results", paste(country),
                     paste0("n_sim_", nsim),
                     paste0("ini_c_", ini_c),
                     paste(weights, collapse = "_"))
dir.create(res_dir, showWarnings = F, recursive = T)

# -- download results from cluster --
scp_download(session, file.path("/home", "mpg08","daniel.ciganda",
                                "comfert","results",
                                country,
                                paste0("n_sim_",nsim),
                                paste0("ini_c_",ini_c),
                                paste(weights, collapse = "_"),"*"),
             file.path(getwd(), res_dir))
# ANALYSIS
source(file.path("..","analysis","check_paramset.R"))
source(file.path("..","analysis","plot_out.R"))

# -- get posterior --
post <- read.csv(file.path(res_dir, "post", "posterior.csv"))[-1,] 
post[order(post$mse),]
opt_res_dir <- check_paramset(rank = 1)

# Plot outcomes
plot_out(opt_res_dir, country, iniY, endY, nsim, asfr = T, colour = T, save = T)

plot_out(opt_res_dir, country, iniY, endY, nsim, asfr = T, tfr = F,
         ccf = F, ccf_match = F, ccf_edu = F,
         ccf_edu_obs = F, ccf_edu_obs_match = T, mab = F, mabs = F, desired = F,
         gap = F, gap_edu = F, css = F, ysd = 1960, colour = T, save = F)

plot_out(opt_res_dir, country, iniY, endY, nsim, asfr = F, tfr = F,
         ccf = F, ccf_match = F, ccf_edu = F,
         ccf_edu_obs = F, ccf_edu_obs_match = F, mab = F, mabs = F, desired = T,
         gap = F, gap_edu = F, css = F, ysd = 1960, colour = T, save = T)

# compute trajectories
source(file.path("..","sim_trajectories","compute_trajectories.R"))
ini_c <- 3000
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
source(file.path("..","analysis","plot_models.R"))

plot_obs(models_scnd, exclude_sub_level = "none", level = "SEC",
         x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_obs(models_scnd, exclude_sub_level = "none", level = "TER",
         x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_obs_sim(models_scnd, exclude_sub_level = "none", level = "SEC",parity = "second",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_obs_sim(models_scnd, exclude_sub_level = "none", level = "TER", parity = "second",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_obs_sim(models_third, exclude_sub_level = "none", level = "SEC",parity = "third",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_obs_sim(models_third, exclude_sub_level = "none", level = "TER",parity = "third",
             x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)


plot_sim_models(model = models_scnd, level = "SEC", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_sim_models(models_scnd, level = "TER", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_sim_models(models_third, level = "SEC", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)

plot_sim_models(models_third, level = "TER", x1 = 1940, x2 = 1960, y1 = 0.4, y2 = 1.6, save = F)



############################################
# # mean of secondary and tertiary
# mean_sec <- by(kr_coeffs$coef, list(kr_coeffs$ind,kr_coeffs$b_cohorts), mean)
# mean_sec_df <- as.data.frame(do.call("rbind", as.list(mean_sec)))
# mean_sec_df$b_cohorts <- as.factor(rep(dimnames(mean_sec)[[2]], each = 4))
# mean_sec_df$transition <- rep(c("second", "third"), each = 2)
# mean_sec_df$type <- "obs"
# names(mean_sec_df)[1] <- "coef"
# mean_sec_df$edu <- rep(c("secondary", "tertiary"))
# 
# kr_coeffs <- mean_sec_df[,c(1,5,2,4,3)]  
# 




