save_res <- function(res){
if(dir.exists(res_dir)){unlink(res_dir, recursive = T)} # check if results folder exists!
p_set <- function(x){file.path(res_dir, paste0("param_set_", ((x-1) %/% nsim)+1))}
res <- function(x){paste0("full_results",((x-1) %% nsim)+1,".RData")}
dir_names <- lapply(1:(nsim*n0), p_set)
res_names <- lapply(1:(nsim*n0), res)
full_names <- lapply(1:(nsim*n0), function(x) file.path(dir_names[x], res_names[x]))
invisible(lapply(unique(dir_names), dir.create, recursive = T, showWarnings = F))
invisible(lapply(1:(nsim*n0), function(x) saveRDS(output[[x]], full_names[[x]])))
dir.create(file.path(res_dir, "post"), recursive = T)
dir.create(file.path(res_dir, "params"), recursive = T)
dir.create(file.path(res_dir, "sim_trajectories"), recursive = T)
}