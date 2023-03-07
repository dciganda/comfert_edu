check_paramset <- function(rank){
select_mse <- post[order(post$mse),]$mse[rank] 
param_dir <- dir(file.path(res_dir, "results"), pattern = "param_set", full.names = T)
order_res <- sapply(X = strsplit(param_dir, split = "param_set_"), 
                    FUN = function(x){rev(x)[1]})
res_dirs <- param_dir[match(1:length(param_dir), order_res)]
res_list <- lapply(res_dirs, function(x) {list.files(x, "RData", full.names = TRUE)})
empty <- which(sapply(res_list, length)!= nsim)
paramset <- which(post$mse == select_mse) # results dir for min mse
empty_below <- which(empty>n0 & empty<n0+paramset)
paramset <- paramset + length(empty_below)
opt_res_dir <- file.path(res_dir, "results", paste0("param_set_", n0+paramset))
real_params <- read.csv(file.path(opt_res_dir, "Info.txt"), sep = ":")

optim_param <- post[order(post$mse),][rank,!names(post) %in% c("modName", "mse")]  
real_param <- as.numeric(real_params[!real_params[,1] %in% "modName  ",2])
op <- sprintf("%.1f", as.numeric(as.vector(optim_param[1,])))
rp <- sprintf("%.1f",real_param)

if(!identical(op, rp)){stop("not the same parameters")}else{"alles klar, alles gut"
  return(opt_res_dir)
  }


}