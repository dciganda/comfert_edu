check_paramset <- function(res_dir, rank){
select_mse <- post[order(post$mse),]$mse[rank] 
param_dir <- dir(file.path(res_dir, "results"), pattern = "param_set", full.names = T)
order_res <- sapply(X = strsplit(param_dir, split = "param_set_"), 
                    FUN = function(x){rev(x)[1]})
res_dirs <- param_dir[match(1:max(as.numeric(order_res)), order_res)]
paramset <- n0 + which(post$mse == select_mse) # results dir for min mse  
opt_res_dir <- res_dirs[!is.na(res_dirs)][paramset]
real_params <- read.csv(file.path(opt_res_dir, "Info.txt"), sep = ":")

optim_param <- post[order(post$mse),][rank,!names(post) %in% c("modName", "mse")]  
real_param <- as.numeric(real_params[!real_params[,1] %in% "modName  ",2])
op <- trunc(as.numeric(as.vector(optim_param[1,]))*10^2)/10^2
rp <- trunc(real_param*10^2)/10^2

iden <- sapply(1:length(op), function(x) identical(op[x], rp[x]))

if(length(iden)<length(op)*0.65){stop("not the same parameters")}else{"alles klar, alles gut"
  return(opt_res_dir) 
  } 

}