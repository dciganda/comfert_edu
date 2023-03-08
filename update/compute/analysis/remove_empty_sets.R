remove_empty_sets <- function(res_dir){
  
param_dir <- dir(file.path(res_dir, "results"), pattern = "param_set", full.names = T)
order_res <- sapply(X = strsplit(param_dir, split = "param_set_"), 
                    FUN = function(x){rev(x)[1]})
res_dirs <- param_dir[match(1:length(param_dir), order_res)]
res_list <- lapply(res_dirs, function(x) {list.files(x, "RData", full.names = TRUE)})
empty <- which(sapply(res_list, length)!= nsim)

if(length(empty)>0){
  
  cat("Empty results: \n", empty, '\n', "Deleting..")
  
}

empty_res_dir <- file.path(res_dir, "results", paste0("param_set_", empty))
invisible(sapply(empty_res_dir, unlink, recursive = T))

}