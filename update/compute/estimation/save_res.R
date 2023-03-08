save_res <- function(results, pars, seq, res_path, delete = FALSE){
  
  if(delete){
    if(dir.exists(res_path)){unlink(res_path, recursive = T)} # check if results folder exists!
  }
  
p_set <- function(x){file.path(res_path, paste0("param_set_",x))}
res <- function(x){paste0("full_results", ((x-1) %% nsim)+1,".RData")}
dir_names <- rep(lapply(seq, p_set), each = nsim)
res_names <- rep(lapply(seq, res), nsim)
full_names <- lapply(1:length(dir_names), function(x) file.path(dir_names[x], res_names[x]))
invisible(lapply(unique(dir_names), dir.create, recursive = T, showWarnings = F))
invisible(lapply(1:length(full_names), function(x) saveRDS(results[[x]], full_names[[x]])))

for (i in 1:length(unique(dir_names))){
  cat('\nResults obtained with combination of parameters : \n',
    file = file.path(unique(dir_names)[i], 'Info.txt'))
  
    for(I in 1:ncol(pars)){
      
      cat(names(pars)[I], " : ", pars[i, I], "\n",
          file = file.path(unique(dir_names)[i], 'Info.txt'),
          append=T)
  }
  
}

}





