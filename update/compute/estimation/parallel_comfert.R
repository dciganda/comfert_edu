
parallel_comfert <- function(param, hparam) {
  
  hparam_list <- names(hparam)
  for(i in hparam_list){assign(i, hparam[[i]], envir = globalenv())}

  path_to <- function(file){
    file.path("..","data",pop,"in", file)}
  
  o_file <- file.path("..","out_files",
                      paste0("parallel_comfert_",
                      pop,"_", Sys.Date(),".txt"))
  
  if (file.exists(o_file)){file.remove(o_file)} 
  
  cl <- makeCluster(nrow(param), type = "PSOCK",
                    outfile = paste0("pc_",unname(Sys.info()[4]),".txt"))
  
  clusterEvalQ(cl, library(lubridate))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(truncdist))
  
  clusterExport(cl,"pop")
  clusterExport(cl,"path_to", envir = environment())
  clusterExport(cl,"ini_c", envir = environment())
  clusterExport(cl,"param", envir = environment())
  clusterExport(cl,"iniY", envir = environment())
  clusterExport(cl,"endY", envir = environment())
  clusterExport(cl,"fix_p", envir = environment())

  clusterCall(cl, function() {
    source(file.path("..","run","data_&_funs.R")) # load functions
    source(file.path("..","run","comfert.R")) # load model
  })
  
  s <- Sys.time()
  

  output <- parLapply(cl, 1:nrow(param), function (x) comfert(seed_val = x,
                                                               param[x,],
                                                               parallel_run = T,
                                                               cluster = F))
  e <- Sys.time()
  print(e-s)
  
  cat("finished parallel processes \n")
  stopCluster(cl)
  
  return(output)

}