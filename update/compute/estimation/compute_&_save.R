# Parallelization of repetitions
cat("Launching cluster\n")

cl <- makeCluster(nsim, type = "PSOCK")

# Load libraries
clusterEvalQ(cl, library(lubridate))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(truncdist))

# Source country files
path_to = function(file){
  file.path("..","data",country,"in", file)}

# fixed parameters
fix_p <- readRDS("fix_p.rds")


cat("Exporting objects\n")

clusterExport(cl,"path_to", envir = environment())
clusterExport(cl,"fix_p", envir = environment())
clusterExport(cl,"country", envir = environment())
clusterExport(cl,"ini_c", envir = environment())
clusterExport(cl,"iniY", envir = environment())
clusterExport(cl,"endY", envir = environment())

cat("Sourcing on slaves\n")

clusterCall(cl, function() {
  source(file.path("..","run","data_&_funs.R")) # load some functions
  source(file.path("..","run","comfert.R")) # load function running the simulation
})

cat("running comfert.. \n")

s <- Sys.time()
output <- parLapply(cl, 1:nsim, "comfert", param_ls, cluster = F) 
e <- Sys.time()
print(e-s)
cat("finished parallel processes \n")
stopCluster(cl)

cat(".....saving results....")
invisible(lapply(1:nsim, function(x) saveRDS(output[[x]],
                                             file = file.path(resultsPath, paste0("full_results", x,".RData")))))
rm(output)
