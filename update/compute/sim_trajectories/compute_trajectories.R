library(lubridate); library(truncdist); library(data.table)

compute_trajectories <- function(param, ini_c){

source(file.path("..","run","comfert_trajectories.R"))
source(file.path("..","run","data_&_funs.R"))

t_out <- comfert_trajectories(seed_val = 3, param = param, ini_c = ini_c)

t_out$simDat$yb <- year(as.POSIXlt(t_out$simDat$tob, origin="1970-01-01"))

return(t_out)

}