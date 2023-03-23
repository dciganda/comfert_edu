log <- function(hparam, id){
  
  log_path <- file.path("..", "log")
  
  if(!dir.exists(log_path)){
    
    dir.create(log_path, recursive = T) 
    log <- cbind(as.data.frame(id), as.data.frame(hparam))
    write.csv(log, file.path(log_path, "log.csv"), row.names = FALSE)
    
  }else{
    
    log <- read.csv(file.path(log_path, "log.csv"))
    log <- rbind(log, cbind(as.data.frame(id), as.data.frame(hparam)))
    write.csv(log, file.path(log_path, "log.csv"), row.names = FALSE)
    
  }
}



