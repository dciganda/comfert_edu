# log creation functions
generate_id <- function(){
  current_datetime <- Sys.time()
  #current_datetime
  datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
  
  id <- gsub("-","",current_datetime)
  id <- gsub(" ","_",id)
  id <- gsub(":","",id)
  
  date <- datetime_splitted[[1]][1]
  time <- datetime_splitted[[1]][2]
  id_list <- list("id" = id, "date" = date, "time" = time)
  return(id_list)
}
create_sim_params <- function(){
  sim_names <- c("pop", "iniY", "endY", "ini_c", "n0", "nsim", "ne", "iter", "N")
  sim_values <- c(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)
  sim_params <- data.frame(sim_names, sim_values)
  
  return(sim_params)
}
create_empty_log <- function(log_path){
  
  column_names <- c("date", "time", "id","pop", "iniY", "endY",
                    "ini_c", "n0", "nsim", "ne", "iter", "N")
  
  df_log <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  
  colnames(df_log) <- column_names
  
  write.csv(df_log, log_path, row.names = FALSE)
  
}
save_log <- function(){
  
  log_path <- file.path("..", "log", "log.csv")
  
  if(!file.exists(log_path)){
    create_empty_log(log_path)  
  }
  
  id_list <- generate_id()
  sim_params <- create_sim_params()
  
  log <- read.csv(log_path, header = TRUE)
  new_pos <- nrow(log)+1
  
  id <- id_list$id
  date_id <- id_list$date
  time_id <-  id_list$time
  
  log[new_pos,]$id <- id

  log[new_pos,]$date <- date_id
  log[new_pos,]$time <- time_id
  
  log[new_pos,]$pop <- sim_params[1,2]
  log[new_pos,]$iniY <- sim_params[2,2]
  log[new_pos,]$endY <- sim_params[3,2]
  log[new_pos,]$ini_c <- sim_params[4,2]
  log[new_pos,]$n0 <- sim_params[5,2]
  log[new_pos,]$nsim <- sim_params[6,2]
  log[new_pos,]$ne <- sim_params[7,2]
  log[new_pos,]$iter <- sim_params[8,2]
  log[new_pos,]$N <- sim_params[9,2]
  
  write.csv(log, log_path, row.names = FALSE)
  
}



