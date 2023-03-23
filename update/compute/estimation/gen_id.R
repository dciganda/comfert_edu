gen_id <- function(){
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