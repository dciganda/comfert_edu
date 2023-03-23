get_fake_obs <- function(pop){
  
  dir_o <- file.path("..","data", pop, "out")
  # Fake Observed ASFR
  fake_obs <-  read.csv(file.path(dir_o, "fake_asfrs.csv"), header = T) 

  return(fake_obs)  
}  



