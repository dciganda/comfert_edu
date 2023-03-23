compute_fake_obs <-function(param, fixed_param, pop, ini_c, iniY, endY){
  
  fake_obs <- parallel_comfert(param = param,
                               fixed_param = fixed_param,
                               pop = pop,
                               ini_c = ini_c,
                               iniY = iniY,
                               endY = endY)
  
  mean_fake_obs <- as.data.frame(Reduce("+", fake_obs) / length(fake_obs))
  
  dir_o <- file.path("..","data", pop, "out")
  
  write.csv(fake_obs, file.path(dir_o,"fake_asfrs.csv"), row.names = FALSE)
}
