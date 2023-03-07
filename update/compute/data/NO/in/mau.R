c_d <- 1947 # last cohort for which mean age at union decreases
ini_mau <- 25 # initial value of the mean age at union formation
end_mau <- 20 # end value of the mean age at union 
uprob <- c(seq(0.24, 0.1, length.out = 1940 - iniY),
           seq(0.1, 0.15, length.out = (endY + 1) - 1940)) # union probability
mau_trend <- seq(ini_mau, end_mau, length.out = c_d-iniY)