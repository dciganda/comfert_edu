get_obs <- function(country, ysd){
  
  dir_o <- file.path("..","data", country ,"out")
  # Observed ASFR
  obs_asfr_0 <- read.table(file.path(dir_o,"asfrs_hfd.txt"),
                           skip = 2, header=T, stringsAsFactors = F)
  obs_asfr_0[,2] <- ifelse(obs_asfr_0[,2] == "12-", "12", obs_asfr_0[,2]) 
  obs_asfr_0[,2] <- ifelse(obs_asfr_0[,2] == "55+", "55", obs_asfr_0[,2]) 
  obs_asfr_0[,2] <- as.numeric(obs_asfr_0[,2])
  obs_asfr <- obs_asfr_0[obs_asfr_0$Year >= ysd, c(1,2,3)] # 
  obs_asfr <- reshape(obs_asfr, idvar = "Year" , timevar = "Age", direction="wide")
  obs_asfr <- as.matrix(obs_asfr[,-c(1:4,40:45)])
  colnames(obs_asfr) <- paste0("age", 15:49)
  rownames(obs_asfr) <- max(min(obs_asfr_0$Year),ysd):max(obs_asfr_0$Year)
  
  obs_data <- list(obs_asfr = obs_asfr) 
  
  # Observed Unplanned Births
  file_unp <- file.path(dir_o, "unplanned.csv")
  if(file.exists(file_unp)){
  obs_unp_0 <- read.table(file_unp, sep = ",", skip = 3, header = T,
                          stringsAsFactors = F)
  obs_unp_1 <- as.matrix(obs_unp_0[,2])  
  rownames(obs_unp_1) <- obs_unp_0$year
  obs_unplanned <- as.matrix(obs_unp_1[!is.na(obs_unp_1),])
  colnames(obs_unplanned) <- "proportion.unplanned"
  obs_data <- c(obs_data, list(obs_unplanned = obs_unplanned))
  }

  # Observed Unwanted Births
  file_unw <- file.path(dir_o, "unwanted.csv")
  if (file.exists(file_unw)){
    obs_unw_0 <- read.table(file_unw, 
                            sep = ",", skip = 3, header = T,
                            stringsAsFactors = F)
    obs_unw_1 <- as.matrix(obs_unw_0[,2]/100)  
    rownames(obs_unw_1) <- obs_unw_0$year
    obs_unwanted <- as.matrix(obs_unw_1[!is.na(obs_unw_1),])
    colnames(obs_unwanted) <- "prop.unwanted"
    obs_data <- c(obs_data, list(obs_unwanted = obs_unwanted))
  }
  
  # Observed D
  file_ideal <- file.path(dir_o, "ideal.csv")
  if (file.exists(file_ideal)){
    if(country == "FR"){
      obs_1 <- read.table(file_ideal, sep = ",", skip = 0, header = T)
      obs_1 <- obs_1[!is.na(obs_1[,5]),c(2,5)]
    }else{
      obs_1 <- read.table(file_ideal, sep = ",", skip = 0, header = T)
    }
  obsD_3 <- predict(smooth.spline(obs_1[,1], obs_1[,2], spar = 0.5),
                    x = seq(min(obs_1$year), max(obs_1$year),1))
  obs_desired <- as.matrix(obsD_3$y)
  rownames(obs_desired) <- obsD_3$x
  colnames(obs_desired) <- "d"
  obs_data <- c(obs_data, list(obs_desired = obs_desired))
  }
  
  # Observed cohort fert by edu
  file_ccf <- file.path(dir_o, "ccf_edu")
  if (file.exists(file_ccf)){
    if(country == "FR"){
      obs_1 <- read.table(file_ccf, sep = "", skip = 0, header = T)
      obs_1[,1] <- seq(1931,1970,5)
      obs_1[nrow(obs_1),1] <- 1970
      edu <- list()
      for(i in 2:4){
      edu[[i-1]] <- predict(smooth.spline(obs_1[,1], obs_1[,i], spar = 0.2),
                            x = seq(min(obs_1$cohort), max(obs_1$cohort),1))
      }
      obs_ccf_edu <- as.matrix(cbind(edu[[1]]$y,edu[[2]]$y,edu[[3]]$y))
      row.names(obs_ccf_edu) <- edu[[1]]$x 
    }else{
      obs_1 <- read.table(file_ccf, sep = "", skip = 0, header = T)
      obs_ccf_edu <- as.matrix(obs_1[,2:4]) 
      row.names(obs_ccf_edu) <- obs_1[,1]
    }
      colnames(obs_ccf_edu) <- c("ccf_edu1", "ccf_edu2", "ccf_edu3")
      obs_ccf_edu <- obs_ccf_edu[rownames(obs_ccf_edu) %in% 1931:1966,]
      obs_data <- c(obs_data, list(obs_ccf_edu = obs_ccf_edu))  
  }
  
  return(obs_data)  
}  



