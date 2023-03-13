##########################################
#              INPUT DATA                #  
##########################################
get.data <- function(ini, end) {
  

  # EDUCATIONAL ATTAINMENT OF COHORTS 1925-2016 - From Census Data | IPUMS
  edu_dist <- read.table(path_to("edu_dist"), header = T)

  # ACTIVITY STATUS COHORTS by EDUCATION 1925-2016 - From Census Data | IPUMS
  work_dist <- as.data.table(read.table(path_to("work_dist"), header = T))
  
  # AGE-SPECIFIC MORTALITY RATES by Cohort 1925-2025 | HMD
  wts_death <- readRDS(path_to("wts_death"))
  
  return(list(edu_dist = edu_dist, work_dist = work_dist , wts_death = wts_death))
}

##########################################
#              FUNCTIONS                 #  
##########################################
# Function to assign years of education
edu_y <- function(level, c_year) {
  
  afMeanLow <- approxfun(c(iniYear, endYear), c(3, 6)) 
  afMeanMed <- approxfun(c(iniYear, endYear), c(8, 12)) 
  afMeanHigh <- approxfun(c(iniYear, endYear), c(16, 22)) 
  
  if(level == 1){
    yrs <- rtrunc(1, spec = "norm", a = minEduYears,
                  b = 6, mean = afMeanLow(c_year))
  }
  if(level == 2){
    yrs <- rtrunc(1, spec = "norm", a = 6, b = 12,
                  mean = afMeanMed(c_year))
  }
  if(level == 3){
    yrs <- rtrunc(1, spec = "norm", a = 12, b = maxEduYears,
                  mean = afMeanHigh(c_year), sd = 6)
  }
  return(round(yrs))
}
# Function to compute Ages from dates  
age_calc <- function(from, to) {
  from_lt <-  as.POSIXlt(from, origin = "1970-01-01")
  to_lt <-  as.POSIXlt(to, origin = "1970-01-01")
  
  age <-  to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1L, age)
}
# Function to get expected value of truncated Gamma
get_exp_vals = function(k, theta, a, b){
  
  res <-  k*theta*(pgamma(q = c(a, b), shape = k+1, scale = theta) %*% c(1,-1) )/
    (pgamma(q = c(a, b), shape = k, scale = theta) %*% c(1, -1) )
  
  return(as.numeric(res))
}

###########################################################
#                     FIXED PARAMETERS                    #
###########################################################
list2env(fix_p, globalenv())

###########################################################
#                     AUX  VARIABLES                      #
###########################################################
Sys.setenv(TZ = "UTC") # Time is handled with POSIXct (seconds from 01.01.1970)
iniTime <- as.POSIXct(paste0(iniY,"-01-01 00:00:00"))
endTime <- as.POSIXct(paste0(endY,"-01-01 00:00:00"))
iniYear <- year(iniTime)
endYear <- year(endTime)
julys <- seq(iniTime + months(6), endTime, "years") # Aux var to identify the month of July each year.
years <- iniYear:endYear # Aux var to identify position in duration vector
time <- iniTime
elapsed <- 0L
year <- year(time)
intervalAux <- 0L
minAge <- 14L # Min age to have kids
maxAge <- 50L # Max age
srb <- 0.515 # Sex ratio at birth
pregnancy <- 270*86400L # 270 Days in seconds
tw_m <- 12*2592000L # twelve months in seconds
six_m <- 6*2592000L
secs_m <- 2592000L
secs_y <- 31536000L
mfa <- 15L # Maximum failed attempts at conception
maxKids <- 6L # Max Nr of Desired kids
maxEduYears <- 28L # Max nr of years of education edu_y Function
minEduYears <- 0L # Min nr of years of education
age2 <-  seq(minAge+.5, maxAge+.5 , 1) 
ini_mean_d <- 1940 # year from which D_bar is computed from real average 
k_v = seq(1, 30, 0.01) 
exp_v <- sapply(k_v, get_exp_vals, theta,0, maxKids)
k_vals = data.table(mean=exp_v, k=k_v) # shape values corresponding to expected values of Gamma dist.   
c_d <- 1942 #
mau_trend <- seq(ini_mau, end_mau, length.out = c_d-iniY) # mean age union early cohorts

###########################################################
#         VECTORS AND ARRAYS TO STORE RESULTS             #
###########################################################
cohort <- cohort1 <- cohort2 <- cohort3 <- data.frame(year = iniY:(endY-51), val = -1)
childless <- unionless <- one_child <- two_children <- three_plus_children <- data.frame(year = 1900:(endY-51), val = -1)
trend_d <- mau_cohort <- tfr <- meanAgeBirth <- meanAgeBirth1 <- meanAgeBirth2 <- meanAgeUnion <- vector()
aux_mau <- aux_myedu <- aux_myedu_2 <- dKids <- dKids_all <- dKids_edu1 <- dKids_edu2 <-  dKids_edu3 <-  ppBirths <- vector()
gapKids <- gapKids3 <- gapEdu1 <- gapEdu2 <- gapEdu3 <- gapBread1 <- gapDual3 <- totPop <-vector()
gap0Prob <- gap0Prob_e1 <- gap0Prob_e2 <- gap0Prob_e3 <- gap_pos <- gap_pos_e1 <- meanIntentB  <- meanNrDate <- vector() 
gap_pos_e2 <- gap_pos_e3 <- gap_neg <- gap_neg_e1 <- gap_neg_e2 <- gap_neg_e3 <- vector()
cohortEduLow <- cohortEduMed <- cohortEduHigh <- failed <- mean_age_union <- vector()
eCount <- exposure <-  list()
asfrt  <- data.frame(t(rep(NA, maxAge-minAge+1)))
prop_w_trend <- matrix(NA,1,2)
cohortExpo <- matrix(nrow = length(seq(14, 50,1)), ncol = length(seq(iniY, (endY-52), 1)))
colnames(cohortExpo) <- seq(iniY, (endY-52),1) 
row.names(cohortExpo) <- seq(14, 50,1) 
# Births Matrix for cohorts 1925 to 1965
cohortBirths <- matrix(data = 0, nrow = length(seq(14, 50,1)), ncol = length(seq(iniY-1, (endY-51), 1)))
colnames(cohortBirths) <- seq(iniY-1, (endY-51), 1) 
row.names(cohortBirths) <- seq(14, 50,1) 




