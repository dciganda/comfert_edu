data_prep <- function(raw_data, transition, is.desired = F){
  # convert in data frame format
  raw_data <- as.data.frame(raw_data)
  # cleaning column names
  colnames(raw_data) <- gsub("simDat.", "", colnames(raw_data))
  # create age at entry, for everyone is 14  
  raw_data$agentry <- 14
  # If desired = T keep only planned fertility
  if (is.desired){
    raw_data$selec <- ifelse(raw_data$desiredKids==raw_data$kids,1,0)
    raw_data$want <- ifelse(raw_data$kids<=raw_data$desiredKids,raw_data$kids,raw_data$desiredKids)
    raw_data[raw_data$want %in% 0,c("age1","age2","age3")] <- NA
    raw_data[raw_data$want %in% 1,c("age2","age3")] <- NA
    raw_data[raw_data$want %in%2,c("age3")] <- NA
  }
  
  if (transition == "second"){
    # include in this sample only those who already had the first child
    dat <- raw_data[!is.na(raw_data$age1),]
    # age in year is equal to age (40), for those without second child, and age at second birth for those who had the first child
    dat$ageb <- ifelse(!is.na(dat$age2), dat$age2, dat$age)
    dat$birth <- ifelse(!is.na(dat$age2), 1, 0)
    dat$entry <- dat$age1
  }
  
  if (transition == "third"){
    # include in this sample only those who already had the second child 
    dat <- raw_data[!is.na(raw_data$age2),]
    # age in year is equal to age (40), for those without third child, and age at third birth for those who hd the second child
    dat$ageb <- ifelse(!is.na(dat$age3),dat$age3,dat$age)
    dat$birth <- ifelse(!is.na(dat$age3),1,0)
    dat$entry <- dat$age2
  }
  
  # Splitting the data
  dat <- dat[dat$entry %in% (min(dat$entry, na.rm = T)):39, ]
  dat <- dat[-1, ]
  dat$id <- 1:nrow(dat)
    
  split_data <- survSplit(dat, cut = seq((min(dat$entry)), 39, 1),
                          start = "entry", end = "ageb", event = "birth")
  
  # Create duration variable and convert to data.table
  setDT(split_data)[,durat := seq_len(.N), by = "id"]
  
  # Create cohorts
  cohorts <- sort(c(seq(1940, 1970, 5), seq(1945, 1975, 5)))
  cohorts[length(cohorts)] <- cohorts[length(cohorts)]+1
  split_data$crt <- findInterval(split_data$yb, cohorts, rightmost.closed = T)
  
  #Keeping only relevant cohorts
  split_data <- split_data[split_data$crt %in% 1:13]
  # Spliting set by cohort
  split_data_crts <- split(split_data, split_data$crt)
  
  return(split_data_crts)
  
}