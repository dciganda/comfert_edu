comfert_trajectories <- function(seed_val, param, ini_c, cluster = F){
  
  setDTthreads(1)
  
   param_list <- colnames(param)
   for(i in param_list){
     if(length(param[, i]) > 0){
       assign(i, param[, i])
     }
   }

  set.seed(seed_val)
  
  ##########################################
  # FUNCTIONS                              #  
  ##########################################
  # Effect of Contraception on Fecundability #################
  cnt <- function(y, yr){
    out <- upsilon/((y+1)^alpha) / (1 + exp(r*(yr-(psi-(y+1))))) + rho
    return(out)
  }
  # Effect of years of edu on Intention ######################
  edu_int <- function(y){
    out <- eta / (1 + exp(epsilon*(y-tau)))  
    return(out)
  }
  ###########################################################
  # LOAD DATA                                               #  
  ###########################################################
  data <- get.data(ini = iniYear, end = endYear)
  cat("Data loaded ok\n") ###################################
  edu_dist <- data$edu_dist
  work_dist <- data$work_dist
  wts_death <- data$wts_death
  ###########################################################
  # INITIAL BIRTHS                                          #  
  ###########################################################
  wtb <- runif(ini_c)
  swtb <- wtb/max(wtb) * secs_y 
  ###########################################################
  # POP                                                     #  
  ###########################################################
  pop <- data.table(wtDeath = Inf,
                    wtUnion = Inf,
                    wtBirth = Inf,
                    wtEvalBirth = Inf,
                    age = 14L,
                    ageSec = 441504000,
                    tob = -2335199999,
                    edu = 3L,
                    edu_y = 20L,
                    activity = 0L,
                    union = 0L,
                    unionAge = 0L,
                    desiredKids = 0L,
                    kids = 0L,
                    ageBirth = 0L,
                    timeBirth = 0,
                    kidsLeft = 0L,
                    birthYear = 0L,
                    pregnant = 0L,
                    unionYear = 0L,
                    baseline = 0,
                    intentBirth = 0,
                    failedAttempts = 0L,
                    unplanned = 0L,
                    age1 = NA_integer_,
                    age2 = NA_integer_,
                    age3 = NA_integer_
  )
  
  ###################
  # Aux Vars        # 
  ###################
  eventNames <- c("death", "union", "birth", "evalBirth")
  wt_col <- paste0("wt", toupper(substr(eventNames, 1, 1)), substr(eventNames, 2, 1000L))
  eventCount <- setNames(as.list(rep(0, length(eventNames))), eventNames)
  mau <- mau_trend[1]
  
  #####################
  # Trajectories data # 
  #####################
  vars <- c("tob","age","edu","age1","age2","age3","desiredKids","kids","activity")
  simDat <- setNames(data.table(matrix(nrow = 0, ncol = length(vars))), vars)

  cat("Initialisation ok, 'entering' while loop\n")  ######################################## 
  
  #****************************************************************************************************************************************
  #-------------------------------------------------------- RUNNING THE SIMULATION -------------------------------------------------------#
  #****************************************************************************************************************************************
  while (time < endTime){
    
    min_idx <- pop[, arrayInd(which.min(as.matrix(.SD)), .dim = dim(.SD)), .SDcols = wt_col]
    timeNextEvent <- pop[[min_idx[1], min_idx[2]]]
    nextEvent <- eventNames[min_idx[2]]
    rid <- min_idx[1]
    
    # next event*
    if(length(swtb) > 0){  
      if(timeNextEvent > min(swtb)){
        nextEvent <- "birth"
        timeNextEvent <- min(swtb)
        rid <- NA
        swtb <- swtb[-which.min(swtb)]
      }
      swtb <- swtb - timeNextEvent
    }
    eventCount[[nextEvent]] <- eventCount[[nextEvent]] + 1

    #****************************************************************************************************************************************
    #--------- UPDATING ---------------------------------------------------------------------------------------------------------------------
    #****************************************************************************************************************************************
    # Clock.
    time <- time + timeNextEvent
    elapsed <- elapsed + timeNextEvent
    # Update Age of all agents.
    pop[, ageSec := ageSec + timeNextEvent]
    # Update WTs of all agents
    pop[, c(wt_col):= lapply(.SD, function(x){x-timeNextEvent}), .SDcols = wt_col]

    ########################
    ##       UNION        ##
    ########################
    if (nextEvent=="union") {
      pop[rid, age := age_calc(tob, time)]
      pop[rid, `:=`(union = 1L, 
                    unionAge = age,
                    wtUnion = Inf,
                    unionYear = 1L)]
      
      mean_gamma <- mean_d * (1 + (delta  * (-1)^pop[rid,activity] * (1-prop_w[pop[rid,activity]+1])))
      k <- k_vals[which.min(abs(mean_gamma-mean)), k]
      
      pop[rid, desiredKids := round(rtrunc(1,
                                           spec ="gamma",
                                           a = 0, b = maxKids,
                                           shape = k,
                                           scale = theta),
                                    0)]
      pop[rid, kidsLeft := desiredKids]
      pop[rid, baseline := ifelse(kidsLeft <= 0L, 0, 0.8)]
      pop[rid, intentBirth := baseline - edu_int(y = edu_y) * activity]  
      
      have_child <-  runif(1) < pop[rid, intentBirth]
      pop[rid, unplanned := 0L]
      
      if(have_child){ # go for planned conception
        
        wtConception <- rexp(1, rate = phi / (1 + exp(kappa*(pop[rid, age] - gamma)))) * secs_m
        
        if(wtConception < tw_m){ # conception within the year
          
          pop[rid, `:=` (pregnant = 1, wtBirth = wtConception + pregnancy)]  
          
        }else{ # failed to conceive
          
          pop[rid, `:=` (failedAttempts = failedAttempts + 1, wtEvalBirth = tw_m)]
          
        }
      }else{ # unplanned risk
        
        wtConception <- rexp(1, rate = cnt(y = pop[rid, edu_y], yr = year(time)) * 
                               phi / (1 + exp(kappa*(pop[rid, age]-gamma))) * A^pop[rid, kidsLeft<=0]) * secs_m # unplanned wt
        
        if(wtConception < tw_m){ # unplanned conception within the year
          
          pop[rid, `:=` (pregnant = 1, unplanned = 1L, wtBirth = wtConception + pregnancy)]  
          
        }else{ # no unplanned conception
          
          pop[rid, `:=` (failedAttempts = failedAttempts + 1, wtEvalBirth = tw_m)]
          
        }
      }
    } 
    
    ########################
    ##     EVAL BIRTH     ##
    ########################
    if (nextEvent == "evalBirth") {

      pop[rid, intentBirth := baseline - edu_int(y = edu_y) * activity -
            (baseline - edu_int(y = edu_y) * activity) * exp(-lambda * (elapsed - timeBirth))]

      have_child <-  runif(1) < pop[rid, intentBirth]
      pop[rid, unplanned := 0L]
      
      if(have_child){ # go for planned conception
        
        wtConception <- rexp(1, rate = phi / (1 + exp(kappa*(pop[rid, age]-gamma)))) * secs_m
        
        if(wtConception < tw_m){ # conception within the year
          
          pop[rid, `:=` (pregnant = 1, wtBirth = wtConception + pregnancy, wtEvalBirth = Inf)]  
          
        }else{ # failed to conceive
          
          pop[rid, failedAttempts := failedAttempts + 1]
          pop[rid, `:=` (wtEvalBirth = ifelse(failedAttempts == mfa, Inf, tw_m))]
          
        }
      }else{ # unplanned risk
        
        wtConception <- rexp(1, rate = cnt(y = pop[rid, edu_y], yr = year(time)) * 
                               phi / (1 + exp(kappa*(pop[rid, age]-gamma))) * A^pop[rid, kidsLeft<=0]) * secs_m # unplanned wt
        
        if(wtConception < tw_m){ # unplanned conception within the year
          
          pop[rid, `:=` (pregnant = 1, unplanned = 1L, wtBirth = wtConception + pregnancy, wtEvalBirth = Inf)]  
          
        }else{ # no unplanned conception
          
          pop[rid, failedAttempts := failedAttempts + 1]
          pop[rid, `:=` (wtEvalBirth = ifelse(failedAttempts == mfa, Inf, tw_m))]
          
        }
      }
    }
    
    ########################
    ##       BIRTH        ##
    ########################
    if (nextEvent == "birth") {
      if(is.na(rid)){
        girl <- Inf
      }else{
        girl <- runif(1)
      }
      if (girl > srb){
        pop <- rbindlist(list(pop, pop[.N+1]), fill = T)
        pop[.N, `:=` (age = 0L,
                      ageSec = 0,
                      tob=as.numeric(time),
                      union = 0L,
                      kids = 0L,
                      timeBirth = 0,
                      birthYear = 0L,
                      pregnant = 0L,
                      unionYear = 0L,
                      failedAttempts = 0L,
                      wtBirth = Inf,
                      baseline = NA_real_,
                      intentBirth = NA_real_,
                      ageBirth = NA_integer_,
                      wtEvalBirth = Inf,
                      unplanned = 0L,
                      desiredKids = NA_integer_,
                      kidsLeft = NA_integer_,
                      age1 = NA_integer_,
                      age2 = NA_integer_,
                      age3 = NA_integer_
                      )]
        
        pop[.N, edu := findInterval(runif(1),
                                    edu_dist[edu_dist$year == year(time),1:3],
                                    rightmost.closed = TRUE) + 1]
        pop[.N, edu_y := edu_y(edu, year(time))]
        pop[.N, activity := findInterval(runif(1), 
                                         work_dist[year == year(time), 
                                                  ((edu*2)-1):(((edu*2)-1)+1)],
                                         rightmost.closed = FALSE)]

        unionProb <- u[year(time)-(year(iniTime)-1)]
        if(runif(1) > unionProb){
          
          if(year(time) >= c_d){
            input_age_union <- max(end_mau, 6 + pop[.N, edu_y] + xi)
          }else{
            input_age_union <- mau
          }
          
          wtunionaux <- rlnorm(1,
                               meanlog = log(mean(c(mau, input_age_union))),
                               sdlog = sd_lnrm) 
          
          pop[.N, unionAge := floor(wtunionaux)]
          pop[.N, wtUnion := wtunionaux*secs_y] # seconds from years
          
        }else{
          pop[.N, wtUnion := Inf]  
          pop[.N, unionAge := NA_integer_]
        }
  
        wtd <- sample(wts_death[[year(time) - (iniY-1)]],size = 1)
        wts_death[[year(time) - (iniY-1)]] <- wts_death[[year(time) - (iniY-1)]][-as.numeric(names(wtd))]
        pop[.N, wtDeath := wtd*secs_y] 
        
      }
      
      # Change Indicators for mother
      if(!is.na(rid)){
        pop[rid, age := age_calc(tob, time)]
        pop[rid, `:=`(ageBirth = age,
                      timeBirth = elapsed,
                      pregnant = 0,
                      wtBirth = Inf,
                      kids = kids + 1L)]
        if(pop[rid, kids] <= 3){
        pop[rid, (paste0("age", pop[rid, kids])) := age]
        }
        pop[rid, birthYear := ifelse(unplanned == 1, 2, 1)] # 2 = unplanned birth
        pop[rid, kidsLeft := desiredKids - kids]
        pop[rid, baseline := ifelse(kidsLeft <= 0, 0, baseline)]
        # Redefine intention: 
        pop[rid, wtEvalBirth := six_m]
      }
    }
    
    ########################
    ##       DEATH        ##
    ########################
    if (nextEvent == "death") {
      pop <- pop[-rid]
    }
    
    #****************************************************************************************************************************************#
    #--------- INDICATORS--------------------------------------------------------------------------------------------------------------------#
    #****************************************************************************************************************************************#
    if (intervalAux != findInterval(time, julys)){
      # Number of women by age to compute ASFR
      pop[ageSec > 13L*secs_y, age := age_calc(tob, time)]
      womenAge <- tabulate(pop[age %in% minAge:maxAge, age])
      womenAge <- womenAge[minAge:maxAge]
      womenAge[which(is.na(womenAge))] <- 0
      intervalAux <- intervalAux + 1
      
      pop <- pop[age < 52]
    }
    
    if (year != year(time)){
      print(time)
      # Generating new births for burn-in period
      if (year <= iniYear + minAge){
        wtb <- runif(ini_c)
        swtb <- wtb/max(wtb) * secs_y # seconds from years
      }else{
        if (year > iniYear + minAge & year <= iniYear + (maxAge-minAge-1)){
          wtb <- runif(ini_c)
          swtb <- wtb/max(wtb) * secs_y 
          rw <- (year-(iniYear + minAge))/(iniYear + maxAge-(iniYear + minAge))
          rwt <- sample(1:length(swtb),round(length(swtb)*rw,0))
          swtb <- swtb[-rwt]
        }else{
          wtb <- NULL
          swtb <- Inf
        }
      }
      
      # Mean D of younger cohort
      trend_d <- c(trend_d, mean(pop[age %in% minAge:25, desiredKids], na.rm = T))
      mean_d <- mean(trend_d[length(trend_d)], na.rm = T)
      if(year(time)<ini_mean_d){mean_d <- D_0}
      prop_w <- as.numeric(prop.table(table(factor(pop[age %in% minAge:maxAge, activity], levels = 0:1))))
      
      # Mean D
      dKids <- c(dKids, pop[age %in% 25:34, mean(desiredKids, na.rm = T)]) 
      dKids_all <- c(dKids_all, pop[age > 19, mean(desiredKids, na.rm = T)]) 
      dKids_edu1 <- c(dKids_edu1, pop[age > 19 & edu == 1, mean(desiredKids, na.rm = T)])
      dKids_edu2 <- c(dKids_edu2, pop[age > 19 & edu == 2, mean(desiredKids, na.rm = T)])
      dKids_edu3 <- c(dKids_edu3, pop[age > 19 & edu == 3, mean(desiredKids, na.rm = T)])
      
      # Unions
      yob <- year(as.POSIXlt(pop[,tob], origin="1970-01-01"))

      id_last_year <- which(yob == year(time) - 1) # born last year
      union_ages <- pop[id_last_year, unionAge]
      mau_cohort <- c(mau_cohort, mean(union_ages, na.rm = T))
      
      # Number of births by age to compute ASFR
      birthsAge <- tabulate(pop[age %in% minAge:maxAge & birthYear > 0, ageBirth])
      birthsAge <- birthsAge[minAge:maxAge]
      birthsAge[which(is.na(birthsAge))] <- 0
      # ASFR
      asfr  <-  birthsAge/womenAge # Age-specific Fertility Rates
      asfrt  <-  rbind(asfrt, data.frame(t(birthsAge/womenAge))) # ASFR
      
      # Unwanted Births
      wBirthsAge <- tabulate(pop[age %in% minAge:maxAge & kidsLeft >= 0 & birthYear > 0, ageBirth])
      wBirthsAge <- wBirthsAge[minAge:maxAge]
      wBirthsAge[which(is.na(wBirthsAge))] <- 0
      # Unplanned Births
      pBirthsAge <- tabulate(pop[age %in% minAge:maxAge & birthYear == 1, ageBirth])
      pBirthsAge <- pBirthsAge[minAge:maxAge]
      pBirthsAge[which(is.na(pBirthsAge))] <- 0
      # Births by type
      ppBirths <- rbind(ppBirths, c(sum(birthsAge), sum(pBirthsAge), sum(wBirthsAge))) 
      
      if(!cluster){
        prop_w_trend <- rbind(prop_w_trend, prop_w)
        
        # Cohort Fertility - Method 1.
        cId <- which(yob==(year(time)-1)-50) # Turned 51 in year t.
        eduId1 <- which(pop[,edu == 1])
        eduId2 <- which(pop[,edu == 2])
        eduId3 <- which(pop[,edu == 3])
        
        if(!is.na(yob[cId[1]]) & yob[cId[1]] >= iniYear){
          
          cohort[!is.na(cohort[,1]) & cohort[,1] == paste(yob[cId[1]]),2] <- pop[cId, mean(kids, na.rm = T)] # Mean number of kids cohort
          cohort1[!is.na(cohort1[,1]) & cohort1[,1] == paste(yob[cId[1]]),2] <- pop[intersect(eduId1,cId), mean(kids, na.rm = T)]
          cohort2[!is.na(cohort1[,1]) & cohort2[,1] == paste(yob[cId[1]]),2] <- pop[intersect(eduId2,cId), mean(kids, na.rm = T)] 
          cohort3[!is.na(cohort1[,1]) & cohort3[,1] == paste(yob[cId[1]]),2] <- pop[intersect(eduId3,cId), mean(kids, na.rm = T)]
        }
        
        if(!is.na(yob[cId[1]]) & yob[cId[1]] >= 1900){
          childless[childless[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(kids))[1]]
          one_child[one_child[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(kids))[2]]
          two_children[two_children[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(kids))[3]]
          three_plus_children[three_plus_children[,1] == paste(yob[cId[1]]),2] <- sum(pop[cId, prop.table(table(kids))[4]],
                                                                                      pop[cId, prop.table(table(kids))[5]],
                                                                                      pop[cId, prop.table(table(kids))[6]],
                                                                                      pop[cId, prop.table(table(kids))[7]], na.rm = T)
          unionless[unionless[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(union))[1]]
        }
        
        mean_age_union <- c(mean_age_union, mean(pop[age %in% minAge:maxAge & unionYear == 1, unionAge])) # period
        unionsAge <- tabulate(pop[age %in% minAge:maxAge & unionYear == 1, unionAge])
        unionsAge <- unionsAge[minAge:maxAge]
        unionsAge[which(is.na(unionsAge))] <- 0
        
        birthsAge1 <- tabulate(pop[age %in% minAge:maxAge & birthYear > 0 & kids == 1, ageBirth])
        birthsAge1 <- birthsAge1[minAge:maxAge]
        birthsAge1[which(is.na(birthsAge1))] <- 0
        
        birthsAge2 <- tabulate(pop[age %in% minAge:maxAge & birthYear > 0 & kids == 2, ageBirth])
        birthsAge2 <- birthsAge2[minAge:maxAge]
        birthsAge2[which(is.na(birthsAge2))] <- 0
        
        icf <- sum(asfr) # TFR
        if (is.na(icf)){icf <- 1}
        tfr <- c(tfr, icf)
        
        asfr1  <-  birthsAge1/womenAge # Age-specific Fertility Rates (1st)
        asfr2  <-  birthsAge2/womenAge # Age-specific Fertility Rates (2nd)
        icf1 <- sum(asfr1) # TFR 1 
        icf2 <- sum(asfr2) # TFR 2
        asfrAge  <- asfr %*% age2 # Mean Age
        meanAgeBirth <- c(meanAgeBirth, asfrAge / icf)
        asfrAge1  <- asfr1 %*% age2 # Mean Age 1st
        meanAgeBirth1 <- c(meanAgeBirth1, asfrAge1 / icf1)
        asfrAge2  <- asfr2 %*% age2 # Mean Age 2st
        meanAgeBirth2 <- c(meanAgeBirth2,asfrAge2 / icf2)
        
        asnr <- unionsAge/womenAge
        icn <- sum(asnr)
        if (is.na(icn)){icn <- 1}
        asnrAge  <- asnr %*% age2 # Mean Age
        meanAgeUnion <- c(meanAgeUnion, asnrAge / icn)
        
        meanIntentB <- c(meanIntentB, pop[age %in% 14:51, mean(intentBirth, na.rm = T)]) # mean intention
        
        failed <- c(failed, pop[age %in% 45:51, mean(failedAttempts, na.rm = T)])
        
        totPop <- c(totPop, pop[,.N]) # Total Population
        
        gapKids <- c(gapKids, pop[age %in% 48:51, mean(kidsLeft, na.rm = T)]) # Gap Kids
        gapAux <- pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft]
        gapAux <- ifelse(gapAux < 0, 0, gapAux)
        gapKids3 <- c(gapKids3, mean(gapAux, na.rm = T))
        gapEdu1 <- c(gapEdu1, pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])
        gapEdu2 <- c(gapEdu2, pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])
        gapEdu3 <- c(gapEdu3, pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])
        gapBread1 <- c(gapBread1, pop[age %in% 48:51 & edu == 1 & activity == 0 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])
        gapDual3 <- c(gapDual3, pop[age %in% 48:51 & edu == 3 & activity == 1 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])
        
        k_lft0 <- sum(pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft == 0]) / length(pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft]) 
        k_lft0_e1 <- sum(pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), kidsLeft == 0]) / length(pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), kidsLeft]) 
        k_lft0_e2 <- sum(pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), kidsLeft == 0]) / length(pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), kidsLeft]) 
        k_lft0_e3 <- sum(pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), kidsLeft == 0]) / length(pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), kidsLeft]) 
        
        
        k_lft_pos <- sum(pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft > 0]) / length(pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft]) 
        k_lft_pos_e1 <- sum(pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), kidsLeft > 0]) / length(pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), kidsLeft]) 
        k_lft_pos_e2 <- sum(pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), kidsLeft > 0]) / length(pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), kidsLeft]) 
        k_lft_pos_e3 <- sum(pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), kidsLeft > 0]) / length(pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), kidsLeft]) 
        
        
        k_lft_neg <- sum(pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft < 0]) / length(pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft]) 
        k_lft_neg_e1 <- sum(pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), kidsLeft < 0]) / length(pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), kidsLeft]) 
        k_lft_neg_e2 <- sum(pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), kidsLeft < 0]) / length(pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), kidsLeft]) 
        k_lft_neg_e3 <- sum(pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), kidsLeft < 0]) / length(pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), kidsLeft]) 
        
        
        gap0Prob <- c(gap0Prob, k_lft0)
        gap0Prob_e1 <- c(gap0Prob_e1, k_lft0_e1)
        gap0Prob_e2 <- c(gap0Prob_e2, k_lft0_e2)
        gap0Prob_e3 <- c(gap0Prob_e3, k_lft0_e3)
        
        gap_pos <- c(gap_pos, k_lft_pos)
        gap_pos_e1 <- c(gap_pos_e1, k_lft_pos_e1)
        gap_pos_e2 <- c(gap_pos_e2, k_lft_pos_e2)
        gap_pos_e3 <- c(gap_pos_e3, k_lft_pos_e3)
        
        gap_neg <- c(gap_neg, k_lft_neg)
        gap_neg_e1 <- c(gap_neg_e1, k_lft_neg_e1)
        gap_neg_e2 <- c(gap_neg_e2, k_lft_neg_e2)
        gap_neg_e3 <- c(gap_neg_e3, k_lft_neg_e3)
        
        cohortEduLow <- c(cohortEduLow, pop[age == 0, prop.table(tabulate(edu))[1]]) # Education of cohorts
        cohortEduMed <- c(cohortEduMed, pop[age == 0, prop.table(tabulate(edu))[2]]) 
        cohortEduHigh <- c(cohortEduHigh, pop[age == 0, prop.table(tabulate(edu))[3]])
        
        eCount[[year(time)-(year(iniTime)-1)]] <- eventCount
        
        aux_mau <- c(aux_mau, mau)
        aux_myedu <- c(aux_myedu, mean(pop[age %in% minAge:maxAge & unionYear == 1, edu_y], na.rm = T))
        aux_myedu_2 <- c(aux_myedu_2, mean(pop[age %in% minAge:maxAge, edu_y], na.rm = T))
        
        eventCount <- setNames(as.list(rep(0, length(eventNames))), eventNames)
        
      }
      
      # MicroData
      simDat <- rbind(simDat, pop[age == 40, ..vars]) 

      # Reset indicators
      if (year(time) >= c_d) {
          mau <- mau_cohort[length(mau_cohort)]
      }else{
          mau <- mau_trend[year(time) - (iniYear-1)]
      }
      pop[,birthYear := 0]
      pop[, unionYear := 0]
      year <-year + 1
    }
    
  }

  cat("exit while loop\n")
  
  if(cluster){
    out <- list(iniYear = iniYear, endYear = endYear, asfrt = asfrt,
                dKids = dKids, dKids_all = dKids_all, ppBirths = ppBirths,
                cohort = cohort, cohort1 = cohort1, cohort2 = cohort2, cohort3 = cohort3, simDat = simDat)  
  }else{
    out <- list(iniYear = iniYear, endYear = endYear, cohort = cohort, cohort1 = cohort1, cohort2 = cohort2, cohort3 = cohort3,
                tfr = tfr, asfrt = asfrt, dKids = dKids, dKids_all = dKids_all, dKids_edu1 = dKids_edu1, dKids_edu2 = dKids_edu2,
                dKids_edu3 = dKids_edu3, meanAgeBirth = meanAgeBirth, meanAgeBirth1 = meanAgeBirth1,
                meanAgeBirth2 = meanAgeBirth2, meanAgeUnion = meanAgeUnion, childless = childless, unionless = unionless,
                ppBirths = ppBirths, meanIntentB = meanIntentB, failed = failed, totPop = totPop, gapKids = gapKids,
                gapKids3 = gapKids3, gapEdu1 = gapEdu1, gapEdu2 = gapEdu2, gapEdu3 = gapEdu3, gapBread1 = gapBread1,
                gapDual3 = gapDual3, gap0Prob = gap0Prob, gap0Prob_e1 = gap0Prob_e1, gap0Prob_e2 = gap0Prob_e2, gap0Prob_e3 = gap0Prob_e3,
                gap_pos = gap_pos, gap_pos_e1 = gap_pos_e1, gap_pos_e2 = gap_pos_e2, gap_pos_e3 = gap_pos_e3, 
                gap_neg = gap_neg, gap_neg_e1 = gap_neg_e1, gap_neg_e2 = gap_neg_e2, gap_neg_e3 = gap_neg_e3, 
                cohortEduLow = cohortEduLow, cohortEduMed = cohortEduMed, cohortEduHigh = cohortEduHigh, exposure = exposure,
                cohortBirths = cohortBirths, mean_age_union = mean_age_union, aux_mau = aux_mau, 
                aux_myedu = aux_myedu, aux_myedu_2 = aux_myedu_2, mau_cohort = mau_cohort, one_child = one_child,
                two_children = two_children, three_plus_children = three_plus_children, prop_w_trend = prop_w_trend, simDat = simDat)
  }
  
  return(out)
  
} 
