plot_out <- function(global_path, res_path, post_dat, pop, iniY, endY, nsim,
                     weights, asfr = F, tfr = F, ccf = F,
                     ccf_edu = F, ccf_edu_obs = F, ccf_compare = F, mab = F, mas = F,
                     mabs = F, unplanned = F, unwanted = F, desired = F,
                     gap = F, gap_edu = F, css = F, ysd = 1960, colour = F,
                     scenario = F, save = F, interval = F,
                     alpha_int = 0.05, fore = F, last_obs_year = 2019) {
  
  # browser()
  
  # to set options in axes
  scaleFUN <- function(x) sprintf("%.1f", x)
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  out_path <- file.path("..","data",pop,"out")
  save_path <- file.path("..","..","latex","plots")
  res_names <- sapply(res_path, function(x) {list.files(x, "RData", full.names = TRUE)})
  tyears <- length(iniY:(endY-1))
  every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
    if (!inverse) {
      if(empty) {
        x[1:nth == 1] <- ""
        x
      } else {
        x[1:nth != 1]
      }
    } else {
      if(empty) {
        x[1:nth != 1] <- ""
        x
      } else {
        x[1:nth == 1]
      }
    }
  }
  base_breaks_x <- function(x){
    b <- pretty(x)
    d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
    list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
         scale_x_continuous(breaks=b))
  }
  base_breaks_y <- function(x){
    b <- pretty(x)
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
         scale_y_continuous(breaks=b))
  }
  long_dat <- function(sim, obs, nsim, ysd, iniY, endY){
    
    sim_frame <- as.data.frame(do.call("cbind", sim))
    sim_frame$Year <- iniY:endY
    sim_frame$mean <- apply(sim_frame[1:nsim], 1, mean)
    sim_dat <- sim_frame[,c(nsim+1, 1:nsim, nsim+2)]
    
    if(length(obs)>1){
      
      sim_obs <- merge(sim_dat, obs, by = "Year", all = T)
      sim_obs_s <- sim_obs[sim_obs$Year >= ysd,]
      names(sim_obs_s) <- c("year", rep("Simulated", nsim),
                            "Mean of simulations", "Observed") 
      
      for(i in 2:(nsim+1)){
        colnames(sim_obs_s)[i] <- paste0(names(sim_obs_s)[i],"_", i-1)
      }
      
      dat_long <- reshape(sim_obs_s, dir = "long", idvar = "year",
                          names(sim_obs_s)[c(2:ncol(sim_obs_s))], v.names = "vals",
                          timevar = "type", times = names(sim_obs_s)[c(2:ncol(sim_obs_s))])
      dat_long$mean <- ifelse(dat_long$type %in% c("Mean of simulations", "Observed"), 1, 0)
      
    }else{
      
      sim_dat_s <- sim_dat[sim_dat$Year >= ysd,]
      names(sim_dat_s) <- c("year", rep("Simulated", nsim),
                            "Mean of simulations") 
      
      for(i in 2:(nsim+1)){
        colnames(sim_dat_s)[i] <- paste0(names(sim_dat_s)[i],"_", i-1)
      }
      dat_long <- reshape(sim_dat_s, dir = "long", idvar = "Year",
                          names(sim_dat_s)[c(2:ncol(sim_dat_s))], v.names = "vals",
                          timevar = "type", times = names(sim_dat_s)[c(2:ncol(sim_dat_s))])
      dat_long$mean <- ifelse(dat_long$type %in% c("Mean of simulations"), 1, 0)
    }
    
    dat_long$mean <- ifelse(dat_long$type == "Mean of simulations", 2, dat_long$mean )
    dat_long$r_type <- ifelse(dat_long$type %in% c("Mean of simulations", "Simulated"), 0, 1)
    dat_long$r_type <- ifelse(dat_long$type == "Mean of simulations", 2, dat_long$r_type)
    
    return(dat_long)
  } 
  p_obs_sim <- function(dat,colour, x1, x2, y1, y2, yaxe_tick = 0.1){
    
    if(colour){
      col <- c("firebrick", "darkorchid4", "firebrick")
      
    }else{
      col <- c("grey65", "grey0", "grey65")
    }
    
    lbs <- c("Simulated", "Observed", "Mean of simulations")
    
    # length_grid <- 15 # ccf_edu_obs
    # length_grid <- 25 # desired
    length_grid <- 36
    
   break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
   break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
   dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
   dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)
   
   p <- ggplot(dat,
           aes(x = year,
               y = vals,
               group = as.factor(type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(mean),
               colour = as.factor(mean))) +
     geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
                  colour = "grey84", lwd = 0.4,
                  data = break_x, inherit.aes = F,
                  alpha = 0.4)+
     geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
                  colour = "grey84", lwd = 0.4,
                  data = break_y,
                  inherit.aes = F,alpha = 0.4)+
      geom_line() +
      geom_point(alpha = 0.8) +
      scale_colour_manual(values = col,
                          labels = lbs)+
      scale_shape_manual(values = c(NA,16,21), labels = lbs)+
      scale_linetype_manual( values = c("dotted","blank","blank"), labels = lbs)+
      scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.5, 3.5))))+
      theme_bw() +
      theme(legend.position = c(0.7,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 15,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(3,"point"),
            legend.key.size = unit(1, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 10),
            axis.title=element_text(size=10))+
      theme(panel.border = element_blank(),
            panel.grid = element_blank())+
     theme(plot.margin = unit(c(1,1,1,1), "cm"))+
     list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
          scale_x_continuous(breaks = round(seq(x1,x2, 10),0)))+
     list(geom_segment(data=dy, aes(x=x, y=y, xend=xend+1, yend=yend), inherit.aes=FALSE))+
     theme(plot.background = element_rect(fill = "transparent",colour = NA),
           panel.background = element_rect(fill = "transparent",colour = NA))+
     theme(text = element_text(family = "Times New Roman"))
     

   return(p)
    
  }
  p_sim <- function(dat, colour,  x1, x2, y1, y2, yaxe_tick = 0.1){
    
    if(colour){
      col <- c("brown2", "brown2")
    }else{
      col <- c("grey65", "grey65")
    }
    
    lbs <- c("Simulated", "Mean of simulations")
    
    length_grid <- 20
    
    break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
    break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
    dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
    dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)
    
    ggplot(dat,
           aes(x = year,
               y = vals,
               group = as.factor(type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(mean),
               colour = as.factor(mean))) +
      geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
                   colour = "grey84", lwd = 0.4,
                   data = break_x, inherit.aes = F,
                   alpha = 0.4)+
      geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
                   colour = "grey84", lwd = 0.4,
                   data = break_y,
                   inherit.aes = F,alpha = 0.4)+
      geom_line() +
      geom_point() +
      scale_colour_manual(values = col,
                          labels = lbs)+
      scale_shape_manual( values = c(NA,21), labels = lbs)+
      scale_linetype_manual( values = c("dotted","blank"), labels = lbs)+
      scale_size_manual( values = c(0.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.55))))+
      theme_bw() +
      theme(legend.position = c(0.7,0.2),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 13,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(3,"point"),
            legend.key.size = unit(1, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 10),
            axis.title=element_text(size=10))+
      theme(panel.border = element_blank(),
            panel.grid = element_blank())+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
           scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                              labels = every_nth(round(seq(x1,x2,length.out =length_grid),0), 4,
                                                 inverse = TRUE)))+
      list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
           scale_y_continuous(breaks=round(seq(y1,y2,yaxe_tick),1),
                              labels = every_nth(round(seq(y1,y2,yaxe_tick),1),
                                                 length(round(seq(y1,y2,yaxe_tick),1))/4,
                                                 inverse = TRUE)))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))+
      theme(text = element_text(family = "Times New Roman"))
    
  }
  p_sim_cats <- function(dat, ylim, xlim, lbs, lbs_cats, leg_pos, cats, colour){
    
    if(colour){
      col <- c("brown2", rep("orange",cats-1))
    }else{
      col <- c("grey65", rep("black",cats-1))
    }
    
    ggplot(dat,
           aes(x = year,
               y = vals,
               group = interaction(cat, mean, type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(cat),
               colour = as.factor(cat))) +
      geom_line() +
      geom_point() +
      xlim(xlim) +
      ylim(ylim) +
      scale_colour_manual(values = col,
                          labels = lbs_cats)+
      scale_shape_manual(values = c(16,15,17,18), labels = lbs_cats)+
      scale_linetype_manual(values = c("dotdash","blank"), labels = lbs)+
      scale_size_manual(values = c(0.5, 2.5), labels = lbs)+
      guides(linetype = FALSE,
             size = FALSE,
             colour = FALSE,
             shape = guide_legend(override.aes = list(colour = col,
                                                      size=5)))+
      theme_bw() +
      theme(legend.position = leg_pos,
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 13,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(3,"point"),
            legend.key.size = unit(1, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 15),
            axis.title=element_text(size=15))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    
  }
  p_sim_obs_cats <- function(dat, x1, x2, y1, y2, lbs, lbs_cats, leg_pos, colour){
    
    if(colour){
      col <- c("firebrick", "darkorchid4", "firebrick")
    }else{
      col <- c("grey65","black","grey65")
    }
    
    length_grid <- 16
    
    break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
    break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
    dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
    dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)
    
    
    ggplot(dat,
           aes(x = year,
               y = vals,
               group = interaction(cat, mean, type),
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(mean),
               colour = as.factor(mean))) +
      geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
                   colour = "grey84", lwd = 0.4,
                   data = break_x, inherit.aes = F,
                   alpha = 0.4)+
      geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
                   colour = "grey84", lwd = 0.4,
                   data = break_y,
                   inherit.aes = F,alpha = 0.4) +
      geom_line() +
      geom_point(alpha = 0.8) +
      scale_colour_manual(values = col,
                          labels = lbs)+
      scale_shape_manual(values = c(NA,16,21), labels = lbs)+
      scale_linetype_manual(values = c("dotted","blank", "blank"), labels = lbs)+
      scale_size_manual(values = c(0.5, 2.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.5, 3.5))))+
      theme_bw() +
      theme(legend.position = c(0.7,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 15,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(3,"point"),
            legend.key.size = unit(1, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 10),
            axis.title=element_text(size=10))+
      theme(panel.border = element_blank(),
            panel.grid = element_blank())+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE))+
      list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))+
      theme(text = element_text(family = "Times New Roman"))
    
    
  }
  p_obs_fore_sc <- function(dat, x1, x2, y1, y2, lbs, lbs_cats, leg_pos, colour){

    if(colour){
      col <- c("firebrick", "darkorchid4", "orange")
    }else{
      col <- c("grey65","black","grey65")
    }

    length_grid <- 30

    break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
    break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
    dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
    dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)


    ggplot(dat,
           aes(x = year,
               y = vals,
               size = as.factor(mean),
               linetype = as.factor(mean),
               shape = as.factor(mean),
               colour = as.factor(cat))) +
      geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
                   colour = "grey84", lwd = 0.4,
                   data = break_x, inherit.aes = F,
                   alpha = 0.4)+
      geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
                   colour = "grey84", lwd = 0.4,
                   data = break_y,
                   inherit.aes = F,alpha = 0.4) +
      geom_line() +
      geom_point(alpha = 0.8) +
      scale_colour_manual(values = col,
                          labels = lbs)+
      scale_shape_manual(values = c(NA,16,16), labels = lbs, guide = "none")+
      scale_linetype_manual(values = c("dotted","blank", "blank"),
                            labels = lbs, guide = "none")+
      scale_size_manual(values = c(0.5, 2.5, 2.5), labels = lbs, guide = "none")+
      theme_bw() +
      theme(legend.position = c(0.7,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 15,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(0,"point"),
            legend.key.size = unit(0, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 10),
            axis.title=element_text(size=10))+
      theme(panel.border = element_blank(),
            panel.grid = element_blank())+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
           scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                              labels = every_nth(round(seq(x1,x2,length.out =length_grid),0), 6,
                                                 inverse = TRUE)))+
      list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
           scale_y_continuous(breaks=round(seq(y1,y2,length.out = length_grid),1),
                              labels = every_nth(round(seq(y1,y2,length.out = length_grid),1), 6,
                                                 inverse = TRUE)))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))+
      theme(text = element_text(family = "Times New Roman"))

  }

  save_plot <- function(name){
    # pdf(file.path(save_path, paste0(name,".pdf")), width=6, height=6) 
    png(file.path(save_path, paste0(name,".png")), width=800, height=800)
    print(p)
    dev.off()
  }
  shape_plot <- function(nsim, obs, sim, ylims, d_fit, colour){
    
    if(colour){
      col <- c("brown2", "grey65", "brown2")
    }else{
      col <- c("grey65", "grey0", "grey65")
    }
    
    dat_obs <- as.data.frame(obs)
    dat_obs$year <- rownames(obs)
    dat_obs <- dat_obs[dat_obs$year %in% c(1960:2011),]
    
    mean_dat_sim <- as.data.frame(Reduce("+", sim) / length(sim))
    dat_sim <- as.data.frame(do.call("cbind", sim))
    dat_sim$year <- as.numeric(rownames(dat_sim))
    dat_sim <- cbind(dat_sim, mean_dat_sim)
    names(dat_sim) <- c(rep("Simulated", nsim),"year", "Mean of simulations")
    
    dat_aux_0 <- merge(dat_obs, dat_sim, by = "year", all = T)
    dat_aux_1 <- dat_aux_0[dat_aux_0$year > 1960,]
    names(dat_aux_1)[2] <- c("Observed")
    
    dat_long <- reshape(dat_aux_1, dir = "long", idvar = "year",
                        names(dat_aux_1)[c(2:ncol(dat_aux_1))], v.names = "vals",
                        timevar = "type", times = names(dat_aux_1)[c(2:ncol(dat_aux_1))])
    
    dat_long$mean <- ifelse(dat_long$type %in% c("Mean of simulations", "Observed"), 1, 0)
    dat_long$mean <- ifelse(dat_long$type == "Mean of simulations", 2, dat_long$mean )
    dat_long$year <- as.numeric(dat_long$year)
    
    lbs <- c("Simulated", "Observed", "Mean of simulations")
    
    par(mfrow=c(1, 1))
    par(bg=NA)
    p <- ggplot(dat_long,
                aes(x = year,
                    y = vals,
                    group = as.factor(type),
                    size = as.factor(mean),
                    linetype = as.factor(mean),
                    shape = as.factor(mean),
                    colour = as.factor(mean))) +
      geom_line() +
      geom_point() +
      ylim(ylims) +
      scale_colour_manual(values = col,
                          labels = lbs)+
      scale_shape_manual( values = c(NA,16,16), labels = lbs)+
      scale_linetype_manual( values = c("dotted","blank", "blank"), labels = lbs)+
      scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.5, 3.5))))+
      theme_bw() +
      theme(legend.position = c(0.7,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 13,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(3,"point"),
            legend.key.size = unit(1, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 15),
            axis.title=element_text(size=15))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    if(d_fit){

      xlims <- c(1960,2016)
      
      p <- p + xlim(xlims)
    }
    
    return(p)
  }
  shape_plot_sim <- function(nsim, sim,x1, x2, y1, y2, colour){
    
    mean_dat_sim <- as.data.frame(Reduce("+", sim) / length(sim))
    dat_sim <- as.data.frame(do.call("cbind", sim))
    dat_sim$year <- as.numeric(rownames(dat_sim))
    dat_sim <- cbind(dat_sim, mean_dat_sim)
    names(dat_sim) <- c(rep("Simulated", nsim),"year", "Mean of simulations")
    
    dat_aux_1 <- dat_sim[dat_sim$year > 1960,]
    dat_aux_1 <- dat_aux_1[c(nsim+1,1:nsim,ncol(dat_aux_1))]
    
    dat_long <- reshape(dat_aux_1, dir = "long", idvar = "year",
                        names(dat_aux_1)[c(2:ncol(dat_aux_1))], v.names = "vals",
                        timevar = "type", times = names(dat_aux_1)[c(2:ncol(dat_aux_1))])
    
    dat_long$mean <- ifelse(dat_long$type %in% c("Mean of simulations", "Observed"), 1, 0)
    dat_long$mean <- ifelse(dat_long$type == "Mean of simulations", 2, dat_long$mean )
    dat_long$year <- as.numeric(dat_long$year)
    
    lbs <- c("Simulated", "Mean of simulations")
    
    length_grid <- 20
    break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
    break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
    dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
    dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)
    
    par(mfrow=c(1, 1))
    par(bg=NA)
    p <- ggplot(dat_long,
                aes(x = year,
                    y = vals,
                    group = as.factor(type),
                    size = as.factor(mean),
                    linetype = as.factor(mean),
                    shape = as.factor(mean),
                    colour = as.factor(mean))) +
      geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
                   colour = "grey84", lwd = 0.4,
                   data = break_x, inherit.aes = F,
                   alpha = 0.4)+
      geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
                   colour = "grey84", lwd = 0.4,
                   data = break_y,
                   inherit.aes = F,alpha = 0.4)+
      geom_line() +
      geom_point() +
      theme_bw() +
      scale_colour_manual(values = c("firebrick","firebrick"),
                          labels = lbs)+
      scale_shape_manual( values = c(NA,16), labels = lbs)+
      scale_linetype_manual( values = c("dotted","blank"), labels = lbs)+
      scale_size_manual( values = c(0.5, 2.5), labels = lbs)+
      guides(shape = guide_legend(override.aes = list(size=c(1, 3.5))))+
      theme(legend.position = c(0.7,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 15,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(3,"point"),
            legend.key.size = unit(1, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 10),
            axis.title=element_text(size=10))+
      theme(panel.border = element_blank(),
            panel.grid = element_blank())+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
           scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                              labels = every_nth(round(seq(x1,x2,length.out =length_grid),0), 5,
                                                 inverse = TRUE)))+
      list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
           scale_y_continuous(breaks=round(seq(y1,y2,length.out = length_grid),1),
                              labels = every_nth(round(seq(y1,y2,length.out = length_grid),1), 5,
                                                 inverse = TRUE)))+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))

    return(p)
  }
  
  plot_year_asfr <- function(year, obs, sim, ylims = c(0, ymax),
                             colour = colour, save_year_p = save, title = F){
    
    get_year <- function(year, dat){
      extract <- as.data.frame(as.numeric(dat[as.character(year),]))
      return(extract)
    } 
    
    obs <- get_year(year, obs)
    obs$Year <- 15:49
    
    sim <- lapply(sim, function(x) get_year(year, x))
    
    ldat <- long_dat(sim, obs, nsim, ysd = 0, iniY = 15, endY = 49)
    
    p_year <- p_obs_sim(dat = ldat,
                        x1 = 15,
                        x2 = 49,
                        y1 = ylims[1],
                        y2 = ylims[2],
                        colour = colour)
    
    p_year <- p_year + labs(y="f(x)",x = "Age") 

                    
    if(title){ # Put the year in the title
      
      p_year <- p_year + ggtitle(paste0("Year: ", year))
      
    }
    

    if(interval){  
      
      intervals <- intervals_function(res_path, post_dat,
                                      element = "asfr", year_asfr = year,
                                      iniY = iniY,
                                      endY = endY,
                                      alpha_int = alpha_int, eps = 0.1) 
      
      colnames(intervals) <- 15:49
      intervals <- t(intervals)
      
      
      ldat$int_inf <- NA
      ldat$int_sup <- NA
      
      
      for (i in 1:nrow(ldat)) {
        
        if (ldat$type[i] == 'Mean of simulations'){
          
          ldat$int_inf[i] <- as.numeric(round(intervals[match(ldat$year[i], rownames(intervals)),2],4))
          ldat$int_sup[i] <- as.numeric(round(intervals[match(ldat$year[i], rownames(intervals)),3],4))
          
        }
        
      }
      
      p_year <- p_year + geom_ribbon(aes(ymin = ldat$int_inf, 
                                         ymax = ldat$int_sup), 
                                     fill = "firebrick", alpha = 0.1, show.legend = F)
      
      
    } # USES YEAR from plot_YEAR
    
    
    if(year != 1961){
      p_year <- p_year + theme(legend.position = "none")
    }
    
    if(save_year_p){
      #pdf(paste0("../../latex/plots/","/asfr_", year,".pdf"), width=5, height=5)
      png(paste0("../../latex/plots/","/asfr_", year,".png"), width=600, height=600)
      print(p_year)
      dev.off()
    }
    return(p_year)
  }
  # browser()
  
  # get data
  source(file.path("..","estimation","get_obs.R"))
  source(file.path("..","estimation","get_sim.R"))
  source(file.path("..","estimation","intervals_function.R"))
  
  obs_set <- get_obs(pop, ysd)
  sim_set <- get_sim(res_path, iniY, endY, nsim,
                     obs_set, asfr, unplanned,
                     unwanted, desired, all_sim = T)
  
  
  
  
  if(asfr){
    
    if(class(obs_set) == "list"){
      obs <- obs_set$obs_asfr
    }else{
      obs <- obs_set
    }

    sim <- sim_set$sim_asfr
    
    ymax <- max(max(unlist(sim)), max(unlist(obs)))
     
    p <- lapply(as.numeric(rownames(obs)), plot_year_asfr, obs, sim, colour = colour)
    
    print(p)
  }
  
  #*************************************************************************************************************  
    
  if(tfr){
    # Obs
    obs <- read.table(file.path(out_path, "tfr_hfd.txt"),
                      skip = 2, header = T,
                      stringsAsFactors = F)[1:2]
    
    # sim
    sim <- sapply(res_names, function(x) readRDS(x)["tfr"])
    
    ysd <- max(1960, min(obs$Year))
    
    ldat <- long_dat(sim, obs, nsim, ysd = ysd, iniY = iniY, endY = endY-1)
    
    ldat <- ldat[ldat$year >= 1939,]
    
    if(!fore){
      
      ldat <- ldat[ldat$year <= last_obs_year,]
      name <- paste0("tfr")
    }else{
      name <- paste0("tfr","_", "fore")
    }
    
    p <- p_obs_sim(dat = ldat,
                   y1 = 0.5,
                   y2 = 3.5,
                   x1 = min(ldat$year)-1,
                   x2 = max(ldat$year)+1,
                   colour = colour)
    
    p <- p + xlab("Year") + ylab("Total Fertility Rates")
    
    if(interval){
      
      intervals <- intervals_function(res_path, post_dat,
                                      element = "tfr", 
                                      iniY = iniY,
                                      endY = endY-1,
                                      alpha_int = alpha_int)
      
      ldat$int_inf <- NA
      ldat$int_sup <- NA
      
      
      for (i in 1:nrow(ldat)) {
        
        if (ldat$type[i] == 'Mean of simulations'){
          
          ldat$int_inf[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),3],4))
          ldat$int_sup[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),4],4))
          
        }
        
      }
      
      
      p <- p + geom_ribbon(aes(ymin = ldat$int_inf, 
                               ymax = ldat$int_sup), 
                           fill = "firebrick", alpha = 0.1, show.legend = F)
      
    }
      
    if(save){save_plot(name)}
    
    print(p)
    
  } 
    
  #*************************************************************************************************************  
  
  if(ccf){
    # sim
    sim <- sapply(res_names, function(x) readRDS(x)["cohort"])
    sim <- lapply(sim, function(x) x[,2])
    # obs
    obs <- read.table(file.path(out_path,"ccf_hfd.txt"), skip = 2, header = T,
                          stringsAsFactors = F)[1:2]
    obs$CCF<- ifelse(obs$CCF==".", NA, obs$CCF)
    obs$CCF <- as.numeric(obs$CCF)
    names(obs)[1] <- "Year" 
    ldat <- long_dat(sim, obs, nsim, ysd = iniY, iniY = iniY, endY = endY-50-1)
    
    x1 <- 1935
    ldat <- ldat[ldat$year>=x1,]
    
    if(!fore){
      
      ldat <- ldat[ldat$year <= last_obs_year-50-1,]
      name <- paste0("ccf")
    }else{
      name <- paste0("ccf","_", "fore")
    }
    
    
    p <- p_obs_sim(ldat,
                   x1 = min(ldat$year),
                   x2 = max(ldat$year)+2,
                   y1 = 1,
                   y2 = 3.5,
                   colour = colour)
    
    p <- p + xlab("Cohort") + ylab("Completed cohort fertility")
    
    
    if(interval){
      
      intervals <- intervals_function(res_path, 
                                      post_dat,
                                      element = "cohort",
                                      col = 2,
                                      iniY = iniY,
                                      endY = endY-50-1,
                                      alpha_int = alpha_int)
      
      ldat$int_inf <- NA
      ldat$int_sup <- NA
      
      
      for (i in 1:nrow(ldat)) {
        
        if (ldat$type[i] == 'Mean of simulations'){
          
          ldat$int_inf[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),3],4))
          ldat$int_sup[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),4],4))
          
        }
        
      }
      
      
      p <- p + geom_ribbon(aes(ymin = ldat$int_inf, 
                               ymax = ldat$int_sup), 
                           fill = "firebrick", alpha = 0.1, show.legend = F)
      
    }
    
    if(save){ save_plot(name) }
    
    print(p)
    
    if(scenario){
      
      scenario_path <- file.path("results",pop,"scenario")
      res_names_sc <- list.files(scenario_path, "RData", full.names = TRUE)
      sim_sc <- sapply(res_names_sc, function(x) readRDS(x)["cohort"])
      sim_sc_short <- lapply(sim_sc, function(x) x[x$year>=1970,])
      sim_sc_short <- as.data.frame(do.call("cbind", sim_sc_short))
      sc_dat <- sim_sc_short[,c(1,seq(2,nsim*2,2))]
      sc_dat$mean <- apply(sc_dat[2:nsim], 1, mean)
      
      names(sc_dat) <- c("year", rep("scenario", nsim),
                          "Mean scenario")
      for(i in 2:(nsim+1)){
        colnames(sc_dat)[i] <- paste0(names(sc_dat)[i],"_", i-1)
      }
      
      sc_dat_long <- reshape(sc_dat, dir = "long", idvar = "year",
                          names(sc_dat)[c(2:ncol(sc_dat))], v.names = "vals",
                          timevar = "type", times = names(sc_dat)[c(2:ncol(sc_dat))])
      sc_dat_long$mean <- ifelse(sc_dat_long$type %in% c("Mean scenario"), 1, 0)
      
      sc_dat_long$r_type <- ifelse(sc_dat_long$type %in% c("Mean scenario", "scenario",
                                                           "Mean forecast", "forecasted"), 0, 1)

      
      ldat_sim <- ldat[ldat$type %in% unique(ldat$type)[1:(nsim+1)],]
      ldat_sim <- ldat_sim[ldat_sim$year %in% 1970:1984,]
      ldat_sim$type <- rep(names(ldat_sim)[2:7], each = length(1970:1984))  
      ldat_obs <- ldat[ldat$type %in% unique(ldat$type)[nsim+2],]
      
      long_dat_all <- rbind(ldat_obs, ldat_sim, sc_dat_long)
      long_dat_all$cat <- c(rep(0, nrow(ldat_obs)),
                            rep(1, nrow(ldat_sim)),
                            rep(2, nrow(sc_dat_long)))
      
      p <- p_obs_fore_sc(long_dat_all,
                          x1 = 1925,
                          x2 = 1985,
                          y1 = 0,
                          y2 = 3.2,
                          lbs = c("Observed","Forecasted", "Scenario"),
                          lbs_cats = c("", ""),
                          leg_pos = c(0.5, 0.8),
                          colour = colour) 

      
      if(save){ save_plot("ccf_scenario") }
      
      print("here")
      print(p)
      
    }
    
   
  }

  #*************************************************************************************************************  
    
  if(ccf_edu){
    
    sim <- list()
    sim[[1]] <- sapply(res_names, function(x) readRDS(x)["cohort"])
    for (i in 1:3) {
      sim[[1+i]] <- sapply(res_names, function(x) readRDS(x)[paste0("cohort", i)])
    }
    
    
    sim_vals <- lapply(sim, function(x) lapply(x, function(j) j[,2]))
    sim_all <- lapply(sim_vals, long_dat, obs = F,
                      nsim, ysd = iniY, iniY = iniY, endY = max(sim[[1]][[1]]$year))
    
    sim <- do.call("rbind", sim_all)
    sim$cat <- rep(0:3, each = nrow(sim_all[[1]]))
    
    
    p <- p_sim_cats(sim, ylim = c(1, 4), xlim = c(1928, 1970),
                    lbs = c("Simulated", "Mean of simulations"),
                    lbs_cats = c("Mean", "Low Edu", "Med Edu", "High Edu"),
                    leg_pos = c(0.8,0.7),
                    cats = 4,
                    colour = colour) 
    
    p <- p + xlab("Cohort") + ylab("Completed Cohort Fertility")
    
    if(interval){
      
      # There are three elements (cohort1, cohort2, cohort3)
      
      #### First element  ####
      
      intervals1 <- intervals_function(res_path, 
                                       post_dat,
                                       element = "cohort1",
                                       col = 2,
                                       iniY = iniY,
                                       endY = endY-50-1,
                                       alpha_int = alpha_int)
      
      sim$int_inf1 <- NA
      sim$int_sup1 <- NA
      
      
      for (i in 1:nrow(sim)) {
        
        if (sim$type[i] == 'Mean of simulations'){
          
          sim$int_inf1[i] <- as.numeric(round(intervals1[match(sim$year[i], intervals1$years.year),3],4))
          sim$int_sup1[i] <- as.numeric(round(intervals1[match(sim$year[i], intervals1$years.year),4],4))
          
        }
        
      }
      
      
      #### Second element #### 
      
      intervals2 <- intervals_function(res_path, 
                                       post_dat,
                                       element = "cohort2",
                                       col = 2,
                                       iniY = iniY,
                                       endY = endY-50-1,
                                       alpha_int = alpha_int)
      
      sim$int_inf2 <- NA
      sim$int_sup2 <- NA
      
      
      for (i in 1:nrow(sim)) {
        
        if (sim$type[i] == 'Mean of simulations'){
          
          sim$int_inf2[i] <- as.numeric(round(intervals2[match(sim$year[i], intervals2$years.year),3],4))
          sim$int_sup2[i] <- as.numeric(round(intervals2[match(sim$year[i], intervals2$years.year),4],4))
          
        }
        
      }

      #### Third element  #### 
      
      intervals3 <- intervals_function(res_path, 
                                       post_dat,
                                       element = "cohort3",
                                       col = 2,
                                       iniY = iniY,
                                       endY = endY-50-1,
                                       alpha_int = alpha_int)
      
      sim$int_inf3 <- NA
      sim$int_sup3 <- NA
      
      
      for (i in 1:nrow(sim)) {
        
        if (sim$type[i] == 'Mean of simulations'){
          
          sim$int_inf3[i] <- as.numeric(round(intervals3[match(sim$year[i], intervals3$years.year),3],4))
          sim$int_sup3[i] <- as.numeric(round(intervals3[match(sim$year[i], intervals3$years.year),4],4))
          
        }
        
      }
      
      # Keep inferior | superior boundaries 
      sim$int_inf <- unlist(lapply(1:nrow(sim), function(i) { min(sim$int_inf1[i], sim$int_inf2[i], sim$int_inf3[i])  }))
      sim$int_sup <- unlist(lapply(1:nrow(sim), function(i) { max(sim$int_sup1[i], sim$int_sup2[i], sim$int_sup3[i])  }))
    
      
      #### Plot ####*
      
      p <- p + geom_ribbon(aes(ymin = sim$int_inf, 
                               ymax = sim$int_sup), 
                           fill = "firebrick", alpha = 0.05, show.legend = F) 
      
      
      
      
      
      
    }
    
    if(save){save_plot("ccf_edu")}
    
    print(p)
    
  }
  
  #*************************************************************************************************************
    
  if(ccf_edu_obs){
    
    obs <- read.table(file.path(out_path,"ccf_edu"), header = T) 
    names(obs)[1] <- "Year"
    
    obs1 <- obs[,c("Year","CFR.1")] 
    obs2 <- obs[,c("Year","CFR.3")] 
    
    sim <- list()
    sim[[1]] <- sapply(res_names, function(x) readRDS(x)["cohort"])
    for (i in 1:3) {
      sim[[1+i]] <- sapply(res_names, function(x) readRDS(x)[paste0("cohort", i)])
    }
    
    sim_vals <- lapply(sim, function(x) lapply(x, function(j) j[,2]))
    sim_all <- list()
    sim_all[[1]] <- long_dat(sim_vals[[2]], obs = obs1, nsim,
                             ysd = iniY, iniY = iniY, endY = max(sim[[1]][[1]]$year))
    sim_all[[2]] <- long_dat(sim_vals[[4]], obs = obs2, nsim,
                             ysd = iniY, iniY = iniY, endY = max(sim[[1]][[1]]$year))
    
    sim_all_b <- do.call("rbind", sim_all)
    sim_all_b$cat <- rep(0:1, each = nrow(sim_all[[1]]))
    
    
    sim_all_b <- sim_all_b[sim_all_b$year>= 1935,]
    
    if(!fore){
      
      sim_all_b <- sim_all_b[sim_all_b$year <= last_obs_year-50-1,]
      
    }
    
    p <- p_sim_obs_cats(sim_all_b,
                        y1 = 1,
                        y2 = 3.5,
                        x1 = max(sim_all_b$year)+2,
                        x2 = 1933,
                        lbs = c("Simulated","Observed", "Mean of simulations"),
                        lbs_cats = c("Edu1", "Edu3"),
                        leg_pos = c(0.5, 0.8),
                        colour = colour) 
    p <- p + annotate(geom="text", x=1965, y=2.4,
                      label="Primary", 
                      color="black", 
                      size = 4) + 
      annotate(geom="text", x=1965, y=1.8,
               label="Tertiary",
               color="black",
               size = 4)
    
    p <- p + xlab("Cohort") + ylab("Completed cohort fertility")
    
    if(interval){
      
      # There are two elements (cohort1, cohort2)
      
      #### First Element  ####
      
      intervals1 <- intervals_function(res_path, 
                                      post_dat,
                                      element = "cohort1",
                                      col = 2,
                                      iniY = iniY,
                                      endY = endY-50-1,
                                      alpha_int = alpha_int)
      
      sim_all_b$int_inf1 <- NA
      sim_all_b$int_sup1 <- NA
      
      
      for (i in 1:nrow(sim_all_b)) {
        
        if (sim_all_b$type[i] == 'Mean of simulations'){
          
          sim_all_b$int_inf1[i] <- as.numeric(round(intervals1[match(sim_all_b$year[i], intervals1$years.year),3],4))
          sim_all_b$int_sup1[i] <- as.numeric(round(intervals1[match(sim_all_b$year[i], intervals1$years.year),4],4))
          
        }
        
      }
      

      #### Second element #### 
      
      intervals2 <- intervals_function(res_path, 
                                       post_dat,
                                       element = "cohort3",
                                       col = 2,
                                       iniY = iniY,
                                       endY = endY-50-1,
                                       alpha_int = alpha_int)
      
      sim_all_b$int_inf2 <- NA
      sim_all_b$int_sup2 <- NA
      
      
      for (i in 1:nrow(sim_all_b)) {
        
        if (sim_all_b$type[i] == 'Mean of simulations'){
          
          sim_all_b$int_inf2[i] <- as.numeric(round(intervals2[match(sim_all_b$year[i], intervals2$years.year),3],4))
          sim_all_b$int_sup2[i] <- as.numeric(round(intervals2[match(sim_all_b$year[i], intervals2$years.year),4],4))
          
        }
        
      }
      

      #### Plot ####*
      
      p <- p + geom_ribbon(aes(ymin = sim_all_b$int_inf1, 
                               ymax = sim_all_b$int_sup1), 
                           fill = "firebrick", alpha = 0.1, show.legend = F) +
               geom_ribbon(aes(ymin = sim_all_b$int_inf2, 
                                ymax = sim_all_b$int_sup2), 
                           fill = "firebrick", alpha = 0.1, show.legend = F)
              
      
      
    }
    
    if(save){save_plot("ccf_edu_obs") }
    
    print(p)
    
  }
    
  #*************************************************************************************************************  
      
  if(mab){
    # Obs
    obs <- read.table(file.path(out_path,"mab_hfd.txt"), 
                      skip = 2,
                      header = T,
                      stringsAsFactors = F) 
    
    obs <- obs[,1:2]
    
    # sim
    sim <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth"])
    
    ldat <- long_dat(sim, obs, nsim, ysd = 1967, iniY = iniY, endY = endY-1)
    
    if(!fore){
      
      ldat <- ldat[ldat$year <= last_obs_year-2,]
      name <- paste0("mab")
    }else{
      name <- paste0("mab","_", "fore")
    }

    p <- p_obs_sim(ldat,
                   x1 = min(ldat$year)-2,
                   x2 = max(ldat$year)+8,
                   y1 = 20,
                   y2 = 40,
                   colour = colour,
                   yaxe_tick = 1)
    
    p <- p + xlab("Year") + ylab("Age")
    
    
    if(interval){
      
      intervals <- intervals_function(res_path,
                                      post_dat,
                                      element = "meanAgeBirth", 
                                      iniY = iniY,
                                      endY = endY-1,
                                      alpha_int = alpha_int)
      
      ldat$int_inf <- NA
      ldat$int_sup <- NA
      
      
      for (i in 1:nrow(ldat)) {
        
        if (ldat$type[i] == 'Mean of simulations'){
          
          ldat$int_inf[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),3],4))
          ldat$int_sup[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),4],4))
          
        }
        
      }
      
      
      p <- p + geom_ribbon(aes(ymin = ldat$int_inf, 
                               ymax = ldat$int_sup), 
                           fill = "firebrick", alpha = 0.2, show.legend = F)
      
    }
    
    if(save){ save_plot(name) }
    
    print(p)
    
  }
  
  #*************************************************************************************************************
    
  if(mabs){
    # Obs
    obs <- read.table(file.path(out_path,"mab_parity_hfd.txt"), 
                      skip = 2, header = T,
                      stringsAsFactors = F) 
    
    obs1 <- obs[,c("MAB1","Year")] 
    obs2 <- obs[,c("MAB2","Year")]
    
    # sim
    sim1 <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth1"])
    sim2 <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth2"])
    
    sim_all <- list()
    sim_all[[1]] <- long_dat(sim1, obs = obs1, nsim, ysd = min(obs$Year), iniY = iniY, endY = endY-1)
    sim_all[[2]] <- long_dat(sim2, obs = obs2, nsim, ysd = min(obs$Year), iniY = iniY, endY = endY-1)
    
    
    sim <- do.call("rbind", sim_all)
    sim$cat <- rep(0:1, each = nrow(sim_all[[1]]))
    
    p <- p_sim_obs_cats(sim,
                        y1 = 20,
                        y2 = 40,
                        x1 = min(sim$year)-1,
                        x2 = max(sim$year)+1,
                        lbs = c("Simulated","Observed", "Mean of simulations"),
                        lbs_cats = c("MAB1", "MAB1"),
                        leg_pos = c(0.3, 0.8),
                        colour = colour)
    
    p <- p + annotate(geom="text", x=2014, y=29,
                      label="First Births",
                      color="black",
                      size = 4) + 
      annotate(geom="text", x=2014, y=35.5,
               label="Second Births",
               color="black",
               size = 4)
    
    p <- p + xlab("Year") + ylab("Mean Age at Birth")
      
    if(save){save_plot("mabs")}
    
    print(p)
    
  }
  
  #*************************************************************************************************************  
    
  if(mas){
    # Obs
    obs <- read.table(file.path(out_path,"mab_parity_hfd.txt"), 
                      skip = 2, header = T,
                      stringsAsFactors = F) 
    
    obs1 <- obs[,c("MAB1","Year")] 
    obs2 <- obs[,c("MAB2","Year")]
    
    obs3 <- read.table(file.path(out_path,"mab_hfd.txt"), 
                      skip = 2,
                      header = T,
                      stringsAsFactors = F) 
    
    obs3 <- obs3[,1:2]
    
    # sim
    sim1 <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth1"])
    sim2 <- sapply(res_names, function(x) readRDS(x)["meanAgeBirth2"])
    sim3 <- sapply(res_names, function(x) readRDS(x)["meanAgeUnion"])
    
    sim_all <- list()
    sim_all[[1]] <- long_dat(sim1, obs = obs1, nsim, ysd = min(obs$Year), iniY = iniY, endY = endY-1)
    sim_all[[2]] <- long_dat(sim2, obs = obs2, nsim, ysd = min(obs$Year), iniY = iniY, endY = endY-1)
    sim_all[[3]] <- long_dat(sim3, obs = obs3, nsim, ysd = min(obs3$Year), iniY = iniY, endY = endY-1)
    
    
    sim <- do.call("rbind", sim_all)
    cat1 <- rep(0:1, each = nrow(sim_all[[1]]))
    sim$cat <- c(cat1, rep(2, nrow(sim_all[[3]])))
    
    p <- p_sim_obs_cats(sim,
                        y1 = 20,
                        y2 = 40,
                        x1 = min(sim$year)-1,
                        x2 = max(sim$year)+1,
                        lbs = c("Simulated","Observed", "Mean of simulations"),
                        lbs_cats = c("MAB1", "MAB2", "MAU"),
                        leg_pos = c(0.3, 0.8),
                        colour = colour)
    
    p <- p + annotate(geom="text", x=2014, y=30,
                      label="First Births",
                      color="black",
                      size = 4) + 
      annotate(geom="text", x=2014, y=32.5,
               label="Second Births",
               color="black",
               size = 4) +
      annotate(geom="text", x=2014, y=24,
               label="Mean Age union",
               color="black",
               size = 4)
    
    p <- p + xlab("Year") + ylab("Mean Age at Birth")
    
    if(save){save_plot("mas")}
    
    print(p)
    
  }
  
  #*************************************************************************************************************  
    
  if(unplanned){
    
    sim <- sim_set$sim_unplanned
    
    ldat <- long_dat(sim, obs = F, nsim, ysd = 0, iniY = iniY, endY = endY-1)
    
    p <- p_sim(ldat, ylim = c(0,1), xlim= c(1958,endY), colour = colour)

    p <- p + xlab("Year") + ylab("Proportion Unplanned Births")
    
    if(save){ save_plot("unplanned") }
    
    print(p)
    
  }
  
  #*************************************************************************************************************  
    
  if(unwanted){
    
    p <-  shape_plot(nsim, obs_set$obs_unwanted, sim_set$sim_unwanted,
                     ylims = c(0,0.4),colour = colour, d_fit = F)
    
    if(save){ save_plot("unwanted") }
    
    print(p)
    
  }
  
  #*************************************************************************************************************
  #   
  # if(desired_obs){
  #   
  #   obs <- as.data.frame(obs_set$obs_desired)
  #   obs$Year <- as.numeric(rownames(obs))
  #   
  #   sim <- sim_set$sim_desired
  #   
  #   ldat <- long_dat(sim, obs, nsim, ysd = 1930, iniY = iniY, endY = endY-1) # CAMBIAR YSD
  #   
  #   if(!fore){
  #     
  #     ldat <- ldat[ldat$year <= last_obs_year-1,]
  #     name <- paste0("desired")
  #     
  #   }else{
  #     name <- paste0("desired","_", "fore")
  #   }
  #   
  #   p <- p_obs_sim(ldat,
  #                  y1 = 0.5,
  #                  y2 = 3.5,
  #                  x1 = min(ldat$year)-1,
  #                  x2 = max(ldat$year)+1,
  #                  colour = colour)
  #   
  #   
  #   p <- p + xlab("Year") + ylab("Desired Family Size")
  # 
  #   if(interval){
  #     
  #     intervals <- intervals_function(res_path, post_dat,
  #                                     element = "dKids_all",
  #                                     iniY = iniY, 
  #                                     endY = endY-1,
  #                                     alpha_int = alpha_int)
  #     
  #     ldat$int_inf <- NA
  #     ldat$int_sup <- NA
  #     
  #     
  #     for (i in 1:nrow(ldat)) {
  #       
  #       if (ldat$type[i] == 'Mean of simulations'){
  #         
  #         ldat$int_inf[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),3],4))
  #         ldat$int_sup[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),4],4))
  #         
  #       }
  #       
  #     }
  #     
  #     
  #     p <- p + geom_ribbon(aes(ymin = ldat$int_inf, 
  #                              ymax = ldat$int_sup), 
  #                          fill = "firebrick", alpha = 0.2, show.legend = F)
  #     
  #   }
  #   
  #   if(save){save_plot(name)}
  #   
  #   print(p)
  #   
  # }
  # 
  if(desired){
    
    sim <- sim_set$sim_desired
    
    ldat <- long_dat(sim, obs = F, nsim, ysd = 1960, iniY = iniY, endY = endY-1)
    
    
    if(!fore){
      
      ldat <- ldat[ldat$year <= last_obs_year-1,]
      name <- paste0("desired")
      
    }else{
      name <- paste0("desired","_", "fore")
    }
    
    p <- p_sim(ldat, 
               colour = colour,
               x1 = 1960,
               x2 = 2020,
               y1 = 1.6,
               y2 = 3)
    
    
    p <- p + xlab("Year") + ylab("Desired family size")
    
    if(interval){
      
      intervals <- intervals_function(res_path, post_dat,
                                      element = "dKids_all",
                                      iniY = iniY, 
                                      endY = endY-1,
                                      alpha_int = alpha_int)
      
      ldat$int_inf <- NA
      ldat$int_sup <- NA
      
      
      for (i in 1:nrow(ldat)) {
        
        if (ldat$type[i] == 'Mean of simulations'){
          
          ldat$int_inf[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),3],4))
          ldat$int_sup[i] <- as.numeric(round(intervals[match(ldat$year[i], intervals$years.year),4],4))
          
        }
        
      }
      
      
      p <- p + geom_ribbon(aes(ymin = ldat$int_inf, 
                               ymax = ldat$int_sup), 
                           fill = "firebrick", alpha = 0.2, show.legend = F)
      
    }
    
    if(save){save_plot(name)}
    
    print(p)
    
  }
    
  #*************************************************************************************************************
  
  if(gap){
    
    sim <- sapply(res_names, function(x) readRDS(x)["gapKids"])
    
    ldat <- long_dat(sim, obs = F, nsim, ysd = 0, iniY = iniY, endY = endY-1)
    
    p <- p_sim(ldat, ylim = c(-1,0.5), xlim= c(1958,endY), colour = colour)
    
    if(save){ save_plot("gap") }
    
    print(p)
    
  }
    
  #*************************************************************************************************************
  
  if(gap_edu){
    
    sim <- sapply(res_names, function(x) readRDS(x)["gapKids"])
    sim1 <- sapply(res_names, function(x) readRDS(x)["gapEdu1"])
    sim2 <- sapply(res_names, function(x) readRDS(x)["gapEdu2"])
    sim3 <- sapply(res_names, function(x) readRDS(x)["gapEdu3"])
    
    sim_all <- lapply(list(sim, sim1, sim2, sim3), long_dat, obs = F,
                  nsim, ysd = 0, iniY = iniY, endY = endY-1)
    
    sim <- do.call("rbind", sim_all)
    sim$cat <- rep(0:3, each = nrow(sim_all[[1]]))
    
    p <- p_sim_cats(sim, ylim = c(-1.5, 0.5), xlim = c(1958, endY),
                    lbs = c("Simulated", "Mean of simulations"),
                    lbs_cats = c("Mean", "Low Edu", "Med Edu", "High Edu"),
                    leg_pos = c(0.7, 0.3),
                    cats = 4, colour = colour)
    
    if(save){ save_plot("gap_edu") }
    
    print(p)
    
  }
    
  #*************************************************************************************************************
  
  if(css){
    
    # Obs css
    obs  <- read.table(file.path(out_path,"childlessness.csv"),
                       sep = " ",
                       skip = 0,
                       header = T)
    names(obs)[1] <- "Year" 
    obs <- obs[obs$Year %in% seq(iniY, endY-1,1),]
    
    # sim
    sim <- lapply(res_names, function(x) readRDS(x)["childless"])
    sim <- sapply(sim, function(x) lapply(x, function(j) j[-c(1:20),2]*100))
    
    ldat <- long_dat(sim, obs, nsim, ysd = 1930, iniY = 1920, endY = endY-50-1)
    
    p <- p_obs_sim(dat = ldat,
                   y1 =10,
                   y2 = 30,
                   x1 = 1930,
                   x2 = endY-50,
                   colour = colour,
                   yaxe_tick = 1)
    
    if(save){save_plot("css") }
    
    print(p)
    
  }
  
  #*************************************************************************************************************

  
  
}





