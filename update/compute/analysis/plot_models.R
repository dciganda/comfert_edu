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
# Transition to second birth - only observed coefficients
plot_obs <- function(model,
                     level,
                     exclude_sub_level,
                     x1,
                     x2,
                     y1,
                     y2,
                     save = F){

dat <- subset(model, educ == level & type == "obs" & model == "m0")[,-c(5,6)]
labels <- rep(dat$b_cohorts[1:5], each = 5)[1:21]
dat_ref <- as.data.frame(cbind(rep(1, nrow(dat)),
                 rep("primary", nrow(dat)),
                 dat$b_cohorts,
                 dat$type,
                 rep("PRI", nrow(dat)),
                 dat$model))
names(dat_ref) <- names(dat)
dat <- rbind(dat, dat_ref)
dat$b_cohorts <- as.numeric(sub("(^[^-]+)-.*", "\\1", dat$b_cohorts))


if(level == "SEC"){
  dat$edu <- factor(dat$edu, levels = c("primary","LS", "HS"))
  lab <- c("Secondary (Lower)", "Secondary (Higher)")
  p_lab <- "Secondary"
}else{
  
  dat$edu <- factor(dat$edu, levels = c("primary","SC", "HD"))
  lab <- c("Tertiary (Lower)", "Tertiary (Higher)")
  p_lab <- "Tertiary"
}

lt <- c("solid","dashed", "twodash")

if(exclude_sub_level!= "none"){
dat <-  dat[!dat$edu == exclude_sub_level,]
lt <- c("dashed", "solid")

if(level == "SEC"){
  lab <- "Secondary"
}else{
  lab <- "Tertiary"
}

}

length_grid <- 21

break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)

p <- ggplot(dat,
       aes(x = b_cohorts,
           y = as.numeric(exp_coefs),
           group = edu,
           linetype = edu))+
  geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
               colour = "grey84", lwd = 0.4,
               data = break_x, inherit.aes = F,
               alpha = 0.4)+
  geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
               colour = "grey84", lwd = 0.4,
               data = break_y,
               inherit.aes = F,alpha = 0.4) +
  geom_line(colour = "darkorchid4")+
  theme_classic() + 
  theme_bw() +
  theme(legend.position = "none",
        legend.title=element_blank())+
  theme(legend.position = c(0.7,0.8),
        legend.title=element_blank())+
  theme(legend.text=element_text(size=13,
                                 margin = margin(l = 10, unit = "pt")),
        legend.key.height=unit(1,"line"),
        legend.key.size = unit(1.5, "cm"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.background=element_blank())+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  scale_linetype_manual(values = lt,
                        labels = c("Primary (REF.)",lab))+
  guides(linetype = guide_legend(reverse = F))+
  theme(axis.text = element_text(size = 12),
        axis.title=element_text(size=12))+
  theme(panel.border = element_blank(),
        panel.grid = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                          labels = every_nth(labels, 5,
                                             inverse = TRUE)))+
  list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=round(seq(y1,y2,length.out = length_grid),1),
                          labels = every_nth(round(seq(y1,y2,length.out = length_grid),1), 5,
                                             inverse = TRUE)))+
  xlab("Birth cohort")+
  ylab("Hazard ratios")

print(p)

if(save){
  
  pdf(file.path("..","..","latex","plots",paste0("kravdal_",p_lab,".pdf")), width=6, height=6) 
  par(mfrow=c(1, 1))
  par(bg=NA)
  print(p)
  dev.off()
  
}
}

# Transition to second birth - simulated vs observed
plot_obs_sim <- function(model, level,exclude_sub_level, x1, x2, y1, y2, parity, save = F){
  
  dat <- subset(model, educ== level & model == "m0")
  labels <- rep(dat$b_cohorts[1:5], each = 5)[1:21]
  dat_ref <- as.data.frame(cbind(rep(1, nrow(dat)),
                                 rep("primary", nrow(dat)),
                                 dat$b_cohorts,
                                 "obs",
                                 NA,
                                 NA,
                                 rep("PRI", nrow(dat)),
                                 dat$model))
  names(dat_ref) <- names(dat)
  dat <- rbind( dat_ref, dat)
  dat$b_cohorts <- as.numeric(sub("(^[^-]+)-.*", "\\1", dat$b_cohorts))
  
  if(level == "SEC"){
    
    dat$edu <- factor(dat$edu, levels = c("primary","LS", "HS", "edu2"))
    lab <- c("Primary (REF.)", "Secondary (Lower)", "Secondary (Higher)", "Simulated Secondary")
    p_lab <- "Secondary"
  
    }else{
    
    dat$edu <- factor(dat$edu, levels = c("primary","SC", "HD", "edu3"))
    lab <- c("Primary (REF.)", "Tertiary (Lower)", "Tertiary (Higher)", "Simulated Tertiary")
    p_lab <- "Tertiary"
  }
  
  lt <- c("solid","dashed", "twodash", "longdash")
  
  if(exclude_sub_level!= "none"){
    dat <-  dat[!dat$edu == exclude_sub_level,]
    lt <- c("dashed", "solid", "longdash")
    
    if(level == "SEC"){
      lab <- c("Secondary", "Simulated Secondary")
    }else{
      lab <- c("Tertiary", "Simulated Teriary")
    }
  }
 
  length_grid <- 21
  
  break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
  break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
  dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
  dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)
  
  
p <- ggplot(dat,
       aes(x = b_cohorts, y = as.numeric(exp_coefs), group = edu,
           linetype = edu, colour = edu)) +
  geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
               colour = "grey84", lwd = 0.4,
               data = break_x, inherit.aes = F,
               alpha = 0.4)+
  geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
               colour = "grey84", lwd = 0.4,
               data = break_y,
               inherit.aes = F,alpha = 0.4) +
  geom_line()+
  theme_classic() +
  geom_ribbon(aes(ymin=as.numeric(low), ymax=as.numeric(up)),
              linetype="blank", alpha=0.1, fill = "red", show.legend = F)+
  theme_bw() +
  theme(legend.position = "none",
        legend.title=element_blank())+
  theme(legend.position = c(0.7,0.8),
        legend.title=element_blank())+
  theme(legend.text=element_text(size=13,
                                 margin = margin(l = 10, unit = "pt")),
        legend.key.height=unit(1,"line"),
        legend.key.size = unit(1, "cm"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.background=element_blank())+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  scale_linetype_manual(values = lt,
                        labels = lab)+
scale_color_manual(values = c(rep("darkorchid4",3),"firebrick"),
                   labels = lab)+
  guides(linetype = guide_legend(reverse = F))+
  theme(axis.text = element_text(size = 12),
        axis.title=element_text(size=12))+
  theme(panel.border = element_blank(),
        panel.grid = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                          labels = every_nth(labels, 5,
                                             inverse = TRUE)))+
  list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=round(seq(y1,y2,length.out = length_grid),1),
                          labels = every_nth(round(seq(y1,y2,length.out = length_grid),1), 5,
                                             inverse = TRUE)))+
    xlab("Birth cohort")+
    ylab("Hazard ratios")

print(p)

if(save){
  
  pdf(file.path("..","..","latex","plots",paste0("kravdal_",p_lab,"_",parity,"_sim.pdf")), width=6, height=6) 
  par(mfrow=c(1, 1))
  par(bg=NA)
  print(p)
  dev.off()
  
}

}

###################################################################################
# DIFFERENT MODELS: Transition to SECOND births
###################################################################################
# ALL MODELS
plot_sim_models <- function(model, level,x1, x2, y1, y2, save = F){
 
  dat <- subset(model, educ== level & type == "sim" & model %in% c("m0", "m2"))
  dat_ref <- as.data.frame(cbind(rep(1, nrow(dat)),
                                 rep("primary", nrow(dat)),
                                 dat$b_cohorts,
                                 dat$type,
                                 NA,
                                 NA,
                                 rep("PRI", nrow(dat)),
                                 "m4"))
  names(dat_ref) <- names(dat)
  dat <- rbind( dat_ref, dat)
  dat$b_cohorts <- as.numeric(sub("(^[^-]+)-.*", "\\1", dat$b_cohorts))
  
  if(level == "SEC"){
    lab <- "Secondary"
  }else{
    lab <- "Tertiary"
  }
  
  length_grid <- 21
  
  break_x <- data.frame(x1, x2, y = seq(y1, y2, length.out = length_grid))
  break_y <- data.frame(y1, y2, x = seq(x1, x2, length.out = length_grid))
  dx <- data.frame(y=-Inf, yend=-Inf, x=x1, xend=x2)
  dy <- data.frame(x=-Inf, xend=-Inf, y=y1, yend=y2)
  
p <- ggplot(dat,
       aes(x = b_cohorts, y = as.numeric(exp_coefs), group = model,
           linetype = model, colour = model))+
  geom_segment(aes(x = x1, y = y, xend = x2, yend =y),
               colour = "grey84", lwd = 0.4,
               data = break_x, inherit.aes = F,
               alpha = 0.4)+
  geom_segment(aes(x = x, y = y1, xend = x, yend =y2),
               colour = "grey84", lwd = 0.4,
               data = break_y,
               inherit.aes = F,alpha = 0.4) +
  geom_line(colour = "firebrick")+
  theme_classic() +
  geom_ribbon(aes(ymin=as.numeric(low), ymax=as.numeric(up)),
              linetype="blank", alpha=0.1, fill = "red", show.legend = F)+
  theme_bw() +
  theme(legend.position = "none",
        legend.title=element_blank())+
  theme(legend.position = c(0.45,0.8),
        legend.title=element_blank())+
  theme(legend.text=element_text(size=13, margin = margin(l = 10, unit = "pt")),
        legend.key.height=unit(1,"line"),
        legend.key.size = unit(1, "cm"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.background=element_blank())+
  theme(plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  scale_linetype_manual(values = c( "twodash", "dashed","solid"),
                        labels = c("Original Model - Simulated",
                                   "Model Desired Fertility - Simulated",
                                   "Primary (REF.) - Simulated"))+
scale_color_manual(values = c("red", "red2", "red4"),
                   labels = c("Original Model - Simulated",
                              "Model Desired Fertility - Simulated",
                              "Primary (REF.) - Simulated"))+
  guides(linetype = guide_legend(reverse = TRUE),
         colour = guide_legend(reverse = TRUE))+
  theme(axis.text = element_text(size = 12),
        axis.title=element_text(size=12))+
  theme(panel.border = element_blank(),
        panel.grid = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  list(geom_segment(data=dx, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks = round(seq(x1,x2, length.out = length_grid),0),
                          labels = every_nth(round(seq(x1,x2, length.out = length_grid),0), 5,
                                             inverse = TRUE)))+
  list(geom_segment(data=dy, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=round(seq(y1,y2,length.out = length_grid),1),
                          labels = every_nth(round(seq(y1,y2,length.out = length_grid),1), 5,
                                             inverse = TRUE)))+
  xlab("Birth cohort")+
  ylab("Hazard ratios")

print(p)

if(save){
  
  pdf(file.path("..","..","latex","plots",paste0("sim_models_",level,".pdf")), width=6, height=6) 
  par(mfrow=c(1, 1))
  par(bg=NA)
  print(p)
  dev.off()
  
}
}

