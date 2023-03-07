duration_models <- function(data, parity_transition, desired, activity, se){

dd <- data_prep(data, parity_transition, is.desired = desired)

models <- list()

if(activity){
models <- lapply(dd, function(x){
  glm(birth ~ bs(ageb, degree = 1, knots = c(35)) +
        bs(durat, degree = 1, knots = c(2,4,6,8)) +
        factor(edu)+
        activity,
      family = binomial("cloglog"), 
      data = x)
})
}else{
models <- lapply(dd, function(x){
  glm(birth ~ bs(ageb, degree = 1, knots = c(35)) +
        bs(durat, degree = 1, knots = c(2,4,6,8)) +
        factor(edu),
      family = binomial("cloglog"), 
      data = x)
})
}

if(parity_transition == "second"){
  dvar <- "Transition to 2nd Child" 
}else{
  dvar <- "Transition to 3rd Child"
}

stargazer(models[[1]], models[[2]], models[[3]],
          models[[4]],models[[5]],
          title="Hazard Regression Model Results - Simulated Data",
          align=TRUE,
          out = file.path("..","..","latex","tables.tex"),
          covariate.labels = c("Age knot - 1","Age knot - 2","Duration knot - 1",
                               "Duration knot - 2", "Duration knot - 3", "Duration knot - 4",
                               "Duration knot - 5","Secondary Education","Tertiary Education","Constant"),
          dep.var.caption = dvar,
          apply.coef = exp,
          column.labels = c("Cohort 1940-4", "Cohort 1945-9", "Cohort 1950-4",
                            "Cohort 1955-9", "Cohort 1960-4"),
          dep.var.labels.include = F)

cohorts2 <- sort(c("1940-4","1945-9","1950-4","1955-9","1960-4","1965-9","1970-6"))
cohorts <- matrix(sort(c(seq(1940, 1970, 5), seq(1945, 1975, 5))), 2)

# extract coefs from second birth models and prep a df from 1940 to 1976
coeff_list <- lapply(models, function(x) summary(x)$coefficients[9:10, 1:2])
coeff_list <- lapply(coeff_list, as.data.frame)
coeff_list <- lapply(1:7, function(x) cbind(coeff_list[[x]], c("edu2","edu3"),rep(cohorts2[x],2), rep("sim",2)))
coeff_list <- lapply(coeff_list, setNames ,c("coef","se","edu","b_cohorts","type"))
sim_coeff <- do.call("rbind", coeff_list)  
rownames(sim_coeff) <- NULL

sim_coeff <- sim_coeff[sim_coeff$b_cohorts %in% c("1940-4","1945-9","1950-4","1955-9","1960-4"),]
kr_coeffs <- kr_coeffs[kr_coeffs$transition == parity_transition, -c(5:ncol(kr_coeffs))]

all_coeffs <- rbind(sim_coeff[,c(1,3,4,5)], kr_coeffs)

if(se){
  
  all_coeffs$up <- c(sim_coeff$coef + sim_coeff$se*1.96, rep(NA, nrow(kr_coeffs)))
  all_coeffs$low <- c(sim_coeff$coef - sim_coeff$se*1.96, rep(NA, nrow(kr_coeffs)))
  all_coeffs$up <- exp(all_coeffs$up)
  all_coeffs$low <- exp(all_coeffs$low)
}

all_coeffs$educ <- ifelse(all_coeffs$edu %in% c("edu2","LS", "HS"),"SEC","TER") 
all_coeffs$coef <- exp(all_coeffs$coef)


names(all_coeffs)[1] <- "exp_coefs"

return(all_coeffs)



}

