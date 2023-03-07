# Coefficients from Kradval and Rindfuss (2008).
get_coeffs <- function(){
  dksb <- as.data.frame(c(-0.12, -0.19, -0.14, -0.06, -0.01,
                          -0.45, -0.49, -0.36,	-0.2,	-0.07,
                          -0.39, -0.49, -0.37,	-0.13,	-0.08,
                          -0.77, -0.91, -0.64,	-0.23,	-0.17))
  
  dksb <- cbind(dksb,c(rep("LS",5),rep("HS",5),rep("SC",5),rep("HD",5)))
  dksb <- cbind(dksb,rep(c("1940-4",	"1945-9",	"1950-4",	"1955-9",	"1960-4"),4)) 
  dksb <- cbind(dksb,"obs")
  dksb$transition <- "second"
  colnames(dksb) <-c("coef","edu","b_cohorts", "type", "transition") 
  dktb <-as.data.frame(c(-0.27,	-0.34,	-0.21,	-0.07,	-0.04,
                         -0.63,	-0.65,	-0.37,	-0.20,	-0.18,
                         -0.46,	-0.45,	-0.15,	-0.07,	-0.03,
                         -0.81,	-0.74,	-0.12,	-0.11,	-0.01))
  dktb <- cbind(dktb,c(rep("LS",5),rep("HS",5),rep("SC",5),rep("HD",5)))
  dktb <-cbind(dktb,rep(c("1940-4",	"1945-9",	"1950-4",	"1955-9",	"1960-4"),4)) 
  dktb <-cbind(dktb,"obs")
  dktb$transition <- "third"
  colnames(dktb) <-c("coef","edu","b_cohorts", "type", "transition") 
  kr_coeffs <- rbind(dksb,dktb)
  
  return(kr_coeffs)
  
}