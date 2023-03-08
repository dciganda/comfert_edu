#### Retrieve Arguments
library(parallel)
# country on which simulations are performed
country <- commandArgs(TRUE)[1]
# years for which the simulations are performed
iniY <- as.numeric(commandArgs(TRUE)[2])
endY <- as.numeric(commandArgs(TRUE)[3])
# size of the initial population
ini_c <- as.numeric(commandArgs(TRUE)[4])
# get index (in sample.csv) of parameters' combination used
paramSetIndex <- as.numeric(commandArgs(TRUE)[5]) 
# nb of populations
nsim <- as.numeric(commandArgs(TRUE)[6])

weights <- commandArgs(TRUE)[7]

#### Retrieve sample of combinations
global_path <- file.path("..","results",
                         country,
                         paste0("n_sim_",nsim),
                         paste0("ini_c_",ini_c),
                         weights)

# get data.frame of design points
param_df <- read.csv(file.path(global_path,"ini_param_sample","params.csv"))

# path where results will be saved
resultsPath <- file.path(global_path,"results", paste0("param_set_", paramSetIndex))

cat('\nResults obtained with combination of parameters : \n',
    file = file.path(resultsPath, 'Info.txt'))

for(I in 1:ncol(param_df)){

 cat(names(param_df)[I], " : ", param_df[paramSetIndex, I], "\n",
     file = file.path(resultsPath, 'Info.txt'),
     append=T)
}

cat("\n\nCountry : ", country, "\n\nSize of the initial cohort : ",ini_c, "\n\n")

cat("Simulation with set of parameters : \n")
print(param_df[paramSetIndex, ])
cat("\n")

param_ls <- param_df[paramSetIndex, ]

source("compute_&_save.R")