#!/bin/sh 
#SBATCH -t 20:00:00 
#SBATCH -o ../results/out_files/Job%A_%a 
#SBATCH --mem=9000 
#SBATCH -p small 

 
mkdir -p ../results/$COUNTRY/n_sim_$N_SIM/ini_c_$INI_C/$WEIGHTS/results/param_set_$SLURM_ARRAY_TASK_ID 

 
module load R/4.0.4 
Rscript compute_initial_sample.R $COUNTRY $INI_YEAR $END_YEAR $INI_C $SLURM_ARRAY_TASK_ID $N_SIM $WEIGHTS
