#!/bin/sh 
#SBATCH -t 22:00:00 
#SBATCH -o ../results/out_files/Job%A_%a 
#SBATCH --mem=9000 
#SBATCH -p medium 

 
mkdir -p ../results/$COUNTRY/n_sim_$N_SIM/ini_c_$INI_C/$WEIGHTS/results/param_set_$SLURM_ARRAY_TASK_ID 

 
module load r/4.0.3 
Rscript compute_new_evals.R $COUNTRY $INI_YEAR $END_YEAR $INI_C $SLURM_ARRAY_TASK_ID $N_SIM $WEIGHTS $NT
