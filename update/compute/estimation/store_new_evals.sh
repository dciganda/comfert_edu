#!/bin/sh 
#SBATCH -t 00:20:00 
#SBATCH -o ../results/out_files/Job%A_%a 
#SBATCH --mem=9000 
#SBATCH -p medium 

 
module load r/4.0.3 
Rscript store_new_evals.R >> ../out_files/bo_out.Rout 2>&1 $COUNTRY $N_SIM $INI_C $WEIGHTS $INI_YEAR $END_YEAR $NE $NT
