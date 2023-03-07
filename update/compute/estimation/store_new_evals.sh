#!/bin/sh 
#SBATCH -t 00:20:00 
#SBATCH -o ../results/out_files/Job%A_%a 
#SBATCH --mem=9000 
#SBATCH -p small 

 
module load R/4.0.4 
Rscript store_new_evals.R $COUNTRY $N_SIM $INI_C $WEIGHTS $INI_YEAR $END_YEAR $NE $NT
