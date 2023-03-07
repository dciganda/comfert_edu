#!/bin/sh 
#SBATCH -t 02:00:00 
#SBATCH -o ../results/out_files/Job%A_%a 
#SBATCH --mem=9000 
#SBATCH -p small 

 
module load R/4.0.4 
Rscript get_new_evals.R >> ../out_files/bo_out.Rout 2>&1 $COUNTRY $N_SIM $INI_C $WEIGHTS $INI_YEAR $END_YEAR $N0 $NE $NT 


