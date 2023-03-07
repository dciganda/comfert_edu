COUNTRY=NO 
INI_YEAR=1910 
END_YEAR=2019 
N0=100 
N_SIM=5 
INI_C=2507 
N=180 
WEIGHTS=0.5_0_0_0_0.5 
NE=30 

 
mkdir -p ../out_files 
mkdir -p ../results/out_files 
mkdir -p ../results/$COUNTRY/n_sim_${N_SIM}/ini_c_$INI_C/$WEIGHTS 

 
export COUNTRY 
export INI_YEAR 
export END_YEAR 
export INI_C 
export N_SIM 
export WEIGHTS 
export N0 
export NE 

 
module load R/4.0.4 
Rscript get_initial_sample.R >> ../out_files/bo_out.Rout 2>&1 $COUNTRY $N_SIM $INI_C $N0 $WEIGHTS 

 
PREV_JOBID=$(sbatch --parsable -J="${COUNTRY}_INI_C${INI_C}_N_SIM${N_SIM}" --export=ALL --array=1-$N0 --tasks-per-node=$N_SIM -n $N_SIM -N 1 compute_initial_sample.sh) 

 
NT=$N0 
while [ $NT -lt $((N0 + N)) ]; do 
export NT 
PREV_JOBID=$(sbatch --parsable -d afterany:$PREV_JOBID -J="get_new_evals" --export=ALL --tasks-per-node=1 -n 1 -N 1 get_new_evals.sh) 
PREV_JOBID=$(sbatch --parsable -d afterok:$PREV_JOBID -J="compute_new_evals" --export=ALL --array=$((NT + 1))-$((NT + NE)) --tasks-per-node=$N_SIM -n $N_SIM -N 1 compute_new_evals.sh) 
PREV_JOBID=$(sbatch --parsable -d afterany:$PREV_JOBID -J="store_new_evals" --export=ALL --tasks-per-node=1 -n 1 -N 1 store_new_evals.sh) 
NT=$((NT+NE)) 
done
