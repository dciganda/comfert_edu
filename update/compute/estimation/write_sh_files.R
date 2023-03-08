sink("../estimation/run_bayes_opt_cluster.sh")
cat(paste0("COUNTRY=",country),
    paste0("INI_YEAR=",iniY),
    paste0("END_YEAR=",endY),
    paste0("N0=",n0),
    paste0("N_SIM=",nsim),
    paste0("INI_C=",ini_c),
    paste0("N=",N),
    paste0("WEIGHTS=",paste(weights, collapse = "_")),
    paste0("NE=",ne),"\n",
    "mkdir -p ../out_files",
    "mkdir -p ../results/out_files",
    paste("mkdir -p", file.path("..","results","$COUNTRY","n_sim_${N_SIM}","ini_c_$INI_C", "$WEIGHTS")), "\n",
    "export COUNTRY",
    "export INI_YEAR",
    "export END_YEAR",
    "export INI_C",
    "export N_SIM",
    "export WEIGHTS",
    "export N0",
    "export NE","\n",
    paste0(ml),
    "Rscript get_initial_sample.R >> ../out_files/bo_out.Rout 2>&1 $COUNTRY $N_SIM $INI_C $N0 $WEIGHTS","\n",
    'PREV_JOBID=$(sbatch --parsable -J="${COUNTRY}_INI_C${INI_C}_N_SIM${N_SIM}" --export=ALL --array=1-$N0 --tasks-per-node=$N_SIM -n $N_SIM -N 1 compute_initial_sample.sh)',"\n",
    "NT=$N0",
    "while [ $NT -lt $((N0 + N)) ]; do",
    "export NT",
    'PREV_JOBID=$(sbatch --parsable -d afterany:$PREV_JOBID -J="get_new_evals" --export=ALL --tasks-per-node=1 -n 1 -N 1 get_new_evals.sh)',
    'PREV_JOBID=$(sbatch --parsable -d afterok:$PREV_JOBID -J="compute_new_evals" --export=ALL --array=$((NT + 1))-$((NT + NE)) --tasks-per-node=$N_SIM -n $N_SIM -N 1 compute_new_evals.sh)',
    'PREV_JOBID=$(sbatch --parsable -d afterany:$PREV_JOBID -J="store_new_evals" --export=ALL --tasks-per-node=1 -n 1 -N 1 store_new_evals.sh)',
    "NT=$((NT+NE))",
    "done",
    fill = 1)
sink()

sink("../estimation/compute_initial_sample.sh")
cat("#!/bin/sh",
    paste("#SBATCH -t", c_time),
    "#SBATCH -o ../results/out_files/Job%A_%a",
    "#SBATCH --mem=9000",
    paste("#SBATCH -p", partition), "\n",
    "mkdir -p ../results/$COUNTRY/n_sim_$N_SIM/ini_c_$INI_C/$WEIGHTS/results/param_set_$SLURM_ARRAY_TASK_ID","\n",
    paste0(ml),
    "Rscript compute_initial_sample.R $COUNTRY $INI_YEAR $END_YEAR $INI_C $SLURM_ARRAY_TASK_ID $N_SIM $WEIGHTS",
    fill = 1)
sink()

sink("../estimation/get_new_evals.sh")
cat("#!/bin/sh",
    paste("#SBATCH -t", "02:00:00"),
    "#SBATCH -o ../results/out_files/Job%A_%a",
    "#SBATCH --mem=9000",
    paste("#SBATCH -p", partition), "\n",
    paste0(ml),
    "Rscript get_new_evals.R >> ../out_files/bo_out.Rout 2>&1 $COUNTRY $N_SIM $INI_C $WEIGHTS $INI_YEAR $END_YEAR $N0 $NE $NT","\n",
    fill = 1)
sink()

sink("../estimation/compute_new_evals.sh")
cat("#!/bin/sh",
    paste("#SBATCH -t", c_time),
    "#SBATCH -o ../results/out_files/Job%A_%a",
    "#SBATCH --mem=9000",
    paste("#SBATCH -p", partition), "\n",
    "mkdir -p ../results/$COUNTRY/n_sim_$N_SIM/ini_c_$INI_C/$WEIGHTS/results/param_set_$SLURM_ARRAY_TASK_ID","\n",
    paste0(ml),
    "Rscript compute_new_evals.R $COUNTRY $INI_YEAR $END_YEAR $INI_C $SLURM_ARRAY_TASK_ID $N_SIM $WEIGHTS $NT",
    fill = 1)
sink()

sink("../estimation/store_new_evals.sh")
cat("#!/bin/sh",
    paste("#SBATCH -t", "00:20:00"),
    "#SBATCH -o ../results/out_files/Job%A_%a",
    "#SBATCH --mem=9000",
    paste("#SBATCH -p", partition), "\n",
    paste0(ml),
    "Rscript store_new_evals.R >> ../out_files/bo_out.Rout 2>&1 $COUNTRY $N_SIM $INI_C $WEIGHTS $INI_YEAR $END_YEAR $NE $NT",
    fill = 1)
sink()


 


 




