#!/bin/bash
#SBATCH -p fasse_ultramem # partition (queue)
#SBATCH -c 60 # number of cores
#SBATCH --mem=1400GB # memory pool for all cores
#SBATCH -t 5:00:00 # time 
#SBATCH -o slurm-subpop-strat-%a.out
#SBATCH --array=5,7,18,22,23,24,26 # medium subpops

module load R/4.2.2-fasrc01

# first argument is slurm array task ID
# second argument is the variable to stratify by
stratify_by=$1

# rename the slurm output file to include user-specified stratifying variable
mv slurm-subpop-strat-${SLURM_ARRAY_TASK_ID}.out slurm-subpop-strat-${stratify_by}-${SLURM_ARRAY_TASK_ID}.out

Rscript --vanilla analysis/2_model_subgroups_stratified.R ${SLURM_ARRAY_TASK_ID} ${stratify_by}