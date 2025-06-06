#!/bin/bash
#SBATCH -p fasse_ultramem # partition (queue)
#SBATCH -c 63 # number of cores
#SBATCH --mem=2000GB # memory pool for all cores (won't be enough for all subpops)
#SBATCH -t 12:00:00 # time 
#SBATCH -o slurm-subpop-strat-%a.out
#SBATCH --array=28 # full pop

module load R/4.2.2-fasrc01

# first argument is slurm array task ID
# second argument is the variable to stratify by
stratify_by=$1

# rename the slurm output file to include user-specified stratifying variable
mv slurm-subpop-strat-${SLURM_ARRAY_TASK_ID}.out slurm-subpop-strat-${stratify_by}-${SLURM_ARRAY_TASK_ID}.out

Rscript --vanilla analysis/2_model_subgroups_stratified.R ${SLURM_ARRAY_TASK_ID} ${stratify_by}