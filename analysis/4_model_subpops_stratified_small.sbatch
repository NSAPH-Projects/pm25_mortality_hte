#!/bin/bash
#SBATCH -p fasse_bigmem # partition (queue)
#SBATCH -c 60 # number of cores
#SBATCH --mem=300GB # memory pool for all cores (won't be enough for all subpops)
#SBATCH -t 2:00:00 # time 
#SBATCH -o slurm-subpop-strat-%a.out
#SBATCH --array=1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,25,27 # small subpops

module load R/4.2.2-fasrc01

# first argument is slurm array task ID
# second argument is the variable to stratify by
stratify_by=$1

# rename the slurm output file to include user-specified stratifying variable
mv slurm-subpop-strat-${SLURM_ARRAY_TASK_ID}.out slurm-subpop-strat-${stratify_by}-${SLURM_ARRAY_TASK_ID}.out

Rscript --vanilla analysis/4_model_subpops_stratified.R ${SLURM_ARRAY_TASK_ID} ${stratify_by}
