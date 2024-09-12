#!/bin/bash
#SBATCH -p fasse_ultramem # partition (queue)
#SBATCH -c 63 # number of cores
#SBATCH --mem=2000GB # memory pool for all cores (won't be enough for all subpops)
#SBATCH -t 12:00:00 # time 
#SBATCH -o slurm-subpop-%a.out
#SBATCH --array=29 # full pop

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/4_model_subpops_stratified.R ${SLURM_ARRAY_TASK_ID}