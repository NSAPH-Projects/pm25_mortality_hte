#!/bin/bash
#SBATCH -p fasse_ultramem # partition (queue)
#SBATCH -c 60 # number of cores
#SBATCH --mem=1000GB # memory pool for all cores
#SBATCH -t 5:00:00 # time 
#SBATCH -o slurm-subpop-%a.out
#SBATCH --array=5,18,22,23,24,26 # medium subpops

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/1_model_subgroups.R ${SLURM_ARRAY_TASK_ID}