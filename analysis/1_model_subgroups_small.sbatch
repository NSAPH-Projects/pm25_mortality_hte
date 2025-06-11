#!/bin/bash
#SBATCH -p fasse_bigmem # partition (queue)
#SBATCH -c 60 # number of cores
#SBATCH --mem=300GB # memory pool for all cores
#SBATCH -t 1:00:00 # time 
#SBATCH -o slurm-subpop-%a.out
#SBATCH --array=1,2,3,4,6,8,9,10,11,12,13,14,15,16,17,19,20,21,25,27 # small subpops

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/1_model_subgroups.R ${SLURM_ARRAY_TASK_ID}