#!/bin/bash
#SBATCH -p fasse # partition (queue)
#SBATCH -c 47 # number of cores
#SBATCH --mem=180GB # memory pool for all cores
#SBATCH -t 05:00:00 # time 
#SBATCH -o slurm-merge-data-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/1_Merge_Data.R