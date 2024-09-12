#!/bin/bash
#SBATCH -p fasse_bigmem #partition (queue)
#SBATCH -c 60 # number of cores
#SBATCH --mem=400GB # memory pool for all cores
#SBATCH -t 01:00:00 # time 
#SBATCH -o slurm-calculate_prevalence-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/calculate_prevalence.R