#!/bin/bash
#SBATCH -p fasse # partition (queue)
#SBATCH -c 13 # number of cores
#SBATCH --mem=48GB # memory pool for all cores
#SBATCH -t 24:00:00 # time 
#SBATCH -o slurm-lasso-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/4_LASSO.R