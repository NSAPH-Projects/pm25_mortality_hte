#!/bin/bash
#SBATCH -p fasse_ultramem # partition (queue)
#SBATCH -c 63 # number of cores
#SBATCH --mem=2000GB # memory pool for all cores
#SBATCH -t 48:00:00 # time 
#SBATCH -o slurm-lasso-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/4_LASSO.R