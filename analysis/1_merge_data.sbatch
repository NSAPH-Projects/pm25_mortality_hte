#!/bin/bash
#SBATCH -p fasse_bigmem # partition (queue)
#SBATCH -c 63 # number of cores
#SBATCH --mem=495GB # memory pool for all cores
#SBATCH -t 6:00:00 # time 
#SBATCH -o slurm-merge-data-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/1_merge_data.R