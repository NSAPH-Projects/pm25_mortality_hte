#!/bin/bash
#SBATCH -p fasse_bigmem #partition (queue)
#SBATCH -c 63 # number of cores
#SBATCH --mem=490GB # memory pool for all cores
#SBATCH -t 02:00:00 # time 
#SBATCH -o slurm-explore-data-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/2_Explore_Data.R