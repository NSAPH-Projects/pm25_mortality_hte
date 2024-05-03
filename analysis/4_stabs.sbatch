#!/bin/bash
#SBATCH -p fasse # partition (queue)
#SBATCH -c 17 # number of cores
#SBATCH --mem=64GB # memory pool for all cores
#SBATCH -t 04:00:00 # time 
#SBATCH -o slurm-stabs-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/4_stabs.R