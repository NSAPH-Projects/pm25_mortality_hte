#!/bin/bash
#SBATCH -p fasse # partition (queue)
#SBATCH -c 6 # number of cores
#SBATCH --mem=24GB # memory pool for all cores
#SBATCH -t 00:15:00 # time 
#SBATCH -o slurm-outcome-model-%j.out

module load R/4.2.2-fasrc01

Rscript --vanilla analysis/5_Outcome_Model.R