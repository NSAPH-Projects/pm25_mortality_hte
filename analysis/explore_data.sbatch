#!/bin/bash
#SBATCH -p fasse_bigmem #partition (queue)
#SBATCH -c 63 # number of cores
#SBATCH --mem=490GB # memory pool for all cores
#SBATCH -t 02:00:00 # time 
#SBATCH -o slurm-explore-data-%j.out

module load R/4.2.2-fasrc01

# set R packages and rstudio server singularity image locations
my_packages=${HOME}/R/ifxrstudio/RELEASE_3_16
rstudio_singularity_image="/n/singularity_images/informatics/ifxrstudio/ifxrstudio:RELEASE_3_16.sif"

# run myscript.R using RStudio Server signularity image
singularity exec --cleanenv --env R_LIBS_USER=${my_packages} ${rstudio_singularity_image} Rscript --vanilla analysis/explore_data.R