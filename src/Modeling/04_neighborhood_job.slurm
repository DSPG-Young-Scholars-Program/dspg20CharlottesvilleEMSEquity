#!/bin/bash
#SBATCH -c 10
#SBATCH --mem=64GB
#SBATCH -t 1-0:00:00
#SBATCH -p bii
#SBATCH -A bii_sdad_dspg
#SBATCH -o neighborhood_models_output.txt

module load lubridate
module load gcc/7.1.0 openmpi/3.1.4 R/4.0.0

Rscript neighborhood_models.R
