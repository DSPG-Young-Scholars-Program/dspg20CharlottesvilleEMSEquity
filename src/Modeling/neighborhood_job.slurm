#!/bin/bash
#SBATCH -c 10
#SBATCH --mem=64GB
#SBATCH -t 1-0:00:00
#SBATCH -p bii
#SBATCH -A bii_sdad_dspg
#SBATCH -o neighborhood_models_output.txt

ml gcc openmpi R/4.0.0

Rscript neighborhood_models.R
