#!/bin/bash
#SBATCH --qos=medium
#SBATCH --time=48:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=40GB

module load mamba
source activate renv
srun $@
