#!/bin/bash
#SBATCH --qos=medium
#SBATCH --time=48:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=15GB

module load miniforge3
source activate renv
srun $@
