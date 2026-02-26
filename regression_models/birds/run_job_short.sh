#!/bin/bash
#SBATCH --qos=normal
#SBATCH --time=4:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=5GB

module load anaconda3
source activate renv
srun $@
