# phylo-acoustic-rhythm
This repository contains code associated with the manuscript "Animal acoustic communication maintains a universal optimum rhythm".

Analyses were run on the University of Zurich's Science Cluster using the Slurm workload management system and the Rstan anaconda environment (https://anaconda.org/conda-forge/r-rstan).

To run everything:

```
cd OU_models
./run_models.sh

cd ..

cd regression_models
./run_all_models.sh
