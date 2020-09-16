#!/bin/bash
#SBATCH -c 20  ## nombre de coeurs à réserver
#SBATCH -t 12-00:00:00
#SBATCH --mem-per-cpu=10G

cd ..
srun python2 TENDL_mp.py 20 U235
