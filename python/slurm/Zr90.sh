#!/bin/bash
#SBATCH -c 12  ## nombre de coeurs à réserver
#SBATCH -t 12-00:00:00
#SBATCH --mem-per-cpu=10G

cd ..
srun python TENDL_mp.py 12 Zr90
