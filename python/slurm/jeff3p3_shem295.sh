#!/bin/bash
#SBATCH -n 1  ## nombre de coeurs à réserver
#SBATCH -t 12-00:00:00
#SBATCH --mem-per-cpu=20G

cd ..
srun python2 jeff3p3_shem295.py
