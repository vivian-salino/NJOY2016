#!/bin/sh
# Output directory
echo "Remove previous SANDY runs in order to avoid interferences..."
rm -rf ../../output/SANDY
mkdir -p ../../output/SANDY
# Allow a sleep period so that SLURM has the time to accept and launch the jobs in that order
           sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J U235   -c 20 --wrap="cd .. ; srun python2 SANDY_mp.py 20 U235  "
sleep 20 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J U238   -c 20 --wrap="cd .. ; srun python2 SANDY_mp.py 20 U238  "
sleep 20 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J H1_H2O -c 20 --wrap="cd .. ; srun python2 SANDY_mp.py 20 H1_H2O"
sleep 20 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J H1     -c 8  --wrap="cd .. ; srun python2 SANDY_mp.py 8  H1    "
sleep 20 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J O16    -c 8  --wrap="cd .. ; srun python2 SANDY_mp.py 8  O16   "
sleep 20 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J B10    -c 8  --wrap="cd .. ; srun python2 SANDY_mp.py 8  B10   "
sleep 20 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J B11    -c 8  --wrap="cd .. ; srun python2 SANDY_mp.py 8  B11   "
