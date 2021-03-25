#!/bin/sh
# Output directory
echo "Remove previous TENDL runs in order to avoid interferences..."
rm -rf ../../output/TENDL-2019
mkdir -p ../../output/TENDL-2019
# Allow a sleep period so that SLURM has the time to accept and launch the jobs in that order
           sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Ni58  -c 20 --wrap="cd .. ; srun python2 TENDL_mp.py 20 Ni58 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Fe56  -c 16 --wrap="cd .. ; srun python2 TENDL_mp.py 16 Fe56 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Zr90  -c 16 --wrap="cd .. ; srun python2 TENDL_mp.py 16 Zr90 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Zr91  -c 12 --wrap="cd .. ; srun python2 TENDL_mp.py 12 Zr91 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Zr92  -c 12 --wrap="cd .. ; srun python2 TENDL_mp.py 12 Zr92 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Zr94  -c 12 --wrap="cd .. ; srun python2 TENDL_mp.py 12 Zr94 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Zr96  -c 12 --wrap="cd .. ; srun python2 TENDL_mp.py 12 Zr96 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Fe54  -c 12 --wrap="cd .. ; srun python2 TENDL_mp.py 12 Fe54 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cr52  -c 8  --wrap="cd .. ; srun python2 TENDL_mp.py 8  Cr52 "
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J In115 -c 8  --wrap="cd .. ; srun python2 TENDL_mp.py 8  In115"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Ag107 -c 8  --wrap="cd .. ; srun python2 TENDL_mp.py 8  Ag107"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Ag109 -c 8  --wrap="cd .. ; srun python2 TENDL_mp.py 8  Ag109"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd106 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd106"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd108 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd108"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd110 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd110"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd111 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd111"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd112 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd112"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd113 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd113"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd114 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd114"
sleep 10 ; sbatch -t 12-00:00:00 --partition="seq,dev,par_IB" --mem-per-cpu=10G -J Cd116 -c 4  --wrap="cd .. ; srun python2 TENDL_mp.py 4  Cd116"
