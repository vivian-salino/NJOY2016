#!/bin/sh
# Output directory
echo "Remove previous TENDL runs in order to avoid interferences..."
rm -rf /SCRATCH/ORION/salino-viv/Njoy/TENDL
mkdir -p /SCRATCH/ORION/salino-viv/Njoy/TENDL
# Allow a sleep period so that SLURM has the time to accept and launch the jobs in that order
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 20 U238 " -c 20 -t 12-00:00:00 --mem-per-cpu=10G -J U238
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 16 U235 " -c 16 -t 12-00:00:00 --mem-per-cpu=10G -J U235
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 12 O16  " -c 12 -t 12-00:00:00 --mem-per-cpu=10G -J O16
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 12 Ni58 " -c 12 -t 12-00:00:00 --mem-per-cpu=10G -J Ni58
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 12 Zr90 " -c 12 -t 12-00:00:00 --mem-per-cpu=10G -J Zr90
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 8  Zr91 " -c 8  -t 12-00:00:00 --mem-per-cpu=10G -J Zr91
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 8  Zr92 " -c 8  -t 12-00:00:00 --mem-per-cpu=10G -J Zr92
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 8  Zr94 " -c 8  -t 12-00:00:00 --mem-per-cpu=10G -J Zr94
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 8  Zr96 " -c 8  -t 12-00:00:00 --mem-per-cpu=10G -J Zr96
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 8  Fe54 " -c 8  -t 12-00:00:00 --mem-per-cpu=10G -J Fe54
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 8  In115" -c 8  -t 12-00:00:00 --mem-per-cpu=10G -J In115
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 8  Ag107" -c 8  -t 12-00:00:00 --mem-per-cpu=10G -J Ag107
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Ag109" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Ag109
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd106" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd106
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd108" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd108
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd110" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd110
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd111" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd111
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd112" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd112
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd113" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd113
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd114" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd114
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cd116" -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cd116
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Fe56 " -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Fe56
sleep 10 ; sbatch --wrap="cd .. ; srun python2 TENDL_mp.py 2  Cr52 " -c 2  -t 12-00:00:00 --mem-per-cpu=10G -J Cr52
