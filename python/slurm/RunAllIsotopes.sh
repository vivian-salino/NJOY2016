#!/bin/bash
echo "Remove previous TENDL runs in order to avoid interferences..."
rm -rf /SCRATCH/ORION/salino-viv/Njoy/TENDL/*
echo "Launching U235..."
sbatch U235.sh
sleep 10
echo "Launching U238..."
sbatch U238.sh
sleep 10
echo "Launching O16..."
sbatch O16.sh
sleep 10
echo "Launching Zr90..."
sbatch Zr90.sh
sleep 10
echo "Launching Zr91..."
sbatch Zr91.sh
sleep 10
echo "Launching Zr92..."
sbatch Zr92.sh
sleep 10
echo "Launching Zr94..."
sbatch Zr94.sh
sleep 10
echo "Launching Zr96..."
sbatch Zr96.sh
