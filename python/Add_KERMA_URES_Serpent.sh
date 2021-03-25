#!/bin/sh
# Add KERMA and unresolved resonance probability tables to ACE file for
# SERPENT's advanced energy deposition model. Inspired from Riku Tuominen's
# runnjoy_kermas.pl (VTT). See 'set edepmode' in SERPENT.
# Ref. : Tuominen, R., Valtavirta, V. and Leppänen, J. "New energy deposition
# treatment in the Serpent 2 Monte Carlo transport code." Ann. Nucl. Energy 129
# (2019) 224-232.
# Author : V. Salino (03/2021)
# Input parameters : paths to ACE, ENDF, PENDF and supkerma files
acefile=$1
endf=$2
pendf=$3
supkerma=$4
# Retrieve 9th line, 2nd column of ACE file
fiss=$(head -n 9 $acefile | tail -n 1 | awk {'print $2'})
# Retrieve 11th line, 7th column of ACE file
ures=$(head -n 11 $acefile | tail -n 1 | awk {'print $7'})
#
if [ "$fiss" != "0" ]; then
    # Retrieve MF1, MT458 (=' 1458') from ENDF file and add it to the ACE file
    data=$(grep -E '^.{70} 1458' $endf)
    if [ "$data" = "" ]; then
        echo 'Warning: No MT458 data for a fissionable nuclide:'$endf
    fi
    echo "$data" >> $acefile
    # Retrieve local fission kerma data (MF3, MT318 = ' 3318') from PENDF file
    # and add it to the ACE file. Also, replace '3318' with '3319' (that last
    # digit being in column 75).
    data=$(grep -E '^.{70} 3318' $pendf | sed 's/./9/75')
    if [ "$data" = "" ]; then
        echo 'Warning: No local fission kerma data:'$pendf
    fi
    echo "$data" >> $acefile
fi
# Retrieve additional non-local kerma data (MF3, MT301 = ' 3301') from PENDF
# file and add it to the ACE file.
data=$(grep -E '^.{70} 3301' $supkerma)
if [ "$data" = "" ]; then
    echo 'Warning: No non-local kerma data:'$supkerma
fi
echo "$data" >> $acefile
#
if [ "$fiss" != "0" ]; then
    # Retrieve non-local fission kerma data (MF3, MT318 = ' 3318') from an
    # additionnal PENDF file, providing additionnal KERMA data, and add it to
    # the ACE file.
    data=$(grep -E '^.{70} 3318' $supkerma)
    if [ "$data" = "" ]; then
        echo 'Warning: No non-local fission kerma data:'$supkerma
    fi
    echo "$data" >> $acefile
fi
#
if [ "$ures" != "0" ]; then
    # Retrieve additional URES data (MF2, MT153 = ' 2153') from an additionnal
    # PENDF file, providing additionnal URES data, and add it to the ACE file.
    data=$(grep -E '^.{70} 2153' $supkerma)
    if [ "$data" = "" ]; then
        echo 'Warning: No additional URES data:'$supkerma
    fi
    echo "$data" >> $acefile
fi
