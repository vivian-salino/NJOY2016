#!/bin/bash

# File to which xsdir and xsdata information will be stored
xsdir=../output/Universal.xsdir
xsdata=../output/Universal.xsdata
# Remove anything that may remain from previous executions
rm -f $xsdir $xsdata

#---
# Produce xsdir
#---
echo "datapath=."            > $xsdir
echo "atomic weight ratios" >> $xsdir
echo "directory"            >> $xsdir
for xsdir_individual in $(find ../output/*/*.xsdir)
#for xsdir_individual in $(find ../output/*/*tsl*.xsdir -name "*.xsdir")
#for xsdir_individual in $(find ../output/*/61348_900K.xsdir -name "*.xsdir")
#for xsdir_individual in $(find ../output/*/1001lwtr_294K*.xsdir -name "*.xsdir")
do
    # ACE path in the same as XSDIR path, only substituting filename extension
    ACE_path=$(echo $xsdir_individual | sed 's/.xsdir/.ace/')
    # Retrieve ZAID in the XSDIR filename (i.e. in "ZAID_temperature.xsdir",
    # before "_"), where the 3rd digit is equal to 3 for metastable/isomeric
    # state (MCNP convention). Also, non-digits can be present and represent
    # TSL name (lwtr, ...) for both the TSL file itself (e.g. ".02t") and the
    # associated ACE (continuous-energy, e.g. ".02c") file.
    ZAID=$(basename $xsdir_individual | cut -d"_" -f1)
    # For TSL files (e.g. "02t"), keep only its name/letters and not the ZAID numbers
    if [[ "$xsdir_individual" == *"tsl"* ]]; then
        ZAID=$(echo $ZAID | sed 's/[0-9]//g')
    fi
    # Capture random sampling number in xsdir path
    lastdir=$(dirname $xsdir_individual | rev | cut -d"/" -f1 | rev)
    if [[ $lastdir =~ _[0-9]{3} ]]
    then
        # If the directory's name containing the ACE file ends up with "_ddd" (3 digits), capture "ddd"
        rand="_"$(echo $lastdir | rev | cut -d_ -f1 | rev )
    else
        # Otherwise, it's not considered to be a randomly sampled ACE file
        rand=""
    fi
    # Retrieve temperature suffix (such as "09c" or "02t") in the first column of XSDIR
    suffix=$(cat $xsdir_individual | awk '{print $1}' | cut -d"." -f2)
    # First XSDIR field shall be : ZAID(_rand).suffix
    First_Field=$ZAID$rand"."$suffix
    xsdirline=$(awk -F '[[:space:]]+' '{print "'$First_Field'", $3, "'$ACE_path'", "0", $6, $7, $8, $9, $10, $11}' $xsdir_individual)
    # Write to output file
    echo $xsdirline >> $xsdir
    # Change accordingly the first line of the ace file to match the (eventually new) xsdir identifier
    acefile=$(awk '{print $3}' <<< $xsdirline)
    Identifier_ace=$(head -n 1 $acefile | awk '{print $1}')
    Identifier_xsdir=$(awk '{print $1}' <<< $xsdirline)
    echo $Identifier_xsdir
    echo $Identifier_ace
    if [ $Identifier_xsdir != $Identifier_ace ]
    then
        echo "Currently on top of ace file:"
        head -n 1 $acefile
        head -n 1 $acefile | awk -F '[[:space:]]+' '{$2="'$Identifier_xsdir'"; print $0}' > $acefile.tmp
        tail -n +2 $acefile >> $acefile.tmp
        mv $acefile.tmp $acefile
    fi
done

#---
# Produce xsdata for Serpent
#---
./xsdirconvert.pl $xsdir >> $xsdata
# In a Serpent input, not only does the user have to point to the xsdata file, but also each line of the
# xsdata has to point to the ACE files, not starting from the xsdata file itself, but from the same point.
# We therefore adjust the paths for Serpent.
sed -i 's| ./../output/| ../../PyNjoy2016/output/|' $xsdata
