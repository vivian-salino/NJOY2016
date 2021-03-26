#!/bin/sh

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
for xsdir_individual in $(find -L ../output/*/* -name "*.xsdir")
do
#   Change default "filename" (3rd column) and replace "route" with a zero (4th column)
    xsdirline=$(awk -F '[[:space:]]+' '{print $2, $3, FILENAME, "0", $6, $7, $8, $9, $10, $11}' $xsdir_individual)
#   xsdir file shall point toward ace filename
    xsdirline=$(sed 's/\.xsdir/.ace/' <<< $xsdirline)
#   For the continuous-energy (non-TSL) ace files, capture the eventual TSL name
#   In every other cases, tsl variable should be empty
    tsl=$(awk -F/ '{if ($0 !~ /tsl/) print $NF}' <<< $xsdirline)
    tsl=$(echo $tsl | cut -d"_" -f1 | sed 's/^[0-9]\+//g' )
#   Capture random sampling number in xsdir path
    lastdir=$(echo $xsdirline | rev | cut -d/ -f2 | rev)
    if [[ $lastdir =~ _[0-9]{3} ]]
    then
        # If the directory's name containing the ACE file ends up with "_ddd" (3 digits), capture "ddd"
        rand=$(echo $lastdir | rev | cut -d_ -f1 | rev )
    else
        # Otherwise, it's not considered to be a randomly sampled ACE file
        rand=""
    fi
    xsdirline=$(awk '{
#   Dealing with metastable isotopes: if ace filenames contains a 3 in third position, then add a "m" after the ZAID identifier
    if ($3 ~ /\/([0-9])[0-9]3[0-9][0-9][^/]*.ace$/)
        sub("\\.","m.",$1);
#   Dealing with continuous-energy ace that are associated to TSL files
    sub("\\.","'$tsl'.",$1);
#   Dealing with TENDL random sampling
    if ($3 ~ /TENDL/)
        sub("\\.","_'$rand'.",$1);
#   Dealing with SANDY random sampling
    if ($3 ~ /SANDY/)
        sub("\\.","_'$rand'.",$1);
    print $0
    }' <<< $xsdirline)
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
# Adjust filepaths so that Serpent can find the ACE files
sed -i 's/ \.\/\.\.\// ..\/PyNjoy2016\//' $xsdata
