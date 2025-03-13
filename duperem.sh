#!/bin/bash -i

for RA_Dec in *_*.*; do
    echo $RA_Dec
    
    while read dupes; do
        rm $RA_Dec"/Linear_Fits/MPC_Format/*"$dupes"*"
    #   RA=$(echo $asteroid | cut -d'_' -f 1)
    #   Dec=$(echo $asteroid | cut -d'_' -f 2)
    #   PNG=$(echo $asteroid | cut -d'_' -f 3)
    #   SEGID=$(echo $PNG | cut -d'.' -f 1)
    #   echo $RA","$Dec","$SEGID >> "$file"/"$file"_"$2".csv
    done <$RA_Dec"/"$RA_Dec"_dupes.txt"
done