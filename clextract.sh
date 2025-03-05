#!/bin/bash -i

for file in *_*.*; do
    echo $file
    touch "$file"/temp.txt
    grep "$file" $1 >> "$file"/temp.txt
    
    echo "RA,Dec,segID" > "$file"/"$file"_Verified.csv
    while read asteroid; do
       RA=$(echo $asteroid | cut -d'_' -f 1)
       Dec=$(echo $asteroid | cut -d'_' -f 2)
       PNG=$(echo $asteroid | cut -d'_' -f 3)
       SEGID=$(echo $PNG | cut -d'.' -f 1)
       echo $RA","$Dec","$SEGID >> "$file"/"$file"_Verified.csv    
    done <"$file"/temp.txt
    rm "$file"/temp.txt
done