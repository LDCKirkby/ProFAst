#!/bin/bash -i

for file in *_*.*; do
    echo $file
    touch "$file"/temp.txt
    grep "$file" $1 >> "$file"/temp.txt
    
    echo "RA,Dec,segID" > "$file"/"$file"_"$2".csv
    while read asteroid; do
       RA=$(echo $asteroid | cut -d'_' -f 1)
       Dec=$(echo $asteroid | cut -d'_' -f 2)
       PNG=$(echo $asteroid | cut -d'_' -f 3)
       SEGID=$(echo $PNG | cut -d'.' -f 1)
       echo $RA","$Dec","$SEGID >> "$file"/"$file"_"$2".csv
    done <"$file"/temp.txt
    rm "$file"/temp.txt
done