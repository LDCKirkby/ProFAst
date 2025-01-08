#!/bin/bash -i

for file in *_*.*; do
    echo $file
    touch "$file"/"$file"_real_asteroids.csv
    touch "$file"/temp.txt

    grep "$file" clean-list.txt >> "$file"/temp.txt

    while read asteroid; do
       RA=$(echo $asteroid | cut -d'_' -f 1)
       Dec=$(echo $asteroid | cut -d'_' -f 2)
       PNG=$(echo $asteroid | cut -d'_' -f 3)
       SEGID=$(echo $PNG | cut -d'.' -f 1)
       echo $RA","$Dec","$SEGID >> "$file"/"$file"_real_asteroids.csv    
    done <"$file"/temp.txt
    rm "$file"/temp.txt
done