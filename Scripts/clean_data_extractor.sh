#!/bin/bash -i

for file in ./*_*.*; do
    touch ./"$file"/"$file"_real_asteroids.txt
    touch ./"$file"/"$file"_real_asteroid_data.csv
    awk "/$file/" ./Asteroid_Images/clean-list.txt > ./"$file"/"$file"_real_asteroids.txt

    for asteroid in ./"$file"/"$file"_real_asteroids.txt; do
        RA=$(echo $asteroid | cut -d'_' -f 1)
        Dec=$(echo $asteroid | cut -d'_' -f 2)
        PNG=$(echo $asteroid | cut -d'_' -f 3)
        SEGID=$(echo $PNG | cut -d'.' -f 1)
        echo $RA","$Dec","$SEGID >> ./"$file"/"$file"_real_asteroid_data.csv    
    done    
done