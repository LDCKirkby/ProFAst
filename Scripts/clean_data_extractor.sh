#!/bin/bash -i

for file in *_*.*; do
    echo $file
    REAL=$(./"$file"/"$file"_real_asteroids.csv)
    touch $REAL

    grep "$file" clean-list.txt >> $REAL

    #while read asteroid; do
    #    RA=$(echo $asteroid | cut -d'_' -f 1)
    #    Dec=$(echo $asteroid | cut -d'_' -f 2)
    #    PNG=$(echo $asteroid | cut -d'_' -f 3)
    #    SEGID=$(echo $PNG | cut -d'.' -f 1)
    #    if [ $RA"_"$Dec = $file ]; then
    #        echo $RA","$Dec","$SEGID
    #        echo $RA","$Dec","$SEGID >> ./"$file"/"$file"_real_asteroids.csv    
    #    fi
    #done <clean-list.txt
done