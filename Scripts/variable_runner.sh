#!/bin/bash -i

while read RA_Dec; do
    Rscript R_Files/R_scripts/"$1" $RA_Dec
done <fields.csv