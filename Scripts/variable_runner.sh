#!/bin/bash -i

while read RA_Dec; do
    Rscript R_Files/Rscripts/"$1" $RA_Dec
done <fields.csv