#!/bin/bash -i

while read RA_Dec; do
    Rscript R_Files/"$2" $RA_Dec
done <fields.csv