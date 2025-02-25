#!/bin/bash -i

while read RA_Dec; do
    Rscript R_Files/"$1" $RA_Dec
done <$2