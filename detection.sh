#!/bin/bash -i

while read RA_DEC; do
    Rscript ./R_Files/Detection.R $RA_DEC $2
done <$1
