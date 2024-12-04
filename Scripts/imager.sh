#!/bin/bash -i

while read RA_DEC; do
    Rscript R_Files/Postage_Stamp_Fixer.R $RA_DEC $2
done <$1
