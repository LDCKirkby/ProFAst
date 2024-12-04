#!/bin/bash -i

for file in *; do
    Rscript R_Files/Postage_Stamp_Fixer.R $file Simon
done
