#!/bin/bash -i

for file in *.*_*.*; do
    cat $file
    Rscript R_Files/Manual_Detection_Extractor.R $file
done
