#!/bin/bash -i

for file in *.*_*.*; do
    echo $file
    Rscript R_Files/Manual_Detection_Extractor.R $file
done
