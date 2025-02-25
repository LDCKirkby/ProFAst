#!/bin/bash -i

for file in *.*_*.*; do
    echo $file
    Rscript R_Files/$1 $file
done
