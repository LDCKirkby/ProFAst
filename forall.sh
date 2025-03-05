#!/bin/bash -i

for file in *.*_*.*; do
    echo $file
    Rscript ProFAst/$1 $file
done
