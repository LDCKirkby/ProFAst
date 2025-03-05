#!/bin/bash -i

for file in *.*_*.*; do
    echo $file
    Rscript ProFAst/Core/$1 $file
done
