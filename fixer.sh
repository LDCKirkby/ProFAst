#!/bin/bash -i
NROW=1
FILE="done.csv"

for i in $(seq 1 $NROW)
do
    Rscript R_Files/Filter_Fixer.R $i
    ex_st=$?
done
