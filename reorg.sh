#!/bin/bash -i
NROW=460
FILE="done.csv"

for i in $(seq 1 $NROW)
do
    Rscript R_Files/Data_Reorg.R $i
    ex_st=$?
done
