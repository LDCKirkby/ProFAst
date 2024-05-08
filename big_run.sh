#!/bin/bash -i
NROW=509
FILE="bearings.csv"

for i in $(seq 1 $NROW)
do
    Rscript R_Files/dezz.R $i
    ex_st=$?
    LIGMA=$(head -n $(($i+1)) $FILE | tail -n1)
    ntfy-post "Finished processing row $LIGMA. Exit status: $ex_st" "lk-silent-notifications"
done
