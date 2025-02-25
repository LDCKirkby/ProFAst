#!/bin/bash -i

while read RA_Dec; do
    Rscript ProFAst/"$1" $RA_Dec
done <$2