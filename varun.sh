#!/bin/bash -i

while read RA_Dec; do
    Rscript ProFAst/Core/"$1" $RA_Dec
done <$2