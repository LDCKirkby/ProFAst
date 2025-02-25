#!/bin/bash -i

while read RA_DEC; do
    Rscript ./ProFAst/Core/Detection.R $RA_DEC $2
done <$1
