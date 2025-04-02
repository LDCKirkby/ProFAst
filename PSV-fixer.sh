#!/bin/bash

find . -name "*.psv" | while read file; do
    IFS='/' read -ra name <<< "$file"
    RA_DEC=${name[1]}
    filename=${name[4]}
    IFS='_' read -ra ID <<< "$filename"
    PSV=${ID[2]}
    Rscript ProFAst/Non/PSV-fixer.R $RA_DEC $PSV
done