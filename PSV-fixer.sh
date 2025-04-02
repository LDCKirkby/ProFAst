#!/bin/bash

find . -name "*.psv" | while read file; do
    IFS='/' read -ra name <<< "$file"
    RA_DEC=${name[1]}
    filename=${name[4]}
    IFS='\.' read -ra psv <<< "$filename"
    IFS='_' read -ra ID <<< ${psv[0]}
    PSV=${ID[2]}
    echo $RA_DEC $PSV
    Rscript ProFAst/Non/PSV-fixer.R $RA_DEC $PSV
done