#!/bin/bash -i
while read RA_DEC; do
  Rscript R_Files/Detection.R $RA_DEC Simon
done <redo.txt
