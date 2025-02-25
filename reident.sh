#!/bin/bash -i

for file in ./*_*.*; do
    	allcat=./"$file"/objectcati.csv
	if test -f "$allcat"; then
		echo ""		
	else
		echo "$file"
				
	fi

done
