#!/bin/bash -i

for file in *; do
    	allcat=./"$file"/objectcati.csv
	if test -f "$allcat"; then
		
		echo "$allcat does exist"
		STACKED=./"$file"/stacked.rds
		
		if test -f "$STACKED"; then
			echo "$STACKED exists"
    			rm "$STACKED"
		else
			echo "$STACKED does not exist"
		fi
		echo ""
	else
		echo "$allcat does not exist"
		echo ""
	fi

done
