#!/bin/bash -i

while read file; do
	echo $file | cut -f2 -f3 -f4 -d '_'
	
done <$1
