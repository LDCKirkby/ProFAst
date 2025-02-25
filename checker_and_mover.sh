#!/bin/bash -i

for file in *; do
	echo $file
	for image in $file/Group_Cutouts/*; do
		if [ $(find Asteroid_Images -name $image) ]; then
			echo $image
			cp $image Asteroid_Images/
		fi
	done
done
