#!/bin/bash -i

rm ./Asteroid_Images/g/
rm ./Asteroid_Images/r/
rm ./Asteroid_Images/i/

for file in *; do
	echo $file
	cp $file/Group_Cutouts/*g*.png Asteroid_Images/g/
	cp $file/Group_Cutouts/*G*.png Asteroid_Images/g/
	cp $file/Group_Cutouts/*r*.png Asteroid_Images/r/
	cp $file/Group_Cutouts/*R*.png Asteroid_Images/r/
	cp $file/Group_Cutouts/*i*.png Asteroid_Images/i/
	cp $file/Group_Cutouts/*I*.png Asteroid_Images/i/
done
