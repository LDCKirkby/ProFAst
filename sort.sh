#!/bin/bash -i

for file in $(cat $1); do mv Asteroid_Images/"$file" Asteroid_Images/$2; done
