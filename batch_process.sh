#!/bin/bash
path=$1
for file in $path/*; do
    python3 extract_solar_images.py -i $file -a
done
