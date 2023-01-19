#!/bin/bash
# Download wave energy data
# @brunj7
# https://data.openei.org/s3_viewer?bucket=wpto-pds-us-wave&prefix=v1.0.0%2FWest_Coast%2F
for y in `seq 1 9`;
do
	echo "Downloading year 200$y"
	wget https://wpto-pds-us-wave.s3.amazonaws.com/v1.0.0/West_Coast/West_Coast_wave_200$y.h5
done

