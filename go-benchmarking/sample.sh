#!/bin/bash
cmd=$1
numSamples=$2

for s in $(seq 1 $numSamples); do
	dt="$($cmd 2>&1 > /dev/null)"
	echo -n ",$dt"
done
