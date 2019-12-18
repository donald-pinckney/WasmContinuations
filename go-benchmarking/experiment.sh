#!/bin/bash

prog=$1
termsLog2=$2
terms=$((2**$termsLog2))
maxThreadsLog2=$3
numSamples=$4

for threadsLog2 in $(seq 0 $maxThreadsLog2); do
	threads=$((2**$threadsLog2))
	dt=$(./sample.sh "$prog $terms $threads" $numSamples)
	echo -n $threads
	echo $dt
done
