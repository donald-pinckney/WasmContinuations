#!/bin/bash

# THIS RUNS GUEST-SIDE

# This function takes a command to run ($1, $cmd) and an integer number of trials to perform ($2, $numSamples).
# It runs $1 exactly $2 times, and concatenates the STDOUT of $1 each time into a string separated by ',' and started by an initial ','.
# The typical use case assumes that $1 outputs only it's own running time to STDOUT.
function sample {
    cmd=$1
    numSamples=$2

    for s in $(seq 1 $numSamples); do
        dt="$($cmd 2>&1 > /dev/null)"
        echo -n ",$dt"
    done
}

# This takes 4 arguments: 
#   the program to benchmark
#   log2(# terms in pi computation): bigger number = more computation time
#   log2(max # of concurrent threads)
#   # of samples to take 
function experiment {
    prog=$1
    termsLog2=$2
    terms=$((2**$termsLog2))
    maxThreadsLog2=$3
    numSamples=$4

    for threadsLog2 in $(seq 0 $maxThreadsLog2); do
        threads=$((2**$threadsLog2))
        dt=$(sample "$prog $terms $threads" $numSamples)
        echo "$threads$dt"
    done
}


mkdir -p bin/
rm bin/* 2> /dev/null

# Build native pi, this is now in bin/pi
go install pi 

experiment "bin/pi" $1 $2 $3


# bin/pi 1000 10

# ruby -e "puts 4+5"
