#!/bin/bash

# THIS RUNS HOST-SIDE

# Arguments:
# 1. log2 number of terms of pi series to compute (i.e. the total work to do)
# 2. log2 of the max number of threads to test
# 3. how many samples to take for each

docker build -t my-golang-app . > /dev/null
docker run -it --rm --cpuset-cpus="0" my-golang-app ./docker_main.sh $1 $2 $3 $4
# docker rmi my-golang-app > /dev/null