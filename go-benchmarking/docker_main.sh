#!/bin/bash
mkdir -p bin/
rm bin/* 2> /dev/null

# Build native pi, this is now in bin/pi
go install pi 

./experiment.sh "bin/pi" $1 $2 $3


# bin/pi 1000 10

# ruby -e "puts 4+5"
