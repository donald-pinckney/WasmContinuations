#!/bin/bash

n=$1
shift
t=$({ time -p for i in `seq $n`; do $@; done 2>/dev/null 1>/dev/null ; } 2>&1 | head -1 | cut -d' ' -f2)
echo "scale=4; $t/$n" | bc
# echo $t