#!/bin/bash

echo "Building ./toyc"
idris --build toy.ipkg

echo "Building test programs"

rm -f TestPrograms/gold/*

for p in TestPrograms/*.toy; do
    f=$(basename "$p" .toy)
    ./toyc "TestPrograms/$f.toy" -o "TestPrograms/gold/$f"
    ./toyc "TestPrograms/$f.toy" --heap-stack -o "TestPrograms/gold/heap_stack_$f"
    ./toyc "TestPrograms/$f.toy" -O -o "TestPrograms/gold/O_$f"
    ./toyc "TestPrograms/$f.toy" -O --heap-stack -o "TestPrograms/gold/O_heap_stack_$f"
done
