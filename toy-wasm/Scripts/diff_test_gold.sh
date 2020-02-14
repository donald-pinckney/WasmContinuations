#!/bin/bash

rm -rf TestPrograms/bin/
mkdir TestPrograms/bin/

for p in TestPrograms/*.toy; do
    f=$(basename "$p" .toy)
    ./toyc "TestPrograms/$f.toy" -o "TestPrograms/bin/$f"
    ./toyc "TestPrograms/$f.toy" --heap-stack -o "TestPrograms/bin/heap_stack_$f"
    ./toyc "TestPrograms/$f.toy" -O -o "TestPrograms/bin/O_$f"
    ./toyc "TestPrograms/$f.toy" -O --heap-stack -o "TestPrograms/bin/O_heap_stack_$f"
done

 diff -w TestPrograms/gold/ TestPrograms/bin/
