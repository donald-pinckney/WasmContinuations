#include <stdio.h>

typedef void (*proc)(); 

proc bad;

int inc(int x) {
    return x + 1;
}

void set_bad(proc f) {
    bad = f;
}

void bar() {
    bad();
}