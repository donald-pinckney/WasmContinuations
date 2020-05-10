#include <stdio.h>

typedef void (*proc)(); 
proc bad;
void set_bad(proc f) {
    bad = f;
}

void bar() {
    printf("Starting bar\n");
    bad();
    printf("Ending bar\n"); // This is never called
}