#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

uint64_t hailstone(uint64_t k) {
    uint64_t i = 0;
    while (k != 1) {
        if (k % 2 == 0) {
			k = k / 2;
		} else {
			k = 3*k + 1;
		}
        i++;
    }
	return i;
}

uint64_t hailstones(uint64_t numTest) {
    uint64_t i = 0;
    for (uint64_t k = 1; k <= numTest; k++) {
		i += hailstone(k);
	}
	return i;
}

int main(int argc, char **argv) {
    int numTest = atoi(argv[1]);

    struct timespec tstart={0,0}, tend={0,0};
    clock_gettime(CLOCK_MONOTONIC, &tstart);
    
    printf("%llu\n", hailstones(numTest));

    clock_gettime(CLOCK_MONOTONIC, &tend);
    double dt = ((double)tend.tv_sec + 1.0e-9*tend.tv_nsec) - ((double)tstart.tv_sec + 1.0e-9*tstart.tv_nsec);
    long long unsigned int dt_nanos = (long long unsigned int)(1000000000 * dt);
    fprintf(stderr, "%llu\n", dt_nanos);
}
