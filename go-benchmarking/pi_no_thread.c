#include <stdio.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

double term(double k) {
    int sign = 2 * -((int)k % 2 ) + 1;
	return 4 * sign / (2*k + 1);
}

double pi(int numTerms) {
    double f = 0.0;
    for(int k = 0; k < numTerms; k++) {
        f += term((double)k);
    }

	return f;
}

int main(int argc, char **argv) {
    int numTerms = atoi(argv[1]);

    struct timespec tstart={0,0}, tend={0,0};
    clock_gettime(CLOCK_MONOTONIC, &tstart);
    
    printf("%f\n", pi(numTerms));

    clock_gettime(CLOCK_MONOTONIC, &tend);
    double dt = ((double)tend.tv_sec + 1.0e-9*tend.tv_nsec) - ((double)tstart.tv_sec + 1.0e-9*tstart.tv_nsec);
    long long unsigned int dt_nanos = (long long unsigned int)(1000000000 * dt);
    fprintf(stderr, "%llu\n", dt_nanos);
}




