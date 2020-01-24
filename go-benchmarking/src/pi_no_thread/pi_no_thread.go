// NON Concurrent computation of pi.
// See https://goo.gl/la6Kli.
//
// This demonstrates Go's ability to handle
// large numbers of concurrent processes.
// It is an unreasonable way to calculate pi.
package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"time"
)

func main() {
	if len(os.Args) != 2 {
		panic("Usage: ./pi_no_threads numTerms:int")
	}

	numTerms, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic("Usage: ./pi_no_threads numTerms:int")
	}

	start := time.Now()

	fmt.Println(pi(numTerms))

	dt := time.Since(start)

	fmt.Fprintln(os.Stderr, dt.Nanoseconds())
}

// pi launches n goroutines to compute an
// approximation of pi
func pi(numTerms int) float64 {
	f := 0.0
	for k := 0; k < numTerms; k++ {
		f += term(float64(k))
		// fmt.Println(k)
	}

	return f
}

func term(k float64) float64 {
	return 4 * math.Pow(-1, k) / (2*k + 1)
}
