// Concurrent computation of pi.
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
	if len(os.Args) != 3 {
		panic("Usage: ./pi numTerms:int numThreads:int")
	}
	
	numTerms, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic("Usage: ./pi numTerms:int numThreads:int")
	}
	numThreads, err := strconv.Atoi(os.Args[2])
	if err != nil {
		panic("Usage: ./pi numTerms:int numThreads:int")
	}

	start := time.Now()

	fmt.Println(pi(numTerms, numThreads))

	dt := time.Since(start)

	fmt.Fprintln(os.Stderr, dt.Nanoseconds())
}

// pi launches n goroutines to compute an
// approximation of pi.
func pi(numTerms int, numThreads int) float64 {
	if numTerms % numThreads != 0 {
		panic("Error, numTerms must be divisible by numThreads")
	}

	ch := make(chan float64)

	termsPerThread := numTerms / numThreads

	for k := 0; k < numThreads; k++ {
		go terms(ch, k, k*termsPerThread, termsPerThread+k*termsPerThread-1)
	}

	f := 0.0
	for k := 0; k < numThreads; k++ {
		f += <-ch
	}
	return f
}

func terms(ch chan float64, threadId int, from int, to int) {
	// fmt.Printf("thread %d: terms %d to %d\n", threadId, from, to)
	f := 0.0
	for k := from; k <= to; k++ {
		f += term(float64(k))
	}
	ch <- f
}


func term(k float64) float64 {
	return 4 * math.Pow(-1, k) / (2*k + 1)
}



// min returns the smaller of x or y.
func min(x, y int) int {
    if x > y {
        return y
    }
    return x
}
