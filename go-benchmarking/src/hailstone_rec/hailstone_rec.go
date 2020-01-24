// NON Concurrent computation of pi.
// See https://goo.gl/la6Kli.
//
// This demonstrates Go's ability to handle
// large numbers of concurrent processes.
// It is an unreasonable way to calculate pi.
package main

import (
	"fmt"
	"os"
	"strconv"
	"time"
)

func main() {
	if len(os.Args) != 2 {
		panic("Usage: ./hailstone numTest:int")
	}

	numTest, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic("Usage: ./hailstone numTest:int")
	}

	start := time.Now()

	fmt.Println(hailstones(uint64(numTest)))

	dt := time.Since(start)

	fmt.Fprintln(os.Stderr, dt.Nanoseconds())
}

// pi launches n goroutines to compute an
// approximation of pi
func hailstones(numTest uint64) uint64 {
	i := uint64(0)
	for k := uint64(1); k <= numTest; k++ {
		i += hailstone(k)
	}

	return i
}

// func hailstone(k uint64) uint64 {
// 	i := uint64(0)
// 	for ; k != 1; {
// 		if k % 2 == 0 {
// 			k = k / 2
// 		} else {
// 			k = 3*k + 1
// 		}
// 		i++
// 	}
// 	return i
// }


func hailstone(k uint64) uint64 {
	if k == 1 {
		return 0
	} else if k % 2 == 0{
		return 1 + hailstone(k / 2)
	} else {
		return 1 + hailstone(3*k + 1)
	}
}