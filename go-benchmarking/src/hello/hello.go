package main

import "fmt"
import "strconv"
import "os"

func main() {
	a, _ := strconv.Atoi(os.Args[1])
	b, _ := strconv.Atoi(os.Args[2])

	ch := make(chan float64)

	fmt.Println(stuff(ch, a, b));
}

//go:noinline
func stuff(ch chan float64, x int, y int) float64 {
	z := x + y
	z = z + 6
	s := 0.0
	for i := 0; i < z; i++ {
		f := float64(i + i*i);

		s += f;
	}
	return s
}