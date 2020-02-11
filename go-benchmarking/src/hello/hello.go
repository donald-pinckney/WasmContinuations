package main

import "fmt"
import "strconv"
import "os"
// import "runtime"

func main() {
	a, _ := strconv.Atoi(os.Args[1])
	b, _ := strconv.Atoi(os.Args[2])

	// ch := make(chan float64)
	// stuff(ch, a, b)
	// go stuff(ch, a+7, b+987234)

	fmt.Println(stuff(a, b));
}

//go:noinline
func other_thing() {
	// runtime.Gosched()
}

//go:noinline
func stuff(x int, y int) float64 {
	z := x + y
	z = z + 6
	s := 0.0
	for i := 0; i < z; i++ {
		f := float64(i + i*i);
		other_thing()
		s += f;
	}
	
	return s
}