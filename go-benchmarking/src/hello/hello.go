package main

import "fmt"
import "strconv"
import "os"

func main() {
	a, _ := strconv.Atoi(os.Args[1])
	b, _ := strconv.Atoi(os.Args[2])
	fmt.Println(stuff(a, b));
}

//go:noinline
func stuff(x int, y int) float64 {
	z := x + y
	z = z + 6
	zd := float64(z)
	return zd
}