package main

import (
    "fmt"
    "sync"
	"time"
	// "strconv"
	"os"
)

func worker(id int, wg *sync.WaitGroup) {

    fmt.Printf("Worker %d starting\n", id)

	i := 0

	for {
		time.Sleep(time.Second)
		fmt.Printf("Worker %d exec (i = %d)\n", id, i)
		i++
	}
    
	
	wg.Done()
}

func thething(x int) int {
	return x + 7
}

func main() {

    // var wg sync.WaitGroup

    // for i := 1; i <= 5; i++ {
    //     wg.Add(1)
    //     go worker(i, &wg)
	// }
	
	n := len(os.Args)


	i := n + 6
	fmt.Printf("%d", i)
	i = i + 7
	fmt.Printf("%d", i)
	i = i + 8
	fmt.Printf("%d", i)
	// i = i + 3

    // wg.Wait()
}