package main

import (
	"fmt"
	"os"
	"bufio"
	"strconv"
	"strings"
	"errors"
)

type inside []struct {
	number int
	bag string
}

type bags map[string]inside

func readLine(s string) (color string, in inside, err error) {
	words := strings.Split(s, " ")
	if len(words) < 7 {
		err = errors.New("readLine: Line too short")
		return
	}

	color = strings.Join(words[0:2], " ")

	if words[4] == "no" {
		return
	}

	nbInside := len(words) / 4 - 1
	in = make(inside, nbInside)

	for i := 0; i < nbInside; i++ {
		place := (i + 1) * 4
		nb, e := strconv.Atoi(words[place])
		if e != nil {
			err = e
			return
		}
		in[i].number = nb
		in[i].bag = strings.Join(words[place + 1 : place + 3], " ")
	}

	return
}

func containsGold(color string, bs bags, cache map[string]bool) bool {
	if res, exists := cache[color]; exists {
		return res
	}

	for _, v := range bs[color] {
		if v.bag == "shiny gold" {
			return true
		}

		c := containsGold(v.bag, bs, cache)
		cache[v.bag] = c

		if c {
			return true
		}
	}

	return false
}

func bagsIn(color string, nb int, bs bags, ch chan<- int) {
	c := make(chan int)
	in := bs[color]
	for _, v := range in {
		go bagsIn(v.bag, v.number, bs, c)
	}

	sum := 0
	for i := 0; i < len(in); i++ {
		sum += <-c
	}

	ch <- (sum + 1) * nb
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	bags := make(bags)
	for scanner.Scan() {
		color, in, err := readLine(scanner.Text())
		if err != nil {
			fmt.Fprintln(os.Stderr, "Error while parsing stdin:", err)
			os.Exit(1)
		}

		bags[color] = in
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	cache := make(map[string]bool)
	sum := 0
	for color := range bags {
		if containsGold(color, bags, cache) {
			sum++
		}
	}

	fmt.Println(sum)

	ch := make(chan int)
	go bagsIn("shiny gold", 1, bags, ch)

	fmt.Println(<-ch - 1)
}
