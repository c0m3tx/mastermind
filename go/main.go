package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"math/rand"
	"os"
)

func main() {
	secret := generateSecret()
	fmt.Println("Welcome to Mastermind!")
	for att := 10; att > 0; att-- {
		fmt.Printf("You have %d attempts left. Insert your guess: ", att)
		guess, err := userInput(os.Stdin)
		if err != nil {
			if errors.Is(err, io.EOF) {
				return
			}
			fmt.Printf("Invalid input")
			att++
			continue
		}

		x := xes(guess, secret)
		o := oes(guess, secret)

		output(os.Stdout, x, o)
		if x == 4 {
			fmt.Println("Congratulations, you won!")
			return
		}
	}

	fmt.Printf("You lose! Code was %v\n", secret)
}

func xes(guess, secret []int8) int8 {
	var ct int8
	for i := range guess {
		if guess[i] == secret[i] {
			ct += 1
		}
	}

	return ct
}

func oes(guess, secret []int8) int8 {
	var ct int8
	for i, g := range guess {
		for j, s := range secret {
			if g == s && i != j {
				ct += 1
			}
		}
	}

	return ct
}

func userInput(rd io.Reader) ([]int8, error) {
	reader := bufio.NewReader(rd)
	digits := make([]int8, 0)
	str, err := reader.ReadString('\n')
	if err != nil {
		return nil, fmt.Errorf("reader error: %w", err)
	}

	for _, c := range str {
		if c >= '0' && c <= '9' {
			digits = append(digits, int8(c)-48)
		}
	}
	if len(digits) == 4 && len(dedup(digits)) == 4 {
		return digits, nil
	}
	return nil, errors.New("invalid input")
}

func output(wr io.Writer, xs, os int8) {
	res := ""
	for i := int8(0); i < xs; i++ {
		res = res + "X"
	}

	for i := int8(0); i < os; i++ {
		res = res + "O"
	}

	fmt.Fprintf(wr, "Result: %s\n", res)
}

func generateSecret() []int8 {
	values := make(map[int8]bool)
	var output [4]int8
	i := 0
	for i < 4 {
		v := int8(rand.Intn(10))
		if _, ok := values[v]; !ok {
			output[i] = int8(v)
			i++
			values[v] = true
		}
	}

	return output[:]
}

func dedup[T comparable](input []T) []T {
	values := make(map[T]bool)
	output := make([]T, 0, len(input))

	for _, v := range input {
		if _, ok := values[v]; !ok {
			output = append(output, v)
			values[v] = true
		}
	}

	return output
}
