package main

import rnd "core:math/rand"

Game :: struct {
	answer: int,
	done: bool,
	guesses: int,
	high: int,
}

pickAnswer :: proc(high: int, rand: ^rnd.Rand) -> int {
	return rnd.int_max(high, rand) + 1
}

main :: proc() {
	high :: 100
	answer := pickAnswer(high, nil)
	game := Game { answer = answer, done = false, guesses = 0, high = high }
}
