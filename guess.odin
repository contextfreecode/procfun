package main

import "core:fmt"
import "core:time"
import "core:math/rand"

Game :: struct {
	answer: int,
	done: bool,
	guesses: int,
	high: int,
}

pickAnswer :: proc(high: int, r: ^rand.Rand) -> int {
	return rand.int_max(high, r) + 1
}

main :: proc() {
	high :: 100
	r := rand.create(transmute(u64)time.now())
	// Or use nil for default random.
	answer := pickAnswer(high, &r)
	game := Game {answer = answer, done = false, guesses = 0, high = high}
	fmt.println(answer)
}
