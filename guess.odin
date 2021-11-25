package main

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:time"
import "core:math/rand"

Game :: struct {
	answer: int,
	done: bool,
	guesses: int,
	high: int,
}

err_count := 0

ask_guess :: proc(high: int) -> (result: int, ok: bool) {
	fmt.printf("Guess a number between 1 and %d: ", high)
	if text, ok := read_line(); ok {
		defer mem.delete(text)
		return strconv.parse_int(s = text, base = 10)
	}
	return
}

ask_guess_multi :: proc(high: int) -> int {
	for {
		if result, ok := ask_guess(high); ok {
			return result
		}
		fmt.println("I didn't understand")
		err_count += 1
	}
}

pick_answer :: proc(high: int, r: ^rand.Rand) -> int {
	return rand.int_max(high, r) + 1
}

play :: proc(game: Game) -> (next: Game) {
	// game.done = true
	// fmt.println(&game)
	next = game
	for !next.done {
		guess := ask_guess_multi(next.high)
		report(next, guess)
		next = update(next, guess)
	}
	return
}

read_line :: proc() -> (result: string, ok: bool) {
	if buffer, err := mem.make([]u8, 1 << 13); err == .None {
		if n, err := os.read(os.stdin, buffer[:]); err == os.ERROR_NONE {
			return cast(string)buffer[:n - 1], true
		}
		mem.delete(buffer)
	}
	return
}

report :: proc(game: Game, guess: int) {
	description := (
		"too low" if guess < game.answer else
		"too high" if guess > game.answer else
		"the answer!"
	)
	fmt.println(guess, "is", description)
}

update :: proc(game: Game, guess: int) -> (next: Game) {
	next = game
	next.done = guess == game.answer
	next.guesses += 1
	return
}

main :: proc() {
	high :: 100
	r := rand.create(transmute(u64)time.now())
	// Or use nil for default random.
	answer := pick_answer(high, &r)
	game := Game {answer = answer, done = false, guesses = 0, high = high}
	game = play(game)
	fmt.println("Finished in", game.guesses, "guesses");
	fmt.println("Total input errors:", err_count)
}
