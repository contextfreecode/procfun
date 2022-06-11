package main

import "core:bufio"
import "core:fmt"
import "core:io"
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

play :: proc(game: ^Game) {
	for !game.done {
		guess := ask_guess_multi(game.high)
		report(game^, guess)
		game^ = update(game^, guess)
	}
}

read_line :: proc() -> (result: string, ok: bool) {
	s := os.stream_from_handle(os.stdin)
	r: bufio.Reader
	bufio.reader_init(&r, io.Reader{s})
	defer bufio.reader_destroy(&r)
	if line, err := bufio.reader_read_string(&r, '\n'); err == .None {
		return line[:len(line) - 1], true
	}
	return
}

report :: proc(game: Game, guess: int) {
	// game.done = true
	// fmt.println(&game)
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
	game := Game {answer = answer, high = high}
	play(&game)
	fmt.println("Finished in", game.guesses, "guesses");
	fmt.println("Total input errors:", err_count)
}
