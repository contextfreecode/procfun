#import "Basic";
#import "Random";
#import "POSIX";

Game :: struct {
	answer: int;
	done: bool;
	guesses: int;
	high: int;
}

err_count := 0;

ask_guess :: (high: int) -> result: int, ok: bool {
	print("Guess a number between 1 and %: ", high);
    text := read_line();
	if text {
        defer free(text);
        result, ok, _ := to_integer(text);
        return result, ok;
	}
    return 0, false;
}

ask_guess_multi :: (high: int) -> int {
	while true {
        result, ok := ask_guess(high);
		if ok  return result;
		print("I didn't understand\n");
		err_count += 1;
	}
}

// Overloading allowed.
// pick_answer :: (high: string) -> int { return 0; }

pick_answer :: (high: int) -> int {
    return cast(int) (random_get() % cast(u64) high) + 1;
}

play :: (game: *Game) {
    while !game.done {
        guess := ask_guess_multi(game.high);
        report(game, guess);
        <<game = update(game, guess);
        // Could reset_temporary_storage() here.
    }
}

read_line :: () -> string {
    buffer: [4096] u8;
    // No real way to distinguish between empty and error?
    bytes_read := read(STDIN_FILENO, buffer.data, buffer.count - 1);
    // No allocation on empty. Could use tprint here if wanted.
    return sprint("%", to_string(buffer.data, bytes_read));
}

report :: (game: Game, guess: int) {
	// game.done = true;
	// print("%\n", *game);
    description: string;
    if (guess < game.answer) {
        description = "too low";
    } else if (guess > game.answer) {
        description = "too high";
    } else {
        description = "the answer!";
    }
	print("% is %\n", guess, description);
}

update :: (game: Game, guess: int) -> next: Game {
	next := game;
	next.done = guess == game.answer;
	next.guesses += 1;
	return next;
}

main :: () {
    high :: 100;
    random_seed(current_time_monotonic().low);
    answer := pick_answer(high);
    // game := Game.{answer=answer, high=high}; // For constants only right now.
    game: Game;
    game.answer = answer;
    game.high = high;
    play(*game);
	print("Finished in % guesses\n", game.guesses);
	print("Total input errors: %\n", err_count);
}
