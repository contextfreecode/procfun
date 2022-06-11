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

ask_guess :: (high: int) -> (result: int, ok: bool) {
	print("Guess a number between 1 and %: ", high);
    result, ok := 0, false;
    text := read_line();
	if text {
        defer free(text);
        n, success, _ := to_integer(text);
        result = n;
	}
}

pick_answer :: (high: int) -> int {
    return cast(int) (random_get() % cast(u64) high) + 1;
}

play :: (game: Game) {
    while !game.done {
        guess := ask_guess(game.high);
        print("You said: %\n", guess);
        if !guess {
            break;
        }
    }
}

read_line :: () -> string {
    buffer: [4096] u8;
    // No real way to distinguish between empty and error?
    bytes_read := read(STDIN_FILENO, buffer.data, buffer.count - 1);
    // Does this also mean nothing gets allocated here?
    return sprint("%", to_string(buffer.data, bytes_read));
}

main :: () {
    high :: 100;
    random_seed(current_time_monotonic().low);
    answer := pick_answer(high);
    // game := Game.{answer=answer, high=high}; // For constants only right now.
    game: Game;
    game.answer = answer;
    game.high = high;
    play(game);
    print("A is of type %, value %.\n", type_of(answer), answer);
}
