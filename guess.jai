#import "Basic";
#import "Random";

Game :: struct {
	answer: int;
	done: bool;
	guesses: int;
	high: int;
}

err_count := 0;

pick_answer :: (high: int) -> int {
    return cast(int) (random_get() % cast(u64) high) + 1;
}

play :: (game: Game) {
    while !game.done {
        break;
    }
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
