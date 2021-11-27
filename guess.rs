use rand::Rng;

#[derive(Clone, Copy)]
struct Game {
    answer: i32,
    done: bool,
    guesses: i32,
    high: i32,
}

fn play(game: Game) -> Game {
    game
}

fn main() {
    let high = 100;
    let mut rng = rand::thread_rng();
    let answer = rng.gen_range(1..=high);
    let game = Game { answer, done: false, guesses: 0, high };
    let game = play(game);
}
