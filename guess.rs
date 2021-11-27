use rand::Rng;
use std::error::Error;
use std::io::{stdin, stdout, Write};

#[derive(Clone, Copy)]
struct Game {
    answer: i32,
    done: bool,
    guesses: i32,
    high: i32,
}

static mut ERR_COUNT: i32 = 0;

fn ask_guess(high: i32) -> Result<i32, Box<dyn Error>> {
    print!("Guess a number between 1 and {}: ", high);
    stdout().flush()?;
    let mut line = String::new();
    stdin().read_line(&mut line)?;
    line.pop(); // Sloppy nix newline.
    Ok(line.parse()?)
}

fn ask_guess_multi(high: i32) -> i32 {
    loop {
        match ask_guess(high) {
            Ok(guess) => return guess,
            Err(_) => {
                println!("I didn't understand");
                unsafe {
                    ERR_COUNT += 1;
                }
            }
        }
    }
}

fn play(mut game: Game) -> Game {
    while !game.done {
        let guess = ask_guess_multi(game.high);
        report(&game, guess);
        // Mutate just to prove we can.
        update(&mut game, guess);
    }
    game
}

fn report(game: &Game, guess: i32) {
    let description = if guess < game.answer {
        "too low"
    } else if guess > game.answer {
        "too high"
    } else {
        "the answer"
    };
    println!("{} is {}", guess, description);
}

fn update(game: &mut Game, guess: i32) {
    if guess == game.answer {
        game.done = true;
    }
    game.guesses += 1;
}

fn main() {
    let high = 100;
    let mut rng = rand::thread_rng();
    let answer = rng.gen_range(1..=high);
    let game = Game {
        answer,
        done: false,
        guesses: 0,
        high,
    };
    let game = play(game);
	println!("Finished in {} guesses", game.guesses);
    unsafe {
	    println!("Total input errors: {}", ERR_COUNT);
    }
}
