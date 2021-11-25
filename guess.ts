type int = number;

type Game = {
  answer: int;
  done: boolean;
  guesses: int;
  high: int;
};

let errCount = 0;

function askGuess(high: int): int {
  const text = prompt(`Guess a number between 1 and ${high}:`);
  return parseIntChecked(text);
}

function askGuessMulti(high: int): int {
  while (true) {
    try {
      return askGuess(high);
    } catch {
      console.log("I didn't understand");
      errCount += 1;
    }
  }
}

function parseIntChecked(text: string | null | undefined): int {
  const value = parseInt(text as string);
  if (value != Number(text)) throw new Error(`bad int: ${text}`);
  return value;
}

function pickAnswer(high: int): int {
  return Math.floor(Math.random() * high) + 1;
}

function play(game: Game) {
  while (!game.done) {
    const guess = askGuessMulti(game.high);
    report(game, guess);
    update(game, guess);
  }
}

function report(game: Game, guess: int) {
  // deno-fmt-ignore
  const description =
    guess < game.answer ? "too low" :
    guess > game.answer ? "too high" :
    "the answer!";
  console.log(`${guess} is ${description}`);
}

function update(game: Game, guess: int) {
  if (guess == game.answer) {
    game.done = true;
  }
  game.guesses += 1;
}

export function main() {
  const high = 100;
  const answer = pickAnswer(high);
  const game = { answer, done: false, guesses: 0, high };
  play(game);
  console.log(`Finished in ${game.guesses} guesses`);
  console.debug(`Total input errors: ${errCount}`)
}

main();
