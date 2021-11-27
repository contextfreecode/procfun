let errCount = 0;

const gameDefaults = Object.freeze({ done: false, guesses: 0 });

function askGuess(high) {
  const text = prompt(`Guess a number between 1 and ${high}:`);
  return parseIntChecked(text);
}

function askGuessMulti(high) {
  while (true) {
    try {
      return askGuess(high);
    } catch {
      console.log("I didn't understand");
      errCount += 1;
    }
  }
}

function parseIntChecked(text) {
  const value = parseInt(text);
  if (value != Number(text)) throw new Error(`bad int: ${text}`);
  return value;
}

function pickAnswer(high) {
  return Math.floor(Math.random() * high) + 1;
}

function play(game) {
  while (!game.done) {
    const guess = askGuessMulti(game.high);
    report(game, guess);
    update(game, guess);
  }
}

function report(game, guess) {
  // deno-fmt-ignore
  const description =
    guess < game.answer ? "too low" :
    guess > game.answer ? "too high" :
    "the answer!";
  console.log(`${guess} is ${description}`);
}

function update(game, guess) {
  if (guess == game.answer) {
    game.done = true;
  }
  game.guesses += 1;
}

export function main() {
  const high = 100;
  const answer = pickAnswer(high);
  const game = { ...gameDefaults, answer, high };
  play(game);
  console.log(`Finished in ${game.guesses} guesses`);
  console.debug(`Total input errors: ${errCount}`);
}

main();
