type int = number;

type Game = {
  answer: int;
  guesses: int;
  high: int;
};

function askGuess(high: int) {
  return prompt(`Guess a number between 1 and ${high}:`);
}

function pickAnswer(high: int) {
  return crypto.getRandomValues(new Uint32Array(1))[0] % high;
}

function play(game: Game) {
  const guess = askGuess(game.high);
  game.guesses += 1;
  // Useless ignore for now.
  guess;
}

function main() {
  const high = 100;
  const answer = pickAnswer(high);
  const game = {answer, guesses: 0, high};
  play(game);
}

main();
