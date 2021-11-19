{.experimental: "strictEffects".}
import std/random

type
  Game = object
    answer: int
    done: bool
    guesses: int
    high: int

var errCount = 0

# function askGuess(high: int): int {
#   const text = prompt(`Guess a number between 1 and ${high}:`);
#   return parseIntChecked(text);
# }

# function askGuessMulti(high: int): int {
#   while (true) {
#     try {
#       return askGuess(high);
#     } catch {
#       console.log("I didn't understand");
#       errCount += 1;
#     }
#   }
# }

# function parseIntChecked(text: string | null | undefined): int {
#   const value = parseInt(text as string);
#   if (value != Number(text)) throw new Error(`bad int: ${text}`);
#   return value;
# }

func pickAnswer(r: var Rand, high: int): int =
  r.rand(high - 1) + 1

proc pickAnswer(high: int): int {.tags: [Rand].} =
  rand(high - 1) + 1

# function play(game: Game) {
#   while (!game.done) {
#     const guess = askGuessMulti(game.high);
#     report(game, guess);
#     update(game, guess);
#   }
# }

# function report(game: Game, guess: int) {
#   // deno-fmt-ignore
#   const description =
#     guess < game.answer ? "too low" :
#     guess > game.answer ? "too high" :
#     "the answer!";
#   console.log(`${guess} is ${description}`);
# }

# function update(game: Game, guess: int) {
#   if (guess == game.answer) {
#     game.done = true;
#   }
#   game.guesses += 1;
# }

# proc main() {.tags: [Rand].} =
proc main() {.tags: [].} =
  var
    r = initRand()
  let
    high = 100
    # answer = pickAnswer(high)
    answer = pickAnswer(r, high)
    # game = { answer, done: false, guesses: 0, high }
  # play(game);
  # console.log(`Finished in ${game.guesses} guesses`);
  echo "Hi!"

main()


# See also: https://play.nim-lang.org/#ix=3FrC
# import random
# template funcRand(x: untyped): untyped =
#   let max = x
#   {.cast(noSideEffect).}:
#     rand(max)
# func pickAnswer(high: int): int =
#   funcRand(high - 1) + 1
# echo pickAnswer(40)
# https://play.nim-lang.org/#ix=3Frz
# func pickAnswer(high: int): int =
#   result = block:
#     {.cast(noSideEffect).}:
#       rand(high - 1)
#   result += 1
