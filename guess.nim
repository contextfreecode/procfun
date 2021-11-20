{.experimental: "strictEffects".}
import std/random
import std/strformat
import std/strutils

type
  Game = object
    answer: int
    done: bool
    guesses: int
    high: int

var errCount = 0

func askGuess(high: int): int {.tags: [ReadIOEffect, WriteIOEffect].} =
  {.cast(noSideEffect).}:
    stdout.write &"Guess a number between 1 and {high}: "
    stdin.readLine.parseInt

func askGuessMulti(high: int): int {.
    raises: [], tags: [ReadIOEffect, WriteIOEffect]
  .} =
  while true:
    try:
      return askGuess(high)
    except IOError, ValueError:
      {.cast(noSideEffect).}:
        echo "I didn't understand"
        errCount += 1

# function parseIntChecked(text: string | null | undefined): int {
#   const value = parseInt(text as string);
#   if (value != Number(text)) throw new Error(`bad int: ${text}`);
#   return value;
# }

proc pickAnswer(high: int): int {.tags: [Rand].} =
  rand(high - 1) + 1

# proc pickAnswer(r: var Rand, high: int): int {.noSideEffect.} =
func pickAnswer(r: var Rand, high: int): int =
  r.rand(high - 1) + 1

proc play(game: Game) {.tags: [ReadIOEffect, WriteIOEffect].} =
  while not game.done:
    let guess = askGuessMulti(game.high)
    # report(game, guess);
    # update(game, guess);
    break

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
proc main() {.tags: [ReadIOEffect, WriteIOEffect].} =
  var
    r = initRand()
  let
    high = 100
    # answer = pickAnswer(high)
    answer = pickAnswer(r, high)
    game = Game(answer: answer, done: false, guesses: 0, high: high)
  play(game)
  # console.log(`Finished in ${game.guesses} guesses`);
  # echo "Hi!"

main()

# func sneaky() =
#   var r = initRand()
#   discard pickAnswer(r, 100)


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
