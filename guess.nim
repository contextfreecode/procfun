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

# Treat as debug info only, for logging purposes.
var errCount = 0

func askGuess(high: int): int {.
    raises: [IOError, ValueError],
    tags: [ReadIOEffect, WriteIOEffect],
  .} =
  {.cast(noSideEffect).}:
    stdout.write &"Guess a number between 1 and {high}: "
    stdin.readLine.parseInt

func askGuessMulti(high: int): int {.
    raises: [IOError],
    tags: [ReadIOEffect, WriteIOEffect],
  .} =
  while true:
    try:
      return askGuess(high)
    except ValueError:
      {.cast(noSideEffect).}:
        echo "I didn't understand"
        errCount += 1

proc pickAnswer(high: int): int {.tags: [Rand].} =
  rand(high - 1) + 1

# proc pickAnswer(r: var Rand, high: int): int {.noSideEffect.} =
func pickAnswer(r: var Rand, high: int): int =
  r.rand(high - 1) + 1

proc report(game: Game, guess: int) {.tags: [WriteIOEffect]} =
  let description =
    if guess < game.answer: "too low"
    elif guess > game.answer: "too high"
    else: "the answer!"
  echo &"{guess} is {description}"

func update(game: Game, guess: int): Game =
  result = game
  if guess == result.answer:
    result.done = true;
  result.guesses += 1

proc play(game: var Game) {.tags: [ReadIOEffect, WriteIOEffect].} =
  while not game.done:
    let guess = askGuessMulti(game.high)
    game.report(guess)
    game = game.update(guess)

# proc main() {.tags: [Rand, ReadIOEffect, WriteIOEffect].} =
proc main() {.tags: [ReadIOEffect, WriteIOEffect].} =
  var r = initRand()
  let
    high = 100
    # answer = pickAnswer(high)
    answer = r.pickAnswer(high)
  var game = Game(answer: answer, done: false, guesses: 0, high: high)
  game.play
  echo &"Finished in {game.guesses} guesses"
  stderr.writeLine &"Total input errors: {errCount}"

main()

# func sneaky() =
#   var r = initRand()
#   discard pickAnswer(r, 100)
