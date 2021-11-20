import std/num/random
import std/os/readline

// var err-count := 0
// val err-count = 0

effect debug
  fun inc-err(): ()

struct game
  answer: int
  done: bool = False
  guesses: int = 0
  high: int

fun ask-guess(high: int): <console, exn> int
  print("Guess a number between 1 and " ++ high.show ++ ": ")
  readline().parse-int-checked

fun ask-guess-multi(high: int): <console, debug, div> int
  with brk throw-exn(exc)
    println("I didn't understand")
    inc-err()
    ask-guess-multi(high)
  ask-guess(high)

fun parse-int-checked(text: string): exn int
  match text.parse-int
    Nothing -> throw("bad int: " ++ text)
    Just(n) -> n

fun pick-answer(high: int): ndet int
  srandom-int32-range(1.int32, high.int32).int

fun play(game: game): <console, debug, div> game
  // game.done := True
  val guess = ask-guess-multi(game.high)
  game.report(guess)
  val next = game.update(guess)
  if next.done then next else next.play

fun report(game: game, guess: int): console ()
  val description =
    if guess < game.answer then "too low"
    elif guess > game.answer then "too high"
    else "the answer!"
  println(guess.show ++ " is " ++ description)

fun update(game: game, guess: int): game  // or `total game``
  var done := False
  if guess == game.answer
    done := True
  game(done = done, guesses = game.guesses + 1)

fun main(): <console, div, ndet> ()  // or `io ()` for overkill
  // Track info for debug
  var err-count := 0
  with fun inc-err()
    err-count := err-count + 1
  // Main code
  val high = 100
  var game := Game(answer = pick-answer(high), high = high)
  game := game.play
  println("Finished in " ++ game.guesses.show ++ " guesses")
  // Report debug info (ideally to stderr or log)
  println("Total input errors: " ++ err-count.show)
