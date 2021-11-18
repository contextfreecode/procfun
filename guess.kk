import std/num/random
import std/os/readline

struct game
  answer: int
  done: bool = False
  guesses: int = 0
  high: int

fun ask-guess(high: int): <console, exn> int
  print("Guess a number between 1 and " ++ high.show ++ ": ")
  readline().parse

fun ask-guess-multi(high: int): <console, div> int
  with brk throw-exn(exc)
    println("I didn't understand")
    ask-guess-multi(high)
  ask-guess(high)

fun parse(text: string): exn int
  match text.parse-int
    Nothing -> throw("bad int: " ++ text)
    Just(n) -> n

fun pick-answer(high: int): ndet int
  srandom-int32-range(1.int32, high.int32).int

fun play(game: game): <console, div> game
  // game.done := True
  var next := game
  while { !next.done }
    val guess = ask-guess-multi(game.high)
    game.report(guess)
    next := game.update(guess)
  next

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
  val high = 100
  val game = Game(answer = pick-answer(high), high = high)
  val outcome = game.play
  print("Finished in " ++ outcome.guesses.show ++ " guesses")
