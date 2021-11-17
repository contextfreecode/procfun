import std/num/random
import readline
// import std/os/readline

struct game
  answer: int
  guesses: int
  high: int

fun ask-guess(high: int): <console, exn> int
  print("Guess a number between 1 and " ++ high.show ++ ": ")
  val line = readline()
  match line.parse-int
    Nothing -> throw("bad int: " ++ line)
    Just(n) -> n

fun pick-answer(high: int): ndet int
  srandom-int32-range(1.int32, high.int32).int

fun play(game): <console, exn> int
  val guess = ask-guess(game.high)
  println("You guessed " ++ guess.show)
  guess

fun main(): <console, exn, ndet> ()
  val high = 100
  val answer = pick-answer(high)
  val game = Game(answer = answer, guesses = 0, high = high)
  val outcome = game.play
  ()
