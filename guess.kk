import std/num/random
import std/os/readline

struct game
  answer: int
  done: bool = False
  guesses: int = 0
  high: int

fun ask-guess(high: int): <console, exn> int
  val message = "Guess a number between 1 and " ++ high.show ++ ": "
  message.prompt.parse

fun parse(text: string): exn int
  match text.parse-int
    Nothing -> throw("bad int: " ++ text)
    Just(n) -> n

fun pick-answer(high: int): ndet int
  srandom-int32-range(1.int32, high.int32).int

fun play(game): <console, div, exn> game
  val guess = ask-guess(game.high)
  game.report(guess)
  val next = game.update(guess)
  if next.done
    next
  else
    next.play

fun prompt(message: string): <console, exn> string
  message.print
  readline()

fun report(game, guess: int): console ()
  if guess < game.answer
    println(guess.show ++ " is too low")
  elif guess > game.answer
    println(guess.show ++ " is too high")
  else
    println(guess.show ++ " is the answer!")

fun update(game, guess: int): game
  val done = guess == game.answer
  game(done = done, guesses = game.guesses + 1)

fun main(): <console, div, exn, ndet> ()
  val high = 100
  val game = Game(answer = pick-answer(high), high = high)
  val outcome = game.play
  ()
