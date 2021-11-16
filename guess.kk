import std/num/random
// import std/os/readline

struct game
  answer: int
  guesses: int
  high: int

fun ask-guess(high: int): console int
  print("Guess a number between 1 and " ++ high.show ++ ": ")
  // readline()
  50

fun pick-answer(high: int): ndet int
  srandom-int32-range(1.int32, high.int32).int

fun play(game): console int
  val guess = ask-guess(game.high)
  guess

fun main(): <console, ndet> int
  val high = 100
  val answer = pick-answer(high)
  val game = Game(answer = answer, guesses = 0, high = high)
  val outcome = play(game)
  outcome
