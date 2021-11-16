import std/num/random
// import std/os/readline

struct game
  answer: int
  guesses: int
  high: int

fun ask-guess(high: int)
  print("Guess a number between 1 and " ++ high.show ++ ": ")
  // readline()

fun pick-answer(high)
  srandom-int32-range(1.int32, high.int32).int

fun play(game)
  val guess = ask-guess(game.high)
  guess

fun main()
  val high = 100
  val answer = pick-answer(high)
  val game = Game(answer = answer, guesses = 0, high = high)
  val outcome = play(game)
  outcome
