#include <exception>
#include <iostream>
#include <random>
#include <string>

struct Game {
  int answer;
  bool done = false;
  int guesses = 0;
  int high;
};

int err_count = 0;

auto ask_guess(int high) noexcept(false) -> int {
  std::cout << "Guess a number between 1 and " << high << ": ";
  std::string text;
  std::getline(std::cin, text);
  return std::stoi(text);
}

auto ask_guess_multi(int high) noexcept -> int {
  while (true) {
    try {
      return ask_guess(high);
    } catch (const std::exception &e) {
      std::cout << "I didn't understand\n";
      err_count += 1;
    }
  }
}

template <typename Generator>
auto pick_answer(Generator &gen, int high) -> int {
  return std::uniform_int_distribution<int>{1, high}(gen);
}

auto pick_answer(int high) -> int {
  std::random_device gen;
  return pick_answer(gen, high);
}

auto report(const Game &game, int guess) -> void {
  // clang-format off
  auto description =
    guess < game.answer ? "too low" :
    guess > game.answer ? "too high" :
    "the answer!";
  // clang-format on
  std::cout << guess << " is " << description << "\n";
}

// clang-format off
constexpr
__attribute__((const))
auto update(Game game, int guess) noexcept -> Game {
  // clang-format on
  if (guess == game.answer) {
    game.done = true;
  }
  game.guesses += 1;
  return game;
}

auto play(const Game &game) -> Game {
  auto next = game;
  while (!next.done) {
    auto guess = ask_guess_multi(next.high);
    report(next, guess);
    next = update(next, guess);
  }
  return next;
}

auto main() -> int {
  auto high = 100;
  auto answer = pick_answer(high);
  auto game = Game{.answer = answer, .high = high};
  game = play(game);
	std::cout << "Finished in " << game.guesses << " guesses" << std::endl;
	std::cerr << "Total input errors: " << err_count << "\n";
}
