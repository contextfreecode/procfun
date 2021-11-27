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
  // int guess;
  // std::cin >> guess;
  // std::cout << std::cin.fail() << " " << guess << "\n";
  return std::stoi(text);
}

auto ask_guess_multi(int high) noexcept -> int {
  while (true) {
    try {
      return ask_guess(high);
    } catch (const std::exception& e) {
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

// __attribute__((const))
auto play(const Game& game) noexcept -> Game {
  auto guess = ask_guess_multi(game.high);
  return game;
}

auto main() -> int {
  auto high = 100;
  auto answer = pick_answer(high);
  auto game = Game{.answer = answer, .high = high};
  game = play(game);
  // std::cout << answer << "\n";
}
