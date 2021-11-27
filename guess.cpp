#include <iostream>
#include <random>

struct Game {
  int answer;
  bool done = false;
  int guesses = 0;
  int high;
};

template <typename Generator>
auto pick_answer(Generator &gen, int high) -> int {
  return std::uniform_int_distribution<int>{1, high}(gen);
}

auto pick_answer(int high) -> int {
  std::random_device gen;
  return pick_answer(gen, high);
}

auto main() -> int {
  auto high = 100;
  auto answer = pick_answer(high);
  std::cout << answer << "\n";
  auto game = Game{.answer = answer, .high = high};
}
