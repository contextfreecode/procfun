struct Game {
    var answer: Int
    var done: Bool = false
    var guesses: Int = 0
    var high: Int
}

var err_count = 0

enum ReadError: Error {
    case eof
    case parse
}

func ask_guess(high: Int) throws -> Int {
    print("Guess a number between 1 and \(high): ", terminator: "")
    guard let text = readLine() else {
        throw ReadError.eof
    }
    guard let guess = Int(text) else {
        throw ReadError.parse
    }
    return guess
}

func ask_guess_multi(high: Int) throws -> Int {
    while true {
        do {
            return try ask_guess(high: high)
        } catch ReadError.parse {
            print("I didn't understand")
            err_count += 1
        }
    }
}

func play(game: inout Game) throws {
    while !game.done {
        let guess = try ask_guess_multi(high: game.high)
        report(game: game, guess: guess)
        game = update(game: game, guess: guess)
    }
}

func report(game: Game, guess: Int) {
    let description =
        guess < game.answer ? "too low" :
        guess > game.answer ? "too high" :
        "the answer!"
    print("\(guess) is \(description)")
}

func update(game: Game, guess: Int) -> Game {
    var game = game
    if (guess == game.answer) {
        game.done = true
    }
    game.guesses += 1
    return game
}

func main() throws {
    var rng = SystemRandomNumberGenerator()
    let high = 100
    let answer = Int.random(in: 1...100, using: &rng)
    var game = Game(answer: answer, high: high)
    try play(game: &game)
    print("Finished in \(game.guesses) guesses")
    print("Total input errors: \(err_count)")
}

try main()
