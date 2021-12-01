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

func play(game: Game) throws -> Game {
    var next = game
    while !next.done {
        let guess = try ask_guess_multi(high: next.high)
        report(game: next, guess: guess)
        update(game: &next, guess: guess)
    }
    return next
}

func report(game: Game, guess: Int) {
    let description =
        guess < game.answer ? "too low" :
        guess > game.answer ? "too high" :
        "the answer!"
    print("\(guess) is \(description)")
}

func update(game: inout Game, guess: Int) {
    if (guess == game.answer) {
        game.done = true
    }
    game.guesses += 1
}

func main() throws {
    var rng = SystemRandomNumberGenerator()
    let high = 100
    let answer = Int.random(in: 1...100, using: &rng)
    let game = Game(answer: answer, high: high)
    let result = try play(game: game)
    print("Finished in \(result.guesses) guesses")
    print("Total input errors: \(err_count)")
}

try main()
