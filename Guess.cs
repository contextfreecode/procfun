int errCount = 0;

Main();

int AskGuess(int high) {
    Console.Write($"Guess a number between 1 and {high}: ");
    var text = Console.ReadLine();
    if (text == null) {
        throw new EndOfStreamException();
    }
    return int.Parse(text);
}

int AskGuessMulti(int high) {
    while (true) {
        try {
            return AskGuess(high);
        } catch (FormatException) {
            Console.WriteLine("I didn't understand");
            errCount += 1;
        }
    }
}

void PickAnswer(Random random, int high, out int answer) {
    // Inherently mutable random.
    answer = random.Next(high) + 1;
}

void Play(ref Game game) {
    while (!game.Done) {
        var guess = AskGuessMulti(game.High);
        Report(game, guess);
        game = Update(game, guess);
    }
}

void Report(Game game, int guess) {
    var description = guess switch {
        _ when guess < game.Answer => "too low",
        _ when guess > game.Answer => "too high",
        _ => "the answer!",
    };
    Console.WriteLine($"{guess} is {description}");
}

Game Update(Game game, int guess) {
    if (guess == game.Answer) {
        game.Done = true;
    }
    game.Guesses += 1;
    return game;
}

void Main() {
    var high = 100;
    int answer;
    PickAnswer(new Random(), high, out answer);
    var game = new Game { Answer = answer, High = high };
    // game = Play(game);
    Play(ref game);
    Console.WriteLine($"Finished in {game.Guesses} guesses");
    Console.WriteLine($"Total input errors: {errCount}");
}

record struct Game {
    public int Answer;
    public bool Done = false;
    public int Guesses = 0;
    public int High;
}
