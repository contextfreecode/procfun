int errCount = 0;

Main();

void Main() {
    var high = 100;
    var answer = PickAnswer(new Random(), high);
    var game = new Game { Answer = answer, High = high };
    game = Play(game);
}

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

int PickAnswer(Random random, int high) {
    return new Random().Next(high) + 1;
}

Game Play(Game game) {
    while (!game.Done) {
        var guess = AskGuessMulti(game.High);
        Report(game, guess);
    }
    return game;
}

void Report(Game game, int guess) {
    var description = guess switch {
        _ when guess < game.Answer => "too low",
        _ when guess > game.Answer => "too high",
        _ => "the answer!",
    };
    Console.WriteLine($"{guess} is {description}");
}

record struct Game {
    public int Answer;
    public bool Done = false;
    public int Guesses = 0;
    public int High;
}
