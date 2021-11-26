import java.util.Random;
import static java.lang.System.*;

class Guess {
    static int pickAnswer(Random random, int high) {
        return random.nextInt(high) + 1;
    }

    public static void main(String[] args) {
        var high = 100;
        var answer = pickAnswer(new Random(), high);
        var game = new Game(answer, high);
        game.play();
        out.printf("Finished in %s guesses\n", game.guesses());
        err.printf("Total input errors: %s\n", game.errCount());
    }
}

class Game {
    private int answer;
    private boolean done;
    private int errCount;
    private int guesses;
    private int high;

    public Game(int answer, int high) {
        this.answer = answer;
        this.high = high;
    }

    int askGuess() throws NumberFormatException {
        out.printf("Guess a number between 1 and %s: ", high);
        return Integer.parseInt(console().readLine());
    }

    int askGuessMulti() {
        while (true) {
            try {
                return askGuess();
            } catch (NumberFormatException e) {
                out.println("I didn't understand");
                errCount += 1;
            }
        }
    }

    int errCount() {
        return errCount;
    }

    public int guesses() {
        return guesses;
    }

    public void play() {
        while (!done) {
            var guess = askGuessMulti();
            report(guess);
            update(guess);
        }
    }

    void report(int guess) {
        var description =
            guess < answer ? "too low" :
            guess > answer ? "too high" :
            "the answer!";
        out.printf("%s is %s\n", guess, description);
    }

    void update(int guess) {
        if (guess == answer) {
            done = true;
        }
        guesses += 1;
    }
}

// record Game(
//     int answer,
//     boolean done,
//     int guesses,
//     int high
// ) {
//     public Game(int answer, int high) {
//         this(answer, false, 0, high);
//     }
// }
