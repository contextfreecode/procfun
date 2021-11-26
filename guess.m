function guess
    game.done = false;
    game.guesses = 0;
    game.high = 100;
    game.answer = randi(game.high);
    fprintf("Hi!\n");
end
