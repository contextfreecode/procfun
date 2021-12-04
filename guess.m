function guess
    global errCount;
    errCount = 0;
    % Set up game.
    game.done = false;
    game.guesses = 0;
    game.high = 100;
    game.answer = randi(game.high);
    % Play and report.
    game = play(game);
    fprintf('Finished in %d guesses\n', game.guesses);
    fprintf('Total input errors: %d\n', errCount);
end

function guess = askGuess(high)
    prompt = sprintf('Guess a number between 1 and %d: ', high);
    guess = parseIntChecked(input(prompt, 's'));
end

function guess = askGuessMulti(high)
    while true
        try
            guess = askGuess(high);
            return;
        catch
            fprintf('I didn''t understand\n');
            global errCount;
            errCount += 1;
        end
    end
end

function value = parseIntChecked(text)
    [value, _, err] = sscanf(text, '%d');
    if ~isempty(err)
        error(sprintf('bad int: %s', text));
    end
end

function game = play(game)
    while ~game.done
        guess = askGuessMulti(game.high);
        report(game, guess);
        game = update(game, guess);
    end
end

function report(game, guess)
    if guess < game.answer
        description = 'too low';
    elseif guess > game.answer
        description = 'too high';
    else
        description = 'the answer!';
    end
    fprintf('%d is %s\n', guess, description);
end

function game = update(game, guess)
    if guess == game.answer
        game.done = true;
    end
    game.guesses += 1;
end
