const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const Game = struct {
    answer: i32,
    done: bool = false,
    guesses: i32 = 0,
    high: i32,
};

const FailingError = error{
    Eof,
    OutOfMemory,
    StreamTooLong,
} || std.os.ReadError || std.os.WriteError;

const Error = FailingError || std.fmt.ParseIntError;

var err_count: i32 = 0;

fn askGuess(high: i32) Error!i32 {
    try stdout.print("Guess a number between 1 and {}: ", .{high});
    var allocator = std.heap.page_allocator;
    const read = stdin.readUntilDelimiterOrEofAlloc;
    const text = try read(allocator, '\n', 1 << 13);
    if (text == null) return error.Eof;
    defer allocator.free(text.?);
    return std.fmt.parseInt(i32, text.?, 10);
}

fn askGuessMulti(high: i32) FailingError!i32 {
    while (true) {
        return askGuess(high) catch |err| switch (err) {
            // std.fmt.ParseIntError => {
            error.InvalidCharacter, error.Overflow => {
                try stdout.print("I didn't understand\n", .{});
                err_count += 1;
                continue;
            },
            else => @errSetCast(FailingError, err),
        };
    }
}

fn play(game: *Game) !void {
    while (!game.done) {
        const guess = try askGuessMulti(game.high);
        try report(game.*, guess);
        update(game, guess);
    }
}

fn report(game: Game, guess: i32) std.os.WriteError!void {
    // game.done = true;
    const description =
        if (guess < game.answer) "too low"
        else if (guess > game.answer) "too high"
        else "the answer!";
    try stdout.print("{} is {s}\n", .{ guess, description });
}

fn update(game: *Game, guess: i32) void {
    if (guess == game.answer) {
        game.done = true;
    }
    game.guesses += 1;
}

pub fn main() !void {
    const seed = @intCast(u64, std.time.milliTimestamp());
    var rng = std.rand.DefaultPrng.init(seed);
    var random = rng.random();
    const high = 100;
    const answer = random.intRangeAtMost(i32, 1, high);
    var game = Game{ .answer = answer, .high = high };
    try play(&game);
    try stdout.print("Finished in {} guesses\n", .{game.guesses});
    try stdout.print("Total input errors: {}\n", .{err_count});
}
