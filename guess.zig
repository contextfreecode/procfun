const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const Game = struct {
    answer: i32,
    done: bool = false,
    guesses: i32 = 0,
    high: i32,
};

const Error = error{
    OutOfMemory,
    StreamTooLong,
} || std.os.ReadError || std.os.WriteError;

fn askGuess(high: i32) Error!i32 {
    try stdout.print("Guess a number between 1 and {}: ", .{high});
    var allocator = std.heap.page_allocator;
    const text = try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 1 << 13);
    try stdout.print("text: {s}\n", .{text});
    // return std.fmt.parseInt(i32, text, 10);
    return 50;
}

fn play(game: Game) Error!Game {
    const guess = try askGuess(game.high);
    try stdout.print("guess: {}\n", .{guess});
    return game;
}

pub fn main() Error!void {
    const seed = @intCast(u64, std.time.milliTimestamp());
    var rng = std.rand.DefaultPrng.init(seed);
    var random = rng.random();
    const high = 100;
    const answer = random.intRangeAtMost(i32, 1, high);
    const game = Game{ .answer = answer, .high = high };
    const result = play(game);
    try stdout.print("Hi: {}\n", .{result});
}
