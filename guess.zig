const std = @import("std");
const stdout = std.io.getStdOut().writer();

pub fn main() std.os.WriteError!void {
    const seed = @mod(std.time.nanoTimestamp(), std.math.maxInt(u64));
    var rng = std.rand.DefaultPrng.init(@intCast(u64, seed));
    var random = rng.random();
    const high = 100;
    const answer = random.intRangeAtMost(i32, 1, high);
    try stdout.print("Hi: {}\n", .{answer});
}
