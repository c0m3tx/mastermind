const std = @import("std");
const Allocator = std.mem.Allocator;
const Reader = std.io.Reader;

fn getInput(gpa: Allocator, reader: anytype) ![4]u4 {
    const input = try reader.readUntilDelimiterAlloc(gpa, '\n', 256);
    defer gpa.free(input);

    return parseInput(input);
}

fn parseInput(input: []const u8) ![4]u4 {
    var i: usize = 0;
    var output: [4]u4 = undefined;

    for (input) |ch| {
        if (ch >= '0' and ch <= '9') {
            if (i < 4) {
                output[i] = @intCast(ch - 48);
            }
            i += 1;
        }
    }

    if (i == 4) {
        return output;
    } else {
        return error.WrongDigitCount;
    }
}

fn formatOutput(x: u8, o: u8) []u8 {
    var i: usize = 0;
    var output: [4]u8 = [4]u8{ 0, 0, 0, 0 };
    for (0..x) |_| {
        output[i] = 'X';
        i += 1;
    }
    for (0..o) |_| {
        output[i] = 'O';
        i += 1;
    }
    return output[0 .. x + o];
}

test "get input" {
    const gpa = std.testing.allocator;
    const content = "1234\n";
    var buffer: [5]u8 = undefined; // Create a mutable fixed-size array
    std.mem.copyForwards(u8, &buffer, content);
    var fbs = std.io.FixedBufferStream([]u8){ .buffer = buffer[0..], .pos = 0 };
    const reader = fbs.reader();
    const input = try getInput(gpa, reader);
    try std.testing.expectEqual(input, [_]u4{ 1, 2, 3, 4 });
}

test "parse input" {
    try std.testing.expectEqual(parseInput("1234"), [_]u4{ 1, 2, 3, 4 });
    try std.testing.expectEqual(parseInput("123aaa4"), [_]u4{ 1, 2, 3, 4 });
    try std.testing.expectError(error.WrongDigitCount, parseInput("123"));
    try std.testing.expectError(error.WrongDigitCount, parseInput("12345"));
}

test "format output" {
    try std.testing.expectEqualStrings(formatOutput(3, 1), "XXXO");
    try std.testing.expectEqualStrings(formatOutput(1, 1), "XO");
    try std.testing.expectEqualStrings(formatOutput(0, 0), "");
}

pub fn say_hello() !void {
    try std.fs.File.stdout().writeAll("Hello world!\n");
}

pub fn main() void {
    say_hello() catch {};
}
