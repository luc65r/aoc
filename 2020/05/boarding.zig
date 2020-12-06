const std = @import("std");
const warn = std.debug.warn;
const assert = std.debug.assert;

const line_size = "BFFFBBFRRR\n".len;
const nbIDs = 1 << 10;

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut().outStream();

    var ids = [_]u1{0} ** nbIDs;
    var line: [line_size]u8 = undefined;

    while (true) {
        const bytes_read = stdin.read(&line) catch |err| {
            warn("Unable to read from stdin\n", .{});
            return err;
        };

        if (bytes_read == 0)
            break;

        ids[seatID(line) catch |err| {
            warn("Unable to parse the line: {}\n", .{line});
            return err;
        }] = 1;
    }

    const largest = try largestID(ids);
    
    try stdout.print("{}\n{}\n", .{
        largest,
        try missingID(ids, largest),
    });
}

fn seatID(str: [line_size]u8) !u10 {
    var id: u10 = 0;
    for (str[0..7]) |c| {
        id = (id << 1) + switch (c) {
            'F' => @as(u10, 0),
            'B' => 1,
            else => return error.InvalidCharacter,
        };
    }
    for (str[7..10]) |c| {
        id = (id << 1) + switch (c) {
            'L' => @as(u10, 0),
            'R' => 1,
            else => return error.InvalidCharacter,
        };
    }
    return id;
}

test "seatID" {
    assert(567 == try seatID("BFFFBBFRRR\n".*));
    assert(119 == try seatID("FFFBBBFRRR\n".*));
    assert(820 == try seatID("BBFFBBFRLL\n".*));
}

fn largestID(ids: [nbIDs]u1) !u10 {
    var i: u10 = ids.len - 1;
    while (i > 0) : (i -= 1) {
        if (ids[i] == 1)
            return i;
    }
    return error.NoSeatID;
}

fn missingID(ids: [nbIDs]u1, largest: u10) !u10 {
    var i: u10 = largest;
    while (i > 0) : (i -= 1) {
        if (ids[i] == 0)
            return i;
    }
    return error.NoMissingID;
}
