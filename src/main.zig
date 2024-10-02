const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(gpa.deinit() == .ok) catch @panic("leak");
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2) {
        std.debug.print("Usage: twilox [script]", .{});
    } else if (args.len == 2) {
        try runFile(allocator, args[1]);
    } else {
        runPrompt();
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    std.debug.print("{s}\n", .{path});
    const file = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
    const contents = try file.readToEndAlloc(allocator, 4096);
    std.debug.print("{s}\n", .{contents});
    defer allocator.free(contents);
}

fn runPrompt() void {
    std.debug.print("{s}\n", .{"prompt"});
}
