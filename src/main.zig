const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(gpa.deinit() == .ok) catch @panic("leak");
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2) {
        std.debug.print("Usage: twilox [script]\n", .{});
    } else if (args.len == 2) {
        try runFile(allocator, args[1]);
    } else {
        runPrompt();
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer file.close();

    const contents = try file.readToEndAlloc(allocator, 4096);
    defer allocator.free(contents);

    std.debug.print("{s}\n", .{contents});
}

fn runPrompt() void {
    std.debug.print("{s}\n", .{"prompt"});
}
