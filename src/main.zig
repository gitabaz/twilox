const std = @import("std");

const Scanner = @import("Scanner.zig").Scanner;
const Parser = @import("Parser.zig").Parser;
const Interpreter = @import("Interpreter.zig").Interpreter;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(gpa.deinit() == .ok) catch @panic("leak");
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var lox: Lox = .{};

    if (args.len > 2) {
        std.debug.print("Usage: twilox [script]\n", .{});
    } else if (args.len == 2) {
        try lox.runFile(allocator, args[1]);
    } else {
        try lox.runPrompt(allocator);
    }
}

pub const Lox = struct {
    has_error: bool = false,

    const Self = @This();

    fn runFile(self: *Self, allocator: std.mem.Allocator, path: []const u8) !void {
        const file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
        defer file.close();

        const contents = try file.readToEndAlloc(allocator, 4096);
        defer allocator.free(contents);

        run(allocator, contents);

        if (self.has_error) {
            return error.FileError;
        }
    }

    fn run(allocator: std.mem.Allocator, contents: []const u8) void {
        var scanner = Scanner{ .source = contents };
        scanner.init(allocator);
        defer scanner.deinit();

        scanner.scanTokens() catch return;

        var parser = Parser{ .tokens = scanner.tokens };
        defer parser.deinit(allocator);
        const expr = parser.parse(allocator) catch return;

        var i = Interpreter{};
        i.init(allocator);
        defer i.deinit(allocator);

        const res = i.interpret(allocator, expr) catch return;
        const str = i.stringify(allocator, res) catch return;
        defer allocator.free(str);

        const stdout = std.io.getStdOut().writer();
        stdout.print("{s}\n", .{str}) catch return;
    }

    fn runPrompt(self: *Self, allocator: std.mem.Allocator) !void {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();

        while (true) {
            try stdout.print("# ", .{});
            const line = try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 4096);
            if (line) |contents| {
                defer allocator.free(contents);
                run(allocator, contents);
                self.has_error = false;
            } else {
                break;
            }
        }
    }

    fn report(self: *Self, line: u8, where: []const u8, message: []const u8) !void {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("[line {d}] Error{s}: {s}", .{ line, where, message });

        self.has_error = true;
    }
};
