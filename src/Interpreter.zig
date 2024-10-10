const std = @import("std");
const Token = @import("Token.zig").Token;
const Expr = @import("Expr.zig").Expr;
const Scanner = @import("Scanner.zig").Scanner;
const Parser = @import("Parser.zig").Parser;

const LoxType = union(enum) {
    nil,
    bool: bool,
    number: f64,
    string: []const u8,
};

pub const Interpreter = struct {
    str_ptrs: std.ArrayList([]const u8) = undefined,

    const Self = @This();

    pub fn init(self: *Self, allocator: std.mem.Allocator) void {
        self.str_ptrs = std.ArrayList([]const u8).init(allocator);
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.str_ptrs.items) |p| {
            allocator.free(p);
        }
        self.str_ptrs.deinit();
    }

    pub fn interpret(self: *Self, allocator: std.mem.Allocator, expr: *const Expr) !LoxType {
        switch (expr.*) {
            .literal => |literal| {
                if (literal.value) |val| {
                    switch (val) {
                        .str => return LoxType{ .string = val.str },
                        .float => return LoxType{ .number = val.float },
                    }
                } else {
                    return LoxType{ .nil = undefined };
                }
            },
            .grouping => |grouping| return try self.interpret(allocator, grouping.expression),
            .unary => |unary| {
                const right = try self.interpret(allocator, unary.right);
                switch (unary.operator.type) {
                    .MINUS => return LoxType{ .number = -right.number },
                    .BANG => return LoxType{ .bool = !isTruthy(right) },
                    else => unreachable,
                }
            },
            .binary => |binary| {
                const left = try self.interpret(allocator, binary.left);
                const right = try self.interpret(allocator, binary.right);
                switch (binary.operator.type) {
                    .MINUS => return LoxType{ .number = (left.number - right.number) },
                    .PLUS => {
                        switch (left) {
                            .number => {
                                switch (right) {
                                    .number => return LoxType{ .number = (left.number + right.number) },
                                    else => return error.NotNumber,
                                }
                            },
                            .string => {
                                switch (right) {
                                    .string => {
                                        const new_str = try std.fmt.allocPrint(allocator, "{s}{s}", .{
                                            left.string,
                                            right.string,
                                        });
                                        try self.str_ptrs.append(new_str);
                                        return LoxType{ .string = new_str };
                                    },
                                    else => return error.NotString,
                                }
                            },
                            else => return error.NotNumberOrString,
                        }
                    },
                    .SLASH => return LoxType{ .number = (left.number / right.number) },
                    .STAR => return LoxType{ .number = (left.number * right.number) },
                    .GREATER => return LoxType{ .bool = (left.number > right.number) },
                    .GREATER_EQUAL => return LoxType{ .bool = (left.number >= right.number) },
                    .LESS => return LoxType{ .bool = (left.number < right.number) },
                    .LESS_EQUAL => return LoxType{ .bool = (left.number <= right.number) },
                    .BANG_EQUAL => return LoxType{ .bool = !isEqual(left, right) },
                    .EQUAL_EQUAL => return LoxType{ .bool = isEqual(left, right) },
                    else => unreachable,
                }
            },
        }
    }

    fn isTruthy(v: LoxType) bool {
        switch (v) {
            .bool => |b| return b,
            .nil => return false,
            else => return true,
        }
    }

    fn isEqual(a: LoxType, b: LoxType) bool {
        switch (a) {
            .nil => {
                switch (b) {
                    .nil => return true,
                    else => return false,
                }
            },
            .bool => {
                switch (b) {
                    .bool => return a.bool and b.bool,
                    else => return false,
                }
            },
            .number => {
                switch (b) {
                    .number => return a.number == b.number,
                    else => return false,
                }
            },
            .string => {
                switch (b) {
                    .string => return std.mem.eql(u8, a.string, b.string),
                    else => return false,
                }
            },
        }
    }

    pub fn stringify(self: *Self, allocator: std.mem.Allocator, v: LoxType) ![]const u8 {
        _ = self;
        switch (v) {
            .nil => return try std.fmt.allocPrint(allocator, "{s}", .{"nil"}),
            .bool => return try std.fmt.allocPrint(allocator, "{any}", .{v.bool}),
            .number => return try std.fmt.allocPrint(allocator, "{d}", .{v.number}),
            .string => return try std.fmt.allocPrint(allocator, "{s}", .{v.string}),
        }
    }
};

test "interpret bool" {
    const allocator = std.testing.allocator;

    const source = "3 != 3";
    var scanner: Scanner = .{ .source = source };

    scanner.init(allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    var parser: Parser = .{ .tokens = scanner.tokens };
    defer parser.deinit(allocator);
    const expr = try parser.parse(allocator);

    var i = Interpreter{};
    i.init(allocator);
    defer i.deinit(allocator);
    const result = try i.interpret(allocator, expr);
    try std.testing.expectEqual(false, result.bool);
}

test "interpret number" {
    const allocator = std.testing.allocator;

    const source = "-1.24 * (23);";
    var scanner: Scanner = .{ .source = source };

    scanner.init(allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    var parser: Parser = .{ .tokens = scanner.tokens };
    defer parser.deinit(allocator);
    const expr = try parser.parse(allocator);

    var i = Interpreter{};
    i.init(allocator);
    defer i.deinit(allocator);
    const result = try i.interpret(allocator, expr);
    try std.testing.expectEqual(-28.52, result.number);
}

test "interpret string sum" {
    const allocator = std.testing.allocator;

    const source = "\"hello\" + \"hello\";";
    var scanner: Scanner = .{ .source = source };

    scanner.init(allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    var parser: Parser = .{ .tokens = scanner.tokens };
    defer parser.deinit(allocator);
    const expr = try parser.parse(allocator);

    var i = Interpreter{};
    i.init(allocator);
    defer i.deinit(allocator);
    const result = try i.interpret(allocator, expr);
    try std.testing.expectEqualStrings("hellohello", result.string);
}
