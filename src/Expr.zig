const std = @import("std");
const Token = @import("Token.zig").Token;
const TokenType = @import("Token.zig").Token;
const TokenLiteral = @import("Token.zig").Literal;

pub const Expr = union(enum) {
    binary: Binary,
    grouping: Grouping,
    literal: Literal,
    unary: Unary,

    const Self = @This();

    pub fn print(self: Self, str: *std.ArrayList(u8)) void {
        switch (self) {
            .binary => |binary| binary.print(str),
            .grouping => |grouping| grouping.print(str),
            .literal => |literal| literal.print(str),
            .unary => |unary| unary.print(str),
        }
    }
};

const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,

    const Self = @This();

    pub fn print(self: *const Self, str: *std.ArrayList(u8)) void {
        parenthesize(str, self.operator.lexeme, self.left, self.right) catch return;
    }

    fn parenthesize(str: *std.ArrayList(u8), name: []const u8, left: *const Expr, right: *const Expr) !void {
        try str.append('(');
        try str.appendSlice(name);
        try str.append(' ');
        left.print(str);
        try str.append(' ');
        right.print(str);
        try str.append(')');
    }
};

const Grouping = struct {
    expression: *const Expr,

    const Self = @This();

    pub fn print(self: *const Self, str: *std.ArrayList(u8)) void {
        parenthesize(str, "group", self.expression) catch return;
    }

    fn parenthesize(str: *std.ArrayList(u8), name: []const u8, expr: *const Expr) !void {
        try str.append('(');
        try str.appendSlice(name);
        try str.append(' ');
        expr.print(str);
        try str.append(')');
    }
};

const Literal = struct {
    value: ?TokenLiteral,

    const Self = @This();

    pub fn print(self: *const Self, str: *std.ArrayList(u8)) void {
        if (self.value) |value| {
            switch (value) {
                .str => |v| str.appendSlice(v) catch return,
                .float => |v| str.writer().print("{d}", .{v}) catch return,
            }
        } else {
            str.appendSlice("nil") catch return;
        }
    }
};

const Unary = struct {
    operator: Token,
    right: *const Expr,

    const Self = @This();

    pub fn print(self: *const Self, str: *std.ArrayList(u8)) void {
        parenthesize(str, self.operator.lexeme, self.right) catch return;
    }

    fn parenthesize(str: *std.ArrayList(u8), name: []const u8, right: *const Expr) !void {
        try str.append('(');
        try str.appendSlice(name);
        try str.append(' ');
        right.print(str);
        try str.append(')');
    }
};

test "print" {
    const allocator = std.testing.allocator;

    var str = std.ArrayList(u8).init(allocator);
    defer str.deinit();

    const expr_unary = Expr{
        .unary = .{
            .operator = Token{
                .type = .MINUS,
                .lexeme = "-",
                .literal = null,
                .line = 1,
            },
            .right = &Expr{
                .literal = .{
                    .value = .{ .float = 123 },
                },
            },
        },
    };

    const expr_group = Expr{
        .grouping = .{
            .expression = &Expr{
                .literal = .{
                    .value = TokenLiteral{ .float = 45.67 },
                },
            },
        },
    };

    const expr_binary = Binary{
        .left = &expr_unary,
        .operator = .{
            .type = .STAR,
            .lexeme = "*",
            .literal = null,
            .line = 1,
        },
        .right = &expr_group,
    };
    expr_binary.print(&str);

    try std.testing.expectEqualStrings("(* (- 123) (group 45.67))", str.items);
}
