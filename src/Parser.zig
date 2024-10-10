const std = @import("std");
const Token = @import("Token.zig").Token;
const TokenType = @import("Token.zig").TokenType;
const Expr = @import("Expr.zig").Expr;
const Scanner = @import("Scanner.zig").Scanner;

const ParserError = error{
    OutOfMemory,
    AccessDenied,
    Unexpected,
    DiskQuota,
    FileTooBig,
    InputOutput,
    NoSpaceLeft,
    DeviceBusy,
    InvalidArgument,
    BrokenPipe,
    SystemResources,
    OperationAborted,
    NotOpenForWriting,
    LockViolation,
    WouldBlock,
    ConnectionResetByPeer,
    ParseError,
    ExpectExpression,
};

pub const Parser = struct {
    tokens: std.ArrayList(Token),
    current: usize = 0,
    expr_ptrs: std.ArrayList(*Expr) = undefined,

    const Self = @This();

    pub fn parse(self: *Self, allocator: std.mem.Allocator) ParserError!*Expr {
        self.expr_ptrs = std.ArrayList(*Expr).init(allocator);
        return try self.expression(allocator);
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.expr_ptrs.items) |ptr| {
            allocator.destroy(ptr);
        }
        self.expr_ptrs.deinit();
    }

    fn expression(self: *Self, allocator: std.mem.Allocator) ParserError!*Expr {
        return try self.equality(allocator);
    }

    fn equality(self: *Self, allocator: std.mem.Allocator) !*Expr {
        var expr = try self.comparison(allocator);
        while (self.match(&[_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator = self.previous();
            const right = try self.comparison(allocator);

            const new_expr = try allocator.create(Expr);
            try self.expr_ptrs.append(new_expr);
            new_expr.* = Expr{
                .binary = .{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }

        return expr;
    }

    fn comparison(self: *Self, allocator: std.mem.Allocator) !*Expr {
        var expr = try self.term(allocator);

        while (self.match(&[_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = self.previous();
            const right = try self.term(allocator);

            const new_expr = try allocator.create(Expr);
            try self.expr_ptrs.append(new_expr);
            new_expr.* = Expr{
                .binary = .{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }

        return expr;
    }

    fn term(self: *Self, allocator: std.mem.Allocator) !*Expr {
        var expr = try self.factor(allocator);

        while (self.match(&[_]TokenType{ .MINUS, .PLUS })) {
            const operator = self.previous();
            const right = try self.factor(allocator);

            const new_expr = try allocator.create(Expr);
            try self.expr_ptrs.append(new_expr);
            new_expr.* = Expr{
                .binary = .{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }

        return expr;
    }

    fn factor(self: *Self, allocator: std.mem.Allocator) !*Expr {
        var expr = try self.unary(allocator);

        while (self.match(&[_]TokenType{ .SLASH, .STAR })) {
            const operator = self.previous();
            const right = try self.unary(allocator);

            const new_expr = try allocator.create(Expr);
            try self.expr_ptrs.append(new_expr);
            new_expr.* = Expr{
                .binary = .{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }

        return expr;
    }

    fn unary(self: *Self, allocator: std.mem.Allocator) !*Expr {
        if (self.match(&[_]TokenType{ .BANG, .MINUS })) {
            const operator = self.previous();
            const right = try self.unary(allocator);

            const expr = try allocator.create(Expr);
            try self.expr_ptrs.append(expr);
            expr.* = Expr{
                .unary = .{
                    .operator = operator,
                    .right = right,
                },
            };
            return expr;
        }

        return try self.primary(allocator);
    }

    fn primary(self: *Self, allocator: std.mem.Allocator) !*Expr {
        if (self.match(&[_]TokenType{.FALSE})) {
            const expr = try allocator.create(Expr);
            try self.expr_ptrs.append(expr);
            expr.* = Expr{ .literal = .{ .value = .{ .str = "false" } } };
            return expr;
        }

        if (self.match(&[_]TokenType{.TRUE})) {
            const expr = try allocator.create(Expr);
            try self.expr_ptrs.append(expr);
            expr.* = Expr{ .literal = .{ .value = .{ .str = "true" } } };
            return expr;
        }

        if (self.match(&[_]TokenType{.NIL})) {
            const expr = try allocator.create(Expr);
            try self.expr_ptrs.append(expr);
            expr.* = Expr{ .literal = .{ .value = null } };
            return expr;
        }

        if (self.match(&[_]TokenType{ .NUMBER, .STRING })) {
            const expr = try allocator.create(Expr);
            try self.expr_ptrs.append(expr);
            expr.* = Expr{ .literal = .{ .value = self.previous().literal.? } };
            return expr;
        }

        if (self.match(&[_]TokenType{.LEFT_PAREN})) {
            const expr = try self.expression(allocator);
            _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");

            const expr_group = try allocator.create(Expr);
            try self.expr_ptrs.append(expr_group);
            expr_group.* = Expr{
                .grouping = .{
                    .expression = expr,
                },
            };
            return expr_group;
        }

        return error.ExpectExpression;
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) return self.advance();

        const stderr = std.io.getStdErr().writer();
        try stderr.print("{s}\n", .{message});
        return error.ParseError;
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().type == .SEMICOLON) return;

            switch (self.peek().type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
            }
            _ = self.advance();
        }
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) self.current += 1;

        return self.previous();
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().type == .EOF;
    }

    fn peek(self: *Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens.items[self.current - 1];
    }

    fn match(self: *Self, token_types: []const TokenType) bool {
        for (token_types) |tok_type| {
            if (self.check(tok_type)) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }

    fn check(self: *Self, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }
};

test "parser bool" {
    const allocator = std.testing.allocator;
    // const source = "var num = 1.23;";
    // const source = "-1.24 * (23);";
    const source = "3 != 3";
    var scanner: Scanner = .{ .source = source };

    scanner.init(allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    var parser: Parser = .{ .tokens = scanner.tokens };
    defer parser.deinit(allocator);
    var expr = try parser.parse(allocator);

    var str = std.ArrayList(u8).init(allocator);
    defer str.deinit();

    expr.print(&str);
    try std.testing.expectEqualStrings("(!= 3 3)", str.items);
}

test "parser number" {
    const allocator = std.testing.allocator;
    // const source = "var num = 1.23;";
    const source = "-1.24 * (23);";
    var scanner: Scanner = .{ .source = source };

    scanner.init(allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    var parser: Parser = .{ .tokens = scanner.tokens };
    defer parser.deinit(allocator);
    var expr = try parser.parse(allocator);

    var str = std.ArrayList(u8).init(allocator);
    defer str.deinit();

    expr.print(&str);
    try std.testing.expectEqualStrings("(* (- 1.24) (group 23))", str.items);
}
