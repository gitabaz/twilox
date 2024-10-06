const std = @import("std");
const Token = @import("Token.zig").Token;
const TokenType = @import("Token.zig").TokenType;
const Literal = @import("Token.zig").Literal;
const Keywords = @import("Token.zig").Keywords;

pub const Scanner = struct {
    source: []const u8 = undefined,
    tokens: std.ArrayList(Token) = undefined,
    start: u8 = 0,
    current: u8 = 0,
    line: u8 = 1,

    const Self = @This();

    pub fn init(self: *Self, allocator: std.mem.Allocator) void {
        self.tokens = std.ArrayList(Token).init(allocator);
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Self) !void {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        try self.tokens.append(Token{
            .type = .EOF,
            .lexeme = "",
            .literal = null,
            .line = self.line,
        });
        // return tokens;
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn scanToken(self: *Self) !void {
        const c: u8 = self.advance();

        switch (c) {
            '(' => try self.addToken(TokenType.LEFT_PAREN, null),
            ')' => try self.addToken(TokenType.RIGHT_PAREN, null),
            '{' => try self.addToken(TokenType.LEFT_BRACE, null),
            '}' => try self.addToken(TokenType.RIGHT_BRACE, null),
            ',' => try self.addToken(TokenType.COMMA, null),
            '.' => try self.addToken(TokenType.DOT, null),
            '-' => try self.addToken(TokenType.MINUS, null),
            '+' => try self.addToken(TokenType.PLUS, null),
            ';' => try self.addToken(TokenType.SEMICOLON, null),
            '*' => try self.addToken(TokenType.STAR, null),
            '!' => try self.addToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG, null),
            '=' => try self.addToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL, null),
            '<' => try self.addToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS, null),
            '>' => try self.addToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER, null),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                } else {
                    try self.addToken(TokenType.SLASH, null);
                }
            },
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,
            '"' => try self.string(),
            else => {
                if (std.ascii.isDigit(c)) {
                    try self.number();
                } else if (std.ascii.isAlphabetic(c)) {
                    try self.identifier();
                } else {
                    std.debug.print("unexpected char: {c}\n", .{c});
                    return error.UnexpectedCharacter;
                }
            },
        }
    }

    fn advance(self: *Self) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn string(self: *Self) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) return error.UnterminatedString;

        _ = self.advance();

        const str = self.source[(self.start + 1)..(self.current - 1)];
        try self.addToken(TokenType.STRING, Literal{ .str = str });
    }

    fn number(self: *Self) !void {
        while (std.ascii.isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance();
            while (std.ascii.isDigit(self.peek())) _ = self.advance();
        }

        try self.addToken(
            TokenType.NUMBER,
            Literal{ .float = try std.fmt.parseFloat(f64, self.source[self.start..self.current]) },
        );
    }

    fn identifier(self: *Self) !void {
        while (std.ascii.isAlphanumeric(self.peek())) _ = self.advance();

        const text = self.source[self.start..self.current];
        if (Keywords.get(text)) |token_type| {
            try self.addToken(token_type, null);
        } else {
            try self.addToken(TokenType.IDENTIFIER, null);
        }
    }

    fn addToken(self: *Self, token_type: TokenType, literal: ?Literal) !void {
        const text = self.source[self.start..self.current];
        try self.tokens.append(Token{
            .type = token_type,
            .lexeme = text,
            .literal = literal,
            .line = self.line,
        });
    }
};

test "scanner" {
    const allocator = std.testing.allocator;
    const source = "var num = 1.23";
    var scanner: Scanner = .{ .source = source };

    scanner.init(allocator);
    defer scanner.deinit();

    try scanner.scanTokens();

    var tok = scanner.tokens.items[0];
    try std.testing.expectEqual(TokenType.VAR, tok.type);
    try std.testing.expectEqualStrings("var", tok.lexeme);
    try std.testing.expectEqual(1, tok.line);
    // try std.testing.expectEqual(null, tok.literal);

    tok = scanner.tokens.items[1];
    try std.testing.expectEqual(TokenType.IDENTIFIER, tok.type);
    try std.testing.expectEqualStrings("num", tok.lexeme);
    try std.testing.expectEqual(1, tok.line);

    tok = scanner.tokens.items[2];
    try std.testing.expectEqual(TokenType.EQUAL, tok.type);
    try std.testing.expectEqualStrings("=", tok.lexeme);
    try std.testing.expectEqual(1, tok.line);

    tok = scanner.tokens.items[3];
    try std.testing.expectEqual(TokenType.NUMBER, tok.type);
    try std.testing.expectEqualStrings("1.23", tok.lexeme);
    try std.testing.expectEqual(1, tok.line);
    try std.testing.expectEqual(1.23, tok.literal.?.float);
}
