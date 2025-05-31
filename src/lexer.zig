const std = @import("std");
const token = @import("token.zig");

const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    pub fn init(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = 0,
        };
        lexer.readChar();
        return lexer;
    }

    pub fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn newToken(token_type: TokenType) Token {
        return Token.init(token_type, token_type.toString());
    }

    pub fn nextToken(self: *Lexer) token.Token {
        const tok = switch (self.ch) {
            '=' => newToken(TokenType.ASSIGN),
            ';' => newToken(TokenType.SEMICOLON),
            '(' => newToken(TokenType.LPAREN),
            ')' => newToken(TokenType.RPAREN),
            ',' => newToken(TokenType.COMMA),
            '+' => newToken(TokenType.PLUS),
            '{' => newToken(TokenType.LBRACE),
            '}' => newToken(TokenType.RBRACE),
            0 => Token.init(TokenType.EOF, ""),
            else => Token.init(TokenType.ILLEGAL, "?"),
        };

        self.readChar();
        return tok;
    }
};

const testing = std.testing;

test "TestNextToken" {
    const input = "=+(){},;";

    const tests = [_]struct {
        expected_type: TokenType,
        expected_literal: []const u8,
    }{
        .{ .expected_type = TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = TokenType.PLUS, .expected_literal = "+" },
        .{ .expected_type = TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = TokenType.LBRACE, .expected_literal = "{" },
        .{ .expected_type = TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = TokenType.COMMA, .expected_literal = "," },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.EOF, .expected_literal = "" },
    };

    var l = Lexer.init(input);

    for (tests) |expected| {
        const tok = l.nextToken();
        try testing.expect(tok.type == expected.expected_type);
        try testing.expectEqualStrings(expected.expected_literal, tok.literal);
    }
}
