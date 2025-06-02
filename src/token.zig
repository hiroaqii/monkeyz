const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    // 識別子 + リテラル
    IDENT, // add, foobar, x, y, ...
    INT, // 1343456

    // 演算子
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,

    // デリミタ
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // キーワード
    FUNCTION,
    LET,

    pub fn toString(self: TokenType) []const u8 {
        return switch (self) {
            .ILLEGAL => "ILLEGAL",
            .EOF => "EOF",
            .IDENT => "IDENT",
            .INT => "INT",
            .ASSIGN => "=",
            .PLUS => "+",
            .MINUS => "-",
            .BANG => "!",
            .ASTERISK => "*",
            .SLASH => "/",
            .LT => "<",
            .GT => ">",
            .COMMA => ",",
            .SEMICOLON => ";",
            .LPAREN => "(",
            .RPAREN => ")",
            .LBRACE => "{",
            .RBRACE => "}",
            .FUNCTION => "FUNCTION",
            .LET => "LET",
        };
    }
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    pub fn init(token_type: TokenType, literal: []const u8) Token {
        return Token{
            .type = token_type,
            .literal = literal,
        };
    }
};

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "fn", TokenType.FUNCTION },
    .{ "let", TokenType.LET },
});

pub fn lookupIdent(ident: []const u8) TokenType {
    return keywords.get(ident) orelse TokenType.IDENT;
}

// test
test "Test Lookup Ident" {
    const testing = std.testing;

    try testing.expectEqual(TokenType.FUNCTION, lookupIdent("fn"));
    try testing.expectEqual(TokenType.LET, lookupIdent("let"));
    try testing.expectEqual(TokenType.IDENT, lookupIdent("foobar"));
    try testing.expectEqual(TokenType.IDENT, lookupIdent("x"));
}
