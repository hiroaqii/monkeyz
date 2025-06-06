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

    // 比較演算子
    EQ,
    NOT_EQ,

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
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

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
            .EQ => "==",
            .NOT_EQ => "!=",
            .COMMA => ",",
            .SEMICOLON => ";",
            .LPAREN => "(",
            .RPAREN => ")",
            .LBRACE => "{",
            .RBRACE => "}",
            .FUNCTION => "FUNCTION",
            .LET => "LET",
            .TRUE => "TRUE",
            .FALSE => "FALSE",
            .IF => "IF",
            .ELSE => "ELSE",
            .RETURN => "RETURN",
        };
    }

    pub fn hasFixedLiteral(self: TokenType) bool {
        return switch (self) {
            .IDENT, .INT, .EOF => false,
            else => true,
        };
    }
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    pub fn initAlnum(token_type: TokenType, literal: []const u8) Token {
        return Token{
            .type = token_type,
            .literal = literal,
        };
    }

    pub fn init(comptime token_type: TokenType) Token {
        if (!comptime token_type.hasFixedLiteral()) {
            @compileError("TokenType " ++ @tagName(token_type) ++ " requires explicit literal");
        }

        return Token{
            .type = token_type,
            .literal = token_type.toString(),
        };
    }
};

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "fn", TokenType.FUNCTION },
    .{ "let", TokenType.LET },
    .{ "true", TokenType.TRUE },
    .{ "false", TokenType.FALSE },
    .{ "if", TokenType.IF },
    .{ "else", TokenType.ELSE },
    .{ "return", TokenType.RETURN },
});

pub fn lookupIdent(ident: []const u8) TokenType {
    return keywords.get(ident) orelse TokenType.IDENT;
}

// test
test "Test Lookup Ident" {
    const testing = std.testing;

    try testing.expectEqual(TokenType.FUNCTION, lookupIdent("fn"));
    try testing.expectEqual(TokenType.LET, lookupIdent("let"));
    try testing.expectEqual(TokenType.TRUE, lookupIdent("true"));
    try testing.expectEqual(TokenType.FALSE, lookupIdent("false"));
    try testing.expectEqual(TokenType.IF, lookupIdent("if"));
    try testing.expectEqual(TokenType.ELSE, lookupIdent("else"));
    try testing.expectEqual(TokenType.RETURN, lookupIdent("return"));
    try testing.expectEqual(TokenType.IDENT, lookupIdent("foobar"));
    try testing.expectEqual(TokenType.IDENT, lookupIdent("x"));
}
