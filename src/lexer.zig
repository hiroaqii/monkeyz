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

    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.readChar();
        }
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const start_position = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[start_position..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const start_position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[start_position..self.position];
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();

        switch (self.ch) {
            '=' => {
                self.readChar();
                return Token.init(TokenType.ASSIGN, "=");
            },
            '+' => {
                self.readChar();
                return Token.init(TokenType.PLUS, "+");
            },
            '-' => {
                self.readChar();
                return Token.init(TokenType.MINUS, "-");
            },
            '!' => {
                self.readChar();
                return Token.init(TokenType.BANG, "!");
            },
            '/' => {
                self.readChar();
                return Token.init(TokenType.SLASH, "/");
            },
            '*' => {
                self.readChar();
                return Token.init(TokenType.ASTERISK, "*");
            },
            '<' => {
                self.readChar();
                return Token.init(TokenType.LT, "<");
            },
            '>' => {
                self.readChar();
                return Token.init(TokenType.GT, ">");
            },
            ';' => {
                self.readChar();
                return Token.init(TokenType.SEMICOLON, ";");
            },
            '(' => {
                self.readChar();
                return Token.init(TokenType.LPAREN, "(");
            },
            ')' => {
                self.readChar();
                return Token.init(TokenType.RPAREN, ")");
            },
            ',' => {
                self.readChar();
                return Token.init(TokenType.COMMA, ",");
            },
            '{' => {
                self.readChar();
                return Token.init(TokenType.LBRACE, "{");
            },
            '}' => {
                self.readChar();
                return Token.init(TokenType.RBRACE, "}");
            },
            0 => {
                return Token.init(TokenType.EOF, "");
            },
            else => {
                if (isLetter(self.ch)) {
                    const literal = self.readIdentifier();
                    const token_type = token.lookupIdent(literal);
                    return Token.init(token_type, literal);
                } else if (isDigit(self.ch)) {
                    const literal = self.readNumber();
                    return Token.init(TokenType.INT, literal);
                } else {
                    const ch = self.ch;
                    self.readChar();
                    return Token.init(TokenType.ILLEGAL, &[_]u8{ch});
                }
            },
        }
    }
};

fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or (ch == '_');
}

// Tests for the Lexer
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

test "TestNextToken v2" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
    ;

    const tests = [_]struct {
        expected_type: token.TokenType,
        expected_literal: []const u8,
    }{
        // let five = 5;
        .{ .expected_type = token.TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "five" },
        .{ .expected_type = token.TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },

        // let ten = 10;
        .{ .expected_type = token.TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "ten" },
        .{ .expected_type = token.TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },

        // let add = fn(x, y) {
        .{ .expected_type = token.TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "add" },
        .{ .expected_type = token.TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.FUNCTION, .expected_literal = "fn" },
        .{ .expected_type = token.TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "x" },
        .{ .expected_type = token.TokenType.COMMA, .expected_literal = "," },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "y" },
        .{ .expected_type = token.TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.LBRACE, .expected_literal = "{" },

        //   x + y;
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "x" },
        .{ .expected_type = token.TokenType.PLUS, .expected_literal = "+" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "y" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },

        // };
        .{ .expected_type = token.TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },

        // let result = add(five, ten);
        .{ .expected_type = token.TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "result" },
        .{ .expected_type = token.TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "add" },
        .{ .expected_type = token.TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "five" },
        .{ .expected_type = token.TokenType.COMMA, .expected_literal = "," },
        .{ .expected_type = token.TokenType.IDENT, .expected_literal = "ten" },
        .{ .expected_type = token.TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },

        // !-/*5;
        .{ .expected_type = token.TokenType.BANG, .expected_literal = "!" },
        .{ .expected_type = token.TokenType.MINUS, .expected_literal = "-" },
        .{ .expected_type = token.TokenType.SLASH, .expected_literal = "/" },
        .{ .expected_type = token.TokenType.ASTERISK, .expected_literal = "*" },
        .{ .expected_type = token.TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },

        // 5 < 10 > 5;
        .{ .expected_type = token.TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = token.TokenType.LT, .expected_literal = "<" },
        .{ .expected_type = token.TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.GT, .expected_literal = ">" },
        .{ .expected_type = token.TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },

        // if (5 < 10) {
        .{ .expected_type = token.TokenType.IF, .expected_literal = "if" },
        .{ .expected_type = token.TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = token.TokenType.LT, .expected_literal = "<" },
        .{ .expected_type = token.TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.LBRACE, .expected_literal = "{" },
        //   return true;
        .{ .expected_type = token.TokenType.RETURN, .expected_literal = "return" },
        .{ .expected_type = token.TokenType.TRUE, .expected_literal = "true" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },
        // } else {
        .{ .expected_type = token.TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = token.TokenType.ELSE, .expected_literal = "else" },
        .{ .expected_type = token.TokenType.LBRACE, .expected_literal = "{" },
        //   return false;
        .{ .expected_type = token.TokenType.RETURN, .expected_literal = "return" },
        .{ .expected_type = token.TokenType.FALSE, .expected_literal = "false" },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },
        // }
        .{ .expected_type = token.TokenType.RBRACE, .expected_literal = "}" },

        // EOF
        .{ .expected_type = token.TokenType.EOF, .expected_literal = "" },
    };

    var l = Lexer.init(input);

    for (tests) |expected| {
        const tok = l.nextToken();
        try testing.expect(tok.type == expected.expected_type);
        try testing.expectEqualStrings(expected.expected_literal, tok.literal);
    }
}
