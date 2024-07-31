const std = @import("std");

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    pub fn new(input: []const u8) Lexer {
        var l = Lexer{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = undefined,
        };
        l.readChar();
        return l;
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

    pub fn nextToken(self: *Lexer) Token {
        const str: []const u8 = &[_]u8{self.ch};

        const tok = switch (self.ch) {
            '=' => Token.init(.ASSIGN, str),
            ';' => Token.init(.SEMICOLON, str),
            '(' => Token.init(.LPAREN, str),
            ')' => Token.init(.RPAREN, str),
            ',' => Token.init(.COMMA, str),
            '+' => Token.init(.PLUS, str),
            '{' => Token.init(.LBRACE, str),
            '}' => Token.init(.RBRACE, str),
            0 => Token.init(.EOF, ""),
            else => Token.init(.ILLEGAL, str),
        };

        self.readChar();
        return tok;
    }
};

// test
test "test next token" {
    const input = "=+(){},;";
    const tokens = [_]token.Token{
        Token.init(.ASSIGN, "="),
        Token.init(.PLUS, "+"),
        Token.init(.LPAREN, "("),
        Token.init(.RPAREN, ")"),
        Token.init(.LBRACE, "{"),
        Token.init(.RBRACE, "}"),
        Token.init(.COMMA, ","),
        Token.init(.SEMICOLON, ";"),
    };

    var lexer = Lexer.new(input);
    for (tokens) |tok| {
        const t = lexer.nextToken();
        try std.testing.expect(std.mem.eql(u8, t.literal, tok.literal));
    }
}
