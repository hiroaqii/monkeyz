const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const Lexer = lexer.Lexer;
const Token = token.Token;
const TokenType = token.TokenType;
const Node = ast.Node;
const Program = ast.Program;
const LetStatement = ast.LetStatement;
const Identifier = ast.Identifier;

pub const Parser = struct {
    l: *Lexer,
    current_token: Token,
    peek_token: Token,
    errors: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, l: *Lexer) Parser {
        var p = Parser{
            .l = l,
            .current_token = undefined,
            .peek_token = undefined,
            .errors = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };

        // 2 つトークンを読み込む。curToken と peekToken の両方がセットされる。
        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn deinit(self: *Parser) void {
        // エラーメッセージの文字列を解放
        for (self.errors.items) |error_msg| {
            self.allocator.free(error_msg);
        }
        self.errors.deinit();
    }

    pub fn parseProgram(self: *Parser) !Program {
        var program = Program.init(self.allocator);

        while (self.current_token.type != TokenType.EOF) {
            if (self.parseStatement()) |stmt_node| {
                try program.addStatement(stmt_node);
            }
            self.nextToken();
        }

        return program;
    }

    pub fn getErrors(self: Parser) []const []const u8 {
        return self.errors.items;
    }

    fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.l.nextToken();
    }

    fn parseStatement(self: *Parser) ?Node {
        return switch (self.current_token.type) {
            TokenType.LET => self.parseLetStatement(),
            else => null,
        };
    }

    fn parseLetStatement(self: *Parser) ?Node {
        const stmt_token = self.current_token;

        if (!self.expectPeek(TokenType.IDENT)) {
            return null;
        }

        const name = Identifier.init(self.current_token, self.current_token.literal);

        if (!self.expectPeek(TokenType.ASSIGN)) {
            return null;
        }

        // TODO: セミコロンに遭遇するまで式を読み飛ばしてる
        while (!self.currentTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        const let_stmt = LetStatement.init(stmt_token, name, null);
        return Node{ .let_statement = let_stmt };
    }

    fn currentTokenIs(self: Parser, t: TokenType) bool {
        return self.current_token.type == t;
    }

    fn peekTokenIs(self: Parser, t: TokenType) bool {
        return self.peek_token.type == t;
    }

    fn expectPeek(self: *Parser, t: TokenType) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        } else {
            self.peekError(t);
            return false;
        }
    }

    fn peekError(self: *Parser, t: TokenType) void {
        const msg = std.fmt.allocPrint(self.allocator, "expected next token to be {s}, got {s} instead", .{ t.toString(), self.peek_token.type.toString() }) catch return; // エラーハンドリング簡略化

        self.errors.append(msg) catch return; // エラーハンドリング簡略化
    }
};

// Tests
const testing = std.testing;

test "TestLetStatement" {
    const allocator = testing.allocator;

    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var l = Lexer.init(input);
    var parser = Parser.init(allocator, &l);
    defer parser.deinit();

    var program = try parser.parseProgram();
    defer program.deinit();

    try checkParserErrors(&parser);

    // program.Statements != 3 のチェック
    try testing.expectEqual(@as(usize, 3), program.statementCount());

    const expected_identifiers = [_][]const u8{ "x", "y", "foobar" };

    for (expected_identifiers, 0..) |expected_identifier, i| {
        const stmt_node = program.nodes.items[i];
        try testLetStatement(stmt_node, expected_identifier);
    }
}

fn testLetStatement(stmt_node: Node, name: []const u8) !void {
    // s.TokenLiteral() != "let" のチェック
    try testing.expectEqualStrings("let", stmt_node.tokenLiteral());

    // stmt_node が let_statement であることを確認
    try testing.expect(stmt_node.isStatement());

    switch (stmt_node) {
        .let_statement => |let_stmt| {
            // letStmt.Name.Value != name のチェック
            try testing.expectEqualStrings(name, let_stmt.name.value);

            // letStmt.Name.TokenLiteral() != name のチェック
            try testing.expectEqualStrings(name, let_stmt.name.token.literal);
        },
        else => {
            try testing.expect(false); // Should not reach here
        },
    }
}

fn checkParserErrors(parser: *Parser) !void {
    const errors = parser.getErrors();
    if (errors.len == 0) {
        return;
    }

    std.debug.print("parser has {} errors\n", .{errors.len});
    for (errors) |msg| {
        std.debug.print("parser error: \"{s}\"\n", .{msg});
    }

    // テストを失敗させる
    try testing.expect(false);
}
