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
const ReturnStatement = ast.ReturnStatement;
const ExpressionStatement = ast.ExpressionStatement;
const Identifier = ast.Identifier;
const IntegerLiteral = ast.IntegerLiteral;
const PrefixExpression = ast.PrefixExpression;

const PrefixParseFn = *const fn (*Parser) ?Node;
const InfixParseFn = *const fn (*Parser, Node) ?Node;

// 演算子優先順位の定義
const Precedence = enum(u8) {
    LOWEST = 1, // 最低優先順位
    EQUALS = 2, // == !=
    LESSGREATER = 3, // > <
    SUM = 4, // +
    PRODUCT = 5, // *
    PREFIX = 6, // -X !X
    CALL = 7, // myFunction(X)
};

pub const Parser = struct {
    l: *Lexer,
    current_token: Token,
    peek_token: Token,
    errors: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    prefix_parse_fns: std.HashMap(TokenType, PrefixParseFn, std.hash_map.AutoContext(TokenType), std.hash_map.default_max_load_percentage),
    infix_parse_fns: std.HashMap(TokenType, InfixParseFn, std.hash_map.AutoContext(TokenType), std.hash_map.default_max_load_percentage),

    pub fn init(allocator: std.mem.Allocator, l: *Lexer) Parser {
        var p = Parser{
            .l = l,
            .current_token = undefined,
            .peek_token = undefined,
            .errors = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
            .prefix_parse_fns = std.HashMap(TokenType, PrefixParseFn, std.hash_map.AutoContext(TokenType), std.hash_map.default_max_load_percentage).init(allocator),
            .infix_parse_fns = std.HashMap(TokenType, InfixParseFn, std.hash_map.AutoContext(TokenType), std.hash_map.default_max_load_percentage).init(allocator),
        };

        // 2 つトークンを読み込む。curToken と peekToken の両方がセットされる。
        p.nextToken();
        p.nextToken();

        // 前置解析関数を登録
        p.registerPrefix(TokenType.IDENT, parseIdentifier) catch {};
        p.registerPrefix(TokenType.INT, parseIntegerLiteral) catch {};
        p.registerPrefix(TokenType.BANG, parsePrefixExpression) catch {};
        p.registerPrefix(TokenType.MINUS, parsePrefixExpression) catch {};

        return p;
    }

    pub fn deinit(self: *Parser) void {
        // エラーメッセージの文字列を解放
        for (self.errors.items) |error_msg| {
            self.allocator.free(error_msg);
        }
        self.errors.deinit();

        // 関数マップを解放
        self.prefix_parse_fns.deinit();
        self.infix_parse_fns.deinit();
    }

    pub fn parseProgram(self: *Parser) !Program {
        var program = Program.init(self.allocator);

        while (self.current_token.type != TokenType.EOF) {
            if (self.parseStatement()) |stmt_node| {
                try program.addStatement(stmt_node);

                // 動的に割り当てられたノードを追跡
                switch (stmt_node) {
                    .expression_statement => |expr_stmt| {
                        if (expr_stmt.expression) |expr| {
                            try program.trackAllocatedNode(expr);
                            
                            // PrefixExpressionの右側ノードも追跡
                            switch (expr.*) {
                                .prefix_expression => |prefix_expr| {
                                    try program.trackAllocatedNode(prefix_expr.right);
                                },
                                else => {},
                            }
                        }
                    },
                    else => {},
                }
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
            TokenType.RETURN => self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseExpressionStatement(self: *Parser) ?Node {
        const stmt_token = self.current_token;

        const expression = self.parseExpression(Precedence.LOWEST);

        // メモリ管理のため、アロケータを使って動的にNodeを作成
        const expr_node = self.allocator.create(Node) catch return null;
        expr_node.* = expression.?;

        const expr_stmt = ExpressionStatement.init(stmt_token, expr_node);

        // セミコロンまで進む
        if (self.peekTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        return Node{ .expression_statement = expr_stmt };
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

    fn parseReturnStatement(self: *Parser) ?Node {
        const stmt_token = self.current_token;

        self.nextToken();

        // TODO: セミコロンに遭遇するまで式を読み飛ばしてる
        while (!self.currentTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        const return_stmt = ReturnStatement.init(stmt_token, null);
        return Node{ .return_statement = return_stmt };
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

    fn registerPrefix(self: *Parser, token_type: TokenType, parse_fn: PrefixParseFn) !void {
        try self.prefix_parse_fns.put(token_type, parse_fn);
    }

    fn registerInfix(self: *Parser, token_type: TokenType, parse_fn: InfixParseFn) !void {
        try self.infix_parse_fns.put(token_type, parse_fn);
    }

    // 式を解析するメソッド
    fn parseExpression(self: *Parser, precedence: Precedence) ?Node {
        _ = precedence; // 最初のバージョンでは使用しない

        const prefix_fn = self.prefix_parse_fns.get(self.current_token.type);
        if (prefix_fn == null) {
            return null;
        }

        const left_exp = prefix_fn.?(self);
        return left_exp;
    }

    // 識別子解析関数
    fn parseIdentifier(self: *Parser) ?Node {
        const ident = Identifier.init(self.current_token, self.current_token.literal);
        return Node{ .identifier = ident };
    }

    // 整数リテラル解析関数
    fn parseIntegerLiteral(self: *Parser) ?Node {
        const value = std.fmt.parseInt(i64, self.current_token.literal, 10) catch {
            const msg = std.fmt.allocPrint(self.allocator, "could not parse {s} as integer", .{self.current_token.literal}) catch return null;
            self.errors.append(msg) catch return null;
            return null;
        };

        const int_lit = IntegerLiteral.init(self.current_token, value);
        return Node{ .integer_literal = int_lit };
    }

    // 前置式解析関数
    fn parsePrefixExpression(self: *Parser) ?Node {
        const prefix_token = self.current_token;
        const operator = self.current_token.literal;

        self.nextToken();

        const right_expr = self.parseExpression(Precedence.PREFIX);
        if (right_expr == null) {
            return null;
        }

        // 動的に右側のノードを作成
        const right_node = self.allocator.create(Node) catch return null;
        right_node.* = right_expr.?;

        const prefix_expr = PrefixExpression.init(prefix_token, operator, right_node);
        return Node{ .prefix_expression = prefix_expr };
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

test "TestReturnStatements" {
    const allocator = testing.allocator;

    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = Lexer.init(input);
    var parser = Parser.init(allocator, &l);
    defer parser.deinit();

    var program = try parser.parseProgram();
    defer program.deinit();

    try checkParserErrors(&parser);

    // program.Statements != 3 のチェック
    try testing.expectEqual(@as(usize, 3), program.statementCount());

    // 各ステートメントがReturnStatementであることを確認
    for (program.nodes.items) |stmt_node| {
        try testReturnStatement(stmt_node);
    }
}

fn testReturnStatement(stmt_node: Node) !void {
    // stmt が return_statement であることを確認
    try testing.expect(stmt_node.isStatement());

    switch (stmt_node) {
        .return_statement => |return_stmt| {
            // returnStmt.TokenLiteral() != "return" のチェック
            try testing.expectEqualStrings("return", return_stmt.token.literal);
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

test "TestIdentifierExpression" {
    const allocator = testing.allocator;

    const input = "foobar;";

    var l = Lexer.init(input);
    var parser = Parser.init(allocator, &l);
    defer parser.deinit();

    var program = try parser.parseProgram();
    defer program.deinit();

    try checkParserErrors(&parser);

    // program has not enough statements のチェック
    try testing.expectEqual(@as(usize, 1), program.nodes.items.len);

    const stmt_node = program.nodes.items[0];

    // program.Statements[0] is not ast.ExpressionStatement のチェック
    try testing.expect(stmt_node.isStatement());

    switch (stmt_node) {
        .expression_statement => |expr_stmt| {
            // stmt.Expression が存在することを確認
            try testing.expect(expr_stmt.expression != null);

            const expression = expr_stmt.expression.?;

            // exp not *ast.Identifier のチェック
            switch (expression.*) {
                .identifier => |ident| {
                    // ident.Value not "foobar" のチェック
                    try testing.expectEqualStrings("foobar", ident.value);

                    // ident.TokenLiteral not "foobar" のチェック
                    try testing.expectEqualStrings("foobar", ident.token.literal);
                },
                else => {
                    try testing.expect(false); // exp not *ast.Identifier
                },
            }
        },
        else => {
            try testing.expect(false); // program.Statements[0] is not ast.ExpressionStatement
        },
    }
}

test "TestIntegerLiteralExpression" {
    const allocator = testing.allocator;

    const input = "5;";

    var l = Lexer.init(input);
    var parser = Parser.init(allocator, &l);
    defer parser.deinit();

    var program = try parser.parseProgram();
    defer program.deinit();

    try checkParserErrors(&parser);

    // program has not enough statements のチェック
    try testing.expectEqual(@as(usize, 1), program.nodes.items.len);

    const stmt_node = program.nodes.items[0];

    // program.Statements[0] is not ast.ExpressionStatement のチェック
    try testing.expect(stmt_node.isStatement());

    switch (stmt_node) {
        .expression_statement => |expr_stmt| {
            // stmt.Expression が存在することを確認
            try testing.expect(expr_stmt.expression != null);

            const expression = expr_stmt.expression.?;

            // exp not *ast.IntegerLiteral のチェック
            switch (expression.*) {
                .integer_literal => |int_lit| {
                    // literal.Value not 5 のチェック
                    try testing.expectEqual(@as(i64, 5), int_lit.value);

                    // literal.TokenLiteral not "5" のチェック
                    try testing.expectEqualStrings("5", int_lit.token.literal);
                },
                else => {
                    try testing.expect(false); // exp not *ast.IntegerLiteral
                },
            }
        },
        else => {
            try testing.expect(false); // program.Statements[0] is not ast.ExpressionStatement
        },
    }
}

// テストヘルパー関数
fn testIntegerLiteral(node: Node, value: i64) !void {
    switch (node) {
        .integer_literal => |int_lit| {
            try testing.expectEqual(value, int_lit.value);
            
            // トークンリテラルのテスト
            const expected_literal = std.fmt.allocPrint(testing.allocator, "{d}", .{value}) catch unreachable;
            defer testing.allocator.free(expected_literal);
            try testing.expectEqualStrings(expected_literal, int_lit.token.literal);
        },
        else => {
            std.debug.print("node is not IntegerLiteral. got={}\n", .{node});
            try testing.expect(false);
        },
    }
}

test "TestParsingPrefixExpressions" {
    const allocator = testing.allocator;

    const PrefixTest = struct {
        input: []const u8,
        operator: []const u8,
        integer_value: i64,
    };

    const prefix_tests = [_]PrefixTest{
        .{ .input = "!5;", .operator = "!", .integer_value = 5 },
        .{ .input = "-15;", .operator = "-", .integer_value = 15 },
    };

    for (prefix_tests) |tt| {
        var l = Lexer.init(tt.input);
        var p = Parser.init(allocator, &l);
        defer p.deinit();

        var program = try p.parseProgram();
        defer program.deinit();

        try checkParserErrors(&p);

        // プログラムが1つの文を含むことを確認
        try testing.expectEqual(@as(usize, 1), program.statementCount());

        const stmt_node = program.nodes.items[0];
        
        // ExpressionStatementであることを確認
        switch (stmt_node) {
            .expression_statement => |expr_stmt| {
                // 式が存在することを確認
                try testing.expect(expr_stmt.expression != null);
                
                const exp = expr_stmt.expression.?.*;
                
                // PrefixExpressionであることを確認
                switch (exp) {
                    .prefix_expression => |prefix_expr| {
                        // 演算子をテスト
                        try testing.expectEqualStrings(tt.operator, prefix_expr.operator);
                        
                        // 右側の値をテスト
                        try testIntegerLiteral(prefix_expr.right.*, tt.integer_value);
                    },
                    else => {
                        std.debug.print("stmt.expression is not PrefixExpression. got={}\n", .{exp});
                        try testing.expect(false);
                    },
                }
            },
            else => {
                std.debug.print("program.statements[0] is not ExpressionStatement. got={}\n", .{stmt_node});
                try testing.expect(false);
            },
        }
    }
}
