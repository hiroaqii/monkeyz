const std = @import("std");
const token = @import("token.zig");

const Token = token.Token;

pub const Node = union(enum) {
    // Statements
    let_statement: LetStatement,
    return_statement: ReturnStatement,

    // Expressions
    identifier: Identifier,

    pub fn tokenLiteral(self: Node) []const u8 {
        return switch (self) {
            .let_statement => |stmt| stmt.token.literal,
            .return_statement => |stmt| stmt.token.literal,
            .identifier => |ident| ident.token.literal,
        };
    }

    // 型チェック用のヘルパー関数
    pub fn isStatement(self: Node) bool {
        return switch (self) {
            .let_statement => true,
            .return_statement => true,
            .identifier => false,
        };
    }

    pub fn isExpression(self: Node) bool {
        return switch (self) {
            .let_statement => false,
            .return_statement => false,
            .identifier => true,
        };
    }
};

// Program - ASTのルートノード
pub const Program = struct {
    nodes: std.ArrayList(Node),

    pub fn init(allocator: std.mem.Allocator) Program {
        return Program{
            .nodes = std.ArrayList(Node).init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        self.nodes.deinit();
    }

    pub fn tokenLiteral(self: Program) []const u8 {
        if (self.nodes.items.len > 0) {
            return self.nodes.items[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn addNode(self: *Program, node: Node) !void {
        try self.nodes.append(node);
    }

    // Statement専用の追加メソッド
    pub fn addStatement(self: *Program, stmt_node: Node) !void {
        if (!stmt_node.isStatement()) {
            return error.NotAStatement;
        }
        try self.addNode(stmt_node);
    }

    // 便利なヘルパーメソッド
    pub fn statementCount(self: Program) usize {
        var count: usize = 0;
        for (self.nodes.items) |node| {
            if (node.isStatement()) count += 1;
        }
        return count;
    }
};

// LetStatement
pub const LetStatement = struct {
    token: Token, // LETトークン
    name: Identifier, // 変数名（ポインタ不要）
    value: ?*Node, // 値（ポインタで循環依存を回避）

    pub fn init(let_token: Token, name: Identifier, value: ?*Node) LetStatement {
        return LetStatement{
            .token = let_token,
            .name = name,
            .value = value,
        };
    }
};

// ReturnStatement
pub const ReturnStatement = struct {
    token: Token, // RETURNトークン
    return_value: ?*Node, // 戻り値（ポインタで循環依存を回避）

    pub fn init(return_token: Token, return_value: ?*Node) ReturnStatement {
        return ReturnStatement{
            .token = return_token,
            .return_value = return_value,
        };
    }
};

// Identifier
pub const Identifier = struct {
    token: Token, // IDENTトークン
    value: []const u8, // 識別子の文字列

    pub fn init(ident_token: Token, value: []const u8) Identifier {
        return Identifier{
            .token = ident_token,
            .value = value,
        };
    }
};

// Tests
const testing = std.testing;

test "Identifier creation and access" {
    const ident = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "myVar"), "myVar");

    try testing.expectEqualStrings("myVar", ident.token.literal);
    try testing.expectEqualStrings("myVar", ident.value);

    // Node として使用
    const node = Node{ .identifier = ident };
    try testing.expectEqualStrings("myVar", node.tokenLiteral());
    try testing.expect(node.isExpression());
    try testing.expect(!node.isStatement());
}

test "LetStatement creation and access" {
    const name = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "x"), "x");

    var value_node = Node{ .identifier = name };

    const let_stmt = LetStatement.init(Token.initAlnum(token.TokenType.LET, "let"), name, &value_node);

    try testing.expectEqualStrings("let", let_stmt.token.literal);
    try testing.expectEqualStrings("x", let_stmt.name.value);

    // Node として使用
    const node = Node{ .let_statement = let_stmt };
    try testing.expectEqualStrings("let", node.tokenLiteral());
    try testing.expect(node.isStatement());
    try testing.expect(!node.isExpression());
}

test "Program operations" {
    const allocator = testing.allocator;

    var program = Program.init(allocator);
    defer program.deinit();

    // 空のプログラムのテスト
    try testing.expectEqualStrings("", program.tokenLiteral());
    try testing.expectEqual(@as(usize, 0), program.statementCount());

    // 識別子ノードを作成
    const ident = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "x"), "x");

    // Let文を作成
    var value_node = Node{ .identifier = ident };
    const let_stmt = LetStatement.init(Token.initAlnum(token.TokenType.LET, "let"), ident, &value_node);

    const stmt_node = Node{ .let_statement = let_stmt };

    // ステートメントを追加
    try program.addStatement(stmt_node);

    // プログラムの状態をテスト
    try testing.expectEqualStrings("let", program.tokenLiteral());
    try testing.expectEqual(@as(usize, 1), program.statementCount());
    try testing.expectEqual(@as(usize, 1), program.nodes.items.len);
}

test "ReturnStatement creation and access" {
    const return_token = Token.initAlnum(token.TokenType.RETURN, "return");
    
    // 戻り値なしのreturn文
    const return_stmt_empty = ReturnStatement.init(return_token, null);
    try testing.expectEqualStrings("return", return_stmt_empty.token.literal);
    try testing.expect(return_stmt_empty.return_value == null);
    
    // 戻り値ありのreturn文
    const ident = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "x"), "x");
    var value_node = Node{ .identifier = ident };
    const return_stmt_with_value = ReturnStatement.init(return_token, &value_node);
    
    try testing.expectEqualStrings("return", return_stmt_with_value.token.literal);
    try testing.expect(return_stmt_with_value.return_value != null);

    // Node として使用
    const node = Node{ .return_statement = return_stmt_with_value };
    try testing.expectEqualStrings("return", node.tokenLiteral());
    try testing.expect(node.isStatement());
    try testing.expect(!node.isExpression());
}

test "Program type safety" {
    const allocator = testing.allocator;

    var program = Program.init(allocator);
    defer program.deinit();

    // 式を文として追加しようとするとエラー
    const ident = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "x"), "x");
    const expr_node = Node{ .identifier = ident };

    // これはエラーになるべき
    const result = program.addStatement(expr_node);
    try testing.expectError(error.NotAStatement, result);
}
