const std = @import("std");
const token = @import("token.zig");

const Token = token.Token;

pub const Node = union(enum) {
    // Statements
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,

    // Expressions
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    prefix_expression: PrefixExpression,

    pub fn tokenLiteral(self: Node) []const u8 {
        return switch (self) {
            .let_statement => |stmt| stmt.token.literal,
            .return_statement => |stmt| stmt.token.literal,
            .expression_statement => |stmt| stmt.token.literal,
            .identifier => |ident| ident.token.literal,
            .integer_literal => |int_lit| int_lit.token.literal,
            .prefix_expression => |prefix_expr| prefix_expr.token.literal,
        };
    }

    // 型チェック用のヘルパー関数
    pub fn isStatement(self: Node) bool {
        return switch (self) {
            .let_statement => true,
            .return_statement => true,
            .expression_statement => true,
            .identifier => false,
            .integer_literal => false,
            .prefix_expression => false,
        };
    }

    pub fn isExpression(self: Node) bool {
        return switch (self) {
            .let_statement => false,
            .return_statement => false,
            .expression_statement => false,
            .identifier => true,
            .integer_literal => true,
            .prefix_expression => true,
        };
    }

    pub fn toString(self: Node, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        return switch (self) {
            .let_statement => |stmt| try stmt.toString(allocator),
            .return_statement => |stmt| try stmt.toString(allocator),
            .expression_statement => |stmt| try stmt.toString(allocator),
            .identifier => |ident| try ident.toString(allocator),
            .integer_literal => |int_lit| try int_lit.toString(allocator),
            .prefix_expression => |prefix_expr| try prefix_expr.toString(allocator),
        };
    }
};

// Program - ASTのルートノード
pub const Program = struct {
    nodes: std.ArrayList(Node),
    allocator: std.mem.Allocator,
    // 動的に割り当てられたNodeを追跡するリスト
    allocated_nodes: std.ArrayList(*Node),

    pub fn init(allocator: std.mem.Allocator) Program {
        return Program{
            .nodes = std.ArrayList(Node).init(allocator),
            .allocator = allocator,
            .allocated_nodes = std.ArrayList(*Node).init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        // 動的に割り当てられたノードを解放
        for (self.allocated_nodes.items) |node| {
            self.allocator.destroy(node);
        }
        self.allocated_nodes.deinit();
        self.nodes.deinit();
    }

    // 動的に割り当てられたNodeを追跡
    pub fn trackAllocatedNode(self: *Program, node: *Node) !void {
        try self.allocated_nodes.append(node);
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

    pub fn toString(self: Program, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        var out = std.ArrayList(u8).init(allocator);
        defer out.deinit();

        for (self.nodes.items) |node| {
            const node_str = try node.toString(allocator);
            defer allocator.free(node_str);
            try out.appendSlice(node_str);
        }

        return out.toOwnedSlice();
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

    pub fn toString(self: LetStatement, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        var out = std.ArrayList(u8).init(allocator);
        defer out.deinit();

        try out.appendSlice(self.token.literal);
        try out.appendSlice(" ");

        const name_str = try self.name.toString(allocator);
        defer allocator.free(name_str);
        try out.appendSlice(name_str);

        try out.appendSlice(" = ");

        if (self.value) |value| {
            const value_str = try value.toString(allocator);
            defer allocator.free(value_str);
            try out.appendSlice(value_str);
        }

        try out.appendSlice(";");

        return out.toOwnedSlice();
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

    pub fn toString(self: ReturnStatement, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        var out = std.ArrayList(u8).init(allocator);
        defer out.deinit();

        try out.appendSlice(self.token.literal);
        try out.appendSlice(" ");

        if (self.return_value) |return_value| {
            const value_str = try return_value.toString(allocator);
            defer allocator.free(value_str);
            try out.appendSlice(value_str);
        }

        try out.appendSlice(";");

        return out.toOwnedSlice();
    }
};

// ExpressionStatement
pub const ExpressionStatement = struct {
    token: Token, // 式の最初のトークン
    expression: ?*Node, // 式（ポインタで循環依存を回避）

    pub fn init(first_token: Token, expression: ?*Node) ExpressionStatement {
        return ExpressionStatement{
            .token = first_token,
            .expression = expression,
        };
    }

    pub fn toString(self: ExpressionStatement, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        if (self.expression) |expression| {
            return try expression.toString(allocator);
        }
        return try allocator.dupe(u8, "");
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

    pub fn toString(self: Identifier, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        return try allocator.dupe(u8, self.value);
    }
};

// IntegerLiteral
pub const IntegerLiteral = struct {
    token: Token, // INTトークン
    value: i64, // 整数値

    pub fn init(int_token: Token, value: i64) IntegerLiteral {
        return IntegerLiteral{
            .token = int_token,
            .value = value,
        };
    }

    pub fn toString(self: IntegerLiteral, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        return try std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }
};

// PrefixExpression
pub const PrefixExpression = struct {
    token: Token, // 前置トークン（例：!、-）
    operator: []const u8, // 演算子
    right: *Node, // 右側の式

    pub fn init(prefix_token: Token, operator: []const u8, right: *Node) PrefixExpression {
        return PrefixExpression{
            .token = prefix_token,
            .operator = operator,
            .right = right,
        };
    }

    pub fn toString(self: PrefixExpression, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        var out = std.ArrayList(u8).init(allocator);
        defer out.deinit();

        try out.appendSlice("(");
        try out.appendSlice(self.operator);

        const right_str = try self.right.toString(allocator);
        defer allocator.free(right_str);
        try out.appendSlice(right_str);

        try out.appendSlice(")");

        return out.toOwnedSlice();
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

test "ExpressionStatement creation and access" {
    const first_token = Token.initAlnum(token.TokenType.IDENT, "x");

    // 式なしのExpression文
    const expr_stmt_empty = ExpressionStatement.init(first_token, null);
    try testing.expectEqualStrings("x", expr_stmt_empty.token.literal);
    try testing.expect(expr_stmt_empty.expression == null);

    // 式ありのExpression文
    const ident = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "x"), "x");
    var expr_node = Node{ .identifier = ident };
    const expr_stmt_with_expr = ExpressionStatement.init(first_token, &expr_node);

    try testing.expectEqualStrings("x", expr_stmt_with_expr.token.literal);
    try testing.expect(expr_stmt_with_expr.expression != null);

    // Node として使用
    const node = Node{ .expression_statement = expr_stmt_with_expr };
    try testing.expectEqualStrings("x", node.tokenLiteral());
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

test "TestString" {
    const allocator = testing.allocator;

    var program = Program.init(allocator);
    defer program.deinit();

    const name = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "myVar"), "myVar");
    const value = Identifier.init(Token.initAlnum(token.TokenType.IDENT, "anotherVar"), "anotherVar");

    var value_node = Node{ .identifier = value };
    const let_stmt = LetStatement.init(Token.initAlnum(token.TokenType.LET, "let"), name, &value_node);
    const let_stmt_node = Node{ .let_statement = let_stmt };

    try program.addStatement(let_stmt_node);

    const program_str = try program.toString(allocator);
    defer allocator.free(program_str);
    try testing.expectEqualStrings("let myVar = anotherVar;", program_str);
}
