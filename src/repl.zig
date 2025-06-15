const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");

const Lexer = lexer.Lexer;
const Token = token.Token;
const TokenType = token.TokenType;

const PROMPT = ">> ";

pub fn start(writer: anytype, reader: anytype) !void {
    while (true) {
        try writer.print("{s}", .{PROMPT});

        var buf: [1024]u8 = undefined;
        if (try reader.readUntilDelimiterOrEof(buf[0..], '\n')) |input| {
            // 空行をスキップ
            if (input.len == 0) continue;
            
            // "exit" または "quit" でREPLを終了
            if (std.mem.eql(u8, std.mem.trim(u8, input, " \t\r\n"), "exit") or
                std.mem.eql(u8, std.mem.trim(u8, input, " \t\r\n"), "quit")) {
                try writer.print("{s}", .{"Goodbye!\n"});
                break;
            }

            var l = Lexer.init(input);
            
            while (true) {
                const tok = l.nextToken();
                if (tok.type == TokenType.EOF) break;
                
                try writer.print("Type: {s}, Literal: \"{s}\"\n", .{ tok.type.toString(), tok.literal });
            }
        } else {
            // EOF (Ctrl+D) が入力された場合
            try writer.print("{s}", .{"\nGoodbye!\n"});
            break;
        }
    }
}

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    
    try stdout.print("{s}", .{"Welcome to Monkey programming language REPL!\n"});
    try stdout.print("{s}", .{"Please enter commands. Type 'exit' or 'quit' to exit.\n"});
    
    try start(stdout, stdin);
}

test "REPL tokenizes simple expression" {
    const testing = std.testing;
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    
    var input_stream = std.io.fixedBufferStream("let x = 5;\nexit\n");
    
    try start(output.writer(), input_stream.reader());
    
    const output_str = output.items;
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: LET") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: IDENT") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: =") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: INT") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: ;") != null);
}

test "REPL handles empty lines" {
    const testing = std.testing;
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    
    var input_stream = std.io.fixedBufferStream("\n\nexit\n");
    
    try start(output.writer(), input_stream.reader());
    
    const output_str = output.items;
    try testing.expect(std.mem.indexOf(u8, output_str, "Goodbye!") != null);
}

test "REPL handles quit command" {
    const testing = std.testing;
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    
    var input_stream = std.io.fixedBufferStream("quit\n");
    
    try start(output.writer(), input_stream.reader());
    
    const output_str = output.items;
    try testing.expect(std.mem.indexOf(u8, output_str, "Goodbye!") != null);
}

test "REPL tokenizes function definition" {
    const testing = std.testing;
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    
    var input_stream = std.io.fixedBufferStream("let add = fn(x, y) { x + y; };\nexit\n");
    
    try start(output.writer(), input_stream.reader());
    
    const output_str = output.items;
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: LET") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Literal: \"add\"") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: =") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: FUNCTION") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: (") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Literal: \"x\"") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: ,") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Literal: \"y\"") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: )") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: {") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: +") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: }") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: ;") != null);
}

test "REPL tokenizes if-else statement" {
    const testing = std.testing;
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    
    var input_stream = std.io.fixedBufferStream("if (true) { return 1; } else { return 0; }\nexit\n");
    
    try start(output.writer(), input_stream.reader());
    
    const output_str = output.items;
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: IF") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: TRUE") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: RETURN") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: ELSE") != null);
}

test "REPL tokenizes operators" {
    const testing = std.testing;
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    
    var input_stream = std.io.fixedBufferStream("5 == 5 != 3 < 10 > 1 + 2 - 1 * 3 / 2 ! true\nexit\n");
    
    try start(output.writer(), input_stream.reader());
    
    const output_str = output.items;
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: ==") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: !=") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: <") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: >") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: +") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: -") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: *") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: /") != null);
    try testing.expect(std.mem.indexOf(u8, output_str, "Type: !") != null);
}