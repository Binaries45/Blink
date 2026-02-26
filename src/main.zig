const std = @import("std");
const Token = @import("tokenization/token.zig").Token;
const Lexer = @import("tokenization/lexer.zig").Lexer;
const Parser = @import("parsing/parser.zig").Parser;
const AstNode = @import("parsing/ast.zig").AstNode;
const SemanticAnalyser = @import("semantic/analyser.zig").SemanticAnalyser;

pub fn main() !void {
    var gpa = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // read the test data
    const test_path = "test_data.txt";
    const file = try std.fs.cwd().openFile(test_path, .{});
    defer file.close();

    const stat = try file.stat();
    const file_size: usize = @intCast(stat.size);

    const content: []const u8 = try file.readToEndAlloc(alloc, file_size);

    // tokenize
    var lex = Lexer.init(content);

    var lexer_timer = try std.time.Timer.start();
    const tokens: []Token = try alloc.alloc(Token, lex.src.len / 2 + 1);
    const num_tokens = try lex.tokenize(tokens);
    const lexer_elapsed: f64 = @floatFromInt(lexer_timer.read());

    std.debug.print("#########################################\n", .{});
    for (tokens[0..num_tokens]) |token| {
        std.debug.print("{f}\n", .{token});
    }

    std.debug.print("tokenized in: {d:.2} microseconds\n", .{lexer_elapsed / 1000});
    std.debug.print("{d} tokens used, {d} available\n", .{num_tokens + 1, tokens.len});
    std.debug.print("#########################################\n", .{});

    // parse
    var parser = Parser.init(tokens, num_tokens, alloc);

    var parse_timer = try std.time.Timer.start();
    const root = try parser.parseRoot();
    const parse_elapsed: f64 = @floatFromInt(parse_timer.read());

    std.debug.print("parsed in: {d:.2} microseconds\n", .{parse_elapsed / 1000});
    std.debug.print("{f}\n", .{root});
    std.debug.print("#########################################\n", .{});

    // semantic analysis
    var semantic = try SemanticAnalyser.init(alloc);

    var semantic_timer = try std.time.Timer.start();
    try semantic.analyse(root);
    const semantic_elapsed: f64 = @floatFromInt(semantic_timer.read());

    std.debug.print("anaylsed in: {d:.2} microseconds\n", .{semantic_elapsed / 1000});
    std.debug.print("TYPE TABLE:\n{f}\n", .{semantic.types});
    std.debug.print("GLOBAL : \n{f}\n", .{semantic.global});
    std.debug.print("#########################################\n", .{});
}