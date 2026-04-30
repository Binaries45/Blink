const std = @import("std");
const Process = @import("cli.zig").Process;
const Token = @import("compiler/frontend/Token.zig");
const Lexer = @import("compiler/frontend/Lexer.zig");
const Parser = @import("compiler/frontend/Parser.zig");

pub const Compiler = @This();

pub fn compile(alloc: std.mem.Allocator, proc: Process) !void {
    // read the given file into buffer
    const file = try std.fs.cwd().openFile(proc.src, .{});
    defer file.close();

    const size = (try file.stat()).size;
    // todo : massive files might become an issue
    const content: [:0]const u8 = try file.readToEndAllocOptions(
        alloc,
        size,
        null,
        std.mem.Alignment.@"1",
        0
    );
    defer alloc.free(content);

    // tokenize & parse the file
    const tokens = try alloc.alloc(Token, size / 2 + 1);
    defer alloc.free(tokens);
    var lexer = Lexer.init(content);
    const n_tokens = lexer.tokenize(tokens);

    for (tokens[0..n_tokens]) |tok| {
        std.debug.print("{s}: {s}\n", .{@tagName(tok.kind), content[tok.start..tok.end]});
    }
    // todo : instead of tokenizing here, we simply call .next() inside of the parse function which we will add as a member of the AST, this function will collect the tokens first, and then parse them into the ast

    // semantic analysis

    // optimization passes

    // target code generation
}