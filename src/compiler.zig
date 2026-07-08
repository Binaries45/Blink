const std = @import("std");
const Io = std.Io;
const Process = @import("cli.zig").Process;
const Token = @import("compiler/frontend/Token.zig");
const Lexer = @import("compiler/frontend/Lexer.zig");
const Parser = @import("compiler/frontend/Parser.zig");
const Ast = @import("compiler/frontend/Ast.zig");
const Render = @import("compiler/frontend/AstRender.zig");

const Compiler = @This();

pub fn compile(alloc: std.mem.Allocator, io: Io, proc: Process) !void {
    // read the given file into buffer
    const content = try Io.Dir.cwd().readFileAllocOptions(io, proc.src, alloc, .unlimited, .@"1", 0);
    defer alloc.free(content);

    // tokenize & parse the file
    var ast = try Ast.parse(alloc, content);
    defer ast.deinit(alloc);
    if (ast.errors.len > 0) {
        Render.renderErrors(&ast, content);
    }
    Render.render(&ast, content);

    // semantic analysis

    // optimization passes

    // target code generation
}
