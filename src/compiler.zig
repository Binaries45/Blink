const std = @import("std");
const Process = @import("cli.zig").Process;
const Token = @import("compiler/frontend/Token.zig");
const Lexer = @import("compiler/frontend/Lexer.zig");
const Parser = @import("compiler/frontend/Parser.zig");
const Ast = @import("compiler/frontend/Ast.zig");
const Render = @import("compiler/frontend/AstRender.zig");

const Compiler = @This();

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
    var ast = try Ast.parse(alloc, content);
    defer ast.deinit(alloc);
    Render.renderErrors(&ast, content);

    // semantic analysis

    // optimization passes

    // target code generation
}