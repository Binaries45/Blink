const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

// todo : this will be used to print the Ast

pub fn renderErrors(ast: *Ast, src: [:0]const u8) void {
    for (ast.errors) |err| {
        switch (err) {
            .ExpectedEof => |e| std.debug.print("Expected EOF got '{s}'\n", .{
                src[e.start..e.end]
            }),
            .ExpectedFn => |e| std.debug.print("Expected 'fn' got '{s}'\n", .{
                src[e.start..e.end]
            }),
            .ExpectedIdentifier => |e| std.debug.print("Expected indetifier got '{s}'\n", .{
                src[e.start..e.end]
            }),
            .ExpectedPubItem => |e| std.debug.print("Expected pub item got '{s}'\n", .{
                src[e.start..e.end]
            }),
            .InvalidContainerType => |e| std.debug.print("'{s}' is not a valid container type\n", .{
                src[e.start..e.end]
            }),
            .UnexpectedToken => |e| std.debug.print("Unexpected Token '{s}'\n", .{
                src[e.start..e.end]
            }),
        }
    }
}