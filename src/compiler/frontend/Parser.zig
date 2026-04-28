const std = @import("std");
const Ast = @import("ast.zig");
const Token = @import("Token.zig");

pub const Parser = @This();
const Self = @This();

alloc: std.mem.Allocator,
/// non owned slice, containing the raw source code
src: []const u8,
/// non owned slice, containing the tokens
tokens: []const Token,
pos: Ast.TokenIndex = 0,

pub fn init(alloc: std.mem.Allocator, src: []const u8, tokens: []const Token) Self {
    return .{
        .alloc = alloc,
        .src = src,
        .tokens = tokens,
    };
}

pub fn parse(self: *Self) !Ast {
    _ = self;
    return error.todo;
}