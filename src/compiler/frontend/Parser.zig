const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

pub const Parser = @This();
const Self = @This();

alloc: std.mem.Allocator,
/// non owned slice, containing the raw source code
src: []const u8,
/// non owned slice, containing the tokens
tokens: Ast.TokenList.Slice,
pos: Ast.TokenIndex = 0,
