const std = @import("std");
const Ast = @import("ast.zig");

pub const Parser = @This();

alloc: std.mem.Allocator,