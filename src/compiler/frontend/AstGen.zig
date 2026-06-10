//! Consumes an AST to create BLIR code

const std = @import("std");
const Ast = @import("../frontend/Ast.zig");
const Blir = @import("../IR/Blir.zig");

const AstGen = @This();

alloc: std.mem.Allocator,
ast: *const Ast,
