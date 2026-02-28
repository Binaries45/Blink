const std = @import("std");
const AstNode = @import("../parsing/ast.zig").AstNode;
const Type = @import("../types/type.zig").Type;

pub const Symbol = struct {
    kind: SymbolKind,
    type: *Type,
    node: *AstNode,

    pub const SymbolKind = enum {
        constant,
        variable,
        function,
        type,
    };

    pub fn format(self: Symbol, writer: *std.io.Writer) !void {
        try writer.print("{s} {f}", .{ @tagName(self.kind), self.type.* });
    }
};