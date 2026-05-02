const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

pub const Parser = @This();
const Self = @This();

alloc: std.mem.Allocator,
/// non owned slice, containing the raw source code
src: []const u8,
tokens: Ast.TokenList.Slice,
nodes: Ast.NodeList,
pos: Ast.TokenIndex = 0,
extra: std.ArrayList(u32),
scratch: std.ArrayList(Ast.NodeIndex),
errors: std.ArrayList(Ast.Error),

const Members = struct {
    len: usize,
    data: Ast.Node.Data,
    trailing: bool,

    pub fn toSpan(self: Members, parser: *Parser) !Ast.SubRange {
        _ = parser;
        return switch (self.len) {
            // todo cases for len 0..2
            else => self.data.extra_range,
        };
    }
};

fn tokenKind(self: *Self, idx: Ast.TokenIndex) Token.Kind {
    return self.tokens.items(.kind)[idx];
}

/// parse a file from the root
pub fn parseRoot(self: *Self) !void {
    self.nodes.appendAssumeCapacity(.{
        .kind = .root,
        .main_token = 0,
        .data = undefined,
    });
    const root_members = try self.parseContainerMembers();
    const root_decls = try root_members.toSpan(self);

    if (self.tokenKind(self.pos) != .eof) {
        // todo : emit error/warning that eof was expected but not reached
    }
    self.nodes.items(.data)[0] = .{ .extra_range = root_decls };
}

fn parseContainerMembers(self: *Self) !Members {
    _ = self;
    return error.todo;
}