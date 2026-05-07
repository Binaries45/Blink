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

pub const ParseError = error {
    OutOfMemory,
    ExpectedEof,
};

const Members = struct {
    len: usize,
    /// is either .opt_node_opt_node or .extra_range
    data: Ast.Node.Data,
    trailing: bool,

    pub fn toRange(self: Members, parser: *Parser) ParseError!Ast.SubRange {
        return switch (self.len) {
            // todo cases for len 0..2
            0 => parser.listToRange(&.{}),
            1 => parser.listToRange(&.{
                self.data.opt_node_opt_node.@"0".unwrap().?
            }),
            2 => parser.listToRange(&.{
                self.data.opt_node_opt_node.@"0".unwrap().?,
                self.data.opt_node_opt_node.@"1".unwrap().?
            }),
            else => self.data.extra_range,
        };
    }
};

/// save a list of nodes to extra data, and return a SubRange over those nodes
fn listToRange(self: *Self, nodes: []const Ast.NodeIndex) ParseError!Ast.SubRange {
    try self.extra.appendSlice(self.alloc, @ptrCast(nodes));
    return .{
        .start = @intCast(self.extra.items.len - nodes.len),
        .end = @intCast(self.extra.items.len),
    };
}

fn tokenKind(self: *Self, idx: Ast.TokenIndex) Token.Kind {
    return self.tokens.items(.kind)[idx];
}

/// expect a token, if it is next in the stream return it and advance,
/// otherwise return null
fn consume(self: *Self, token_kind: Token.Kind) ?Token {
    if (self.tokens.items(.kind)[self.pos] != token_kind) return null;
    self.tokens.get(self.pos);
}

/// parse a file from the root
pub fn parseRoot(self: *Self) ParseError!void {
    self.nodes.appendAssumeCapacity(.{
        .kind = .root,
        .main_token = 0,
        .data = undefined,
    });
    const root_members = try self.parseContainerMembers();
    const root_decls = try root_members.toRange(self);

    if (self.tokenKind(self.pos) != .eof) return ParseError.ExpectedEof;

    self.nodes.items(.data)[0] = .{ .extra_range = root_decls };
}

fn parseContainerMembers(self: *Self) ParseError!Members {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);

    var field_state: union(enum) {
        /// no fields have been seen
        none,
        /// currently parsing fields
        seen,
        /// saw fields, and then a declaration after
        /// payload is the first token of the previous declaration
        end: Ast.TokenIndex,
        /// there was a declaration between fields
        err,
    } = .none;

    // todo : make var when we actually use it
    const last_field: Ast.TokenIndex = undefined;
    _ = last_field;
    var trailing: bool = false;
    // todo : skip all leading comments if they exist

    while (true) {
        // skip comments
        sw: switch (self.tokenKind(self.pos)) {
            // outermost container items
            .@"pub", .@"fn", .@"inline", .@"const" => |t| {
                if (t == .@"inline") {
                    switch (self.tokenKind(self.pos + 1)) {
                        .@"for", .@"while" => |ct| continue :sw ct,
                        else => {},
                    }
                }

                self.pos += @intFromBool(t == .@"pub");
                const decl = try self.parseTopLevelDecl();
                if (decl) |d| {
                    if (field_state == .seen) field_state = .{ .end = d };
                    try self.scratch.append(self.alloc, d);
                }
                trailing = self.tokenKind(self.pos - 1) == .semicolon;
            },
            .eof, .r_brace => {
                // todo : if theres a doc comment,
                //  emit a warning that it has nothing to attach to
                //  (this will only be needed when we support comments fully)
                break;
            },
            else => {
                // todo : this
            }
        }
    }

    const items = self.scratch.items[scratch_top..];

    if (items.len <= 2) {
        return Members {
            .len = items.len,
            .data = .{ .opt_node_opt_node = .{
                if (items.len >= 1) Ast.OptionalNodeIndex.fromOptional(items[0])
                else Ast.OptionalNodeIndex.none,
                if (items.len >= 2) Ast.OptionalNodeIndex.fromOptional(items[1])
                else Ast.OptionalNodeIndex.none,
            }},
            .trailing = trailing,
        };
    }
    return Members {
        .len = items.len,
        .data = .{ .extra_range = try self.listToRange(items) },
        .trailing = trailing,
    };
}

fn parseTopLevelDecl(self: *Self) ParseError!?Ast.NodeIndex {
    _ = self;
    return error.OutOfMemory;
}
