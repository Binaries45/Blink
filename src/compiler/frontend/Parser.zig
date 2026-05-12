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
    ExpectedFn,
    ExpectedPubItem,
    InvalidContainerType,
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

fn nodeKind(self: *Self, idx: Ast.NodeIndex) Ast.Node.Kind {
    return self.nodes.items(.kind)[idx];
}

fn nodeMainToken(self: *Self, idx: Ast.NodeIndex) Ast.TokenIndex {
    return self.nodes.items(.main_token)[idx];
}

fn nodeData(self: *Self, idx: Ast.NodeIndex) Ast.Node.Data {
    return self.nodes.items(.data)[idx];
}

fn next(self: *Self) Ast.TokenIndex {
    const result = self.pos;
    self.pos += 1;
    return result;
}

fn addNode(self: *Self, node: Ast.Node) !Ast.NodeIndex {
    const result = self.nodes.len;
    self.nodes.append(self.alloc, node);
    return result;
}

fn reserveNode(self: *Self, kind: Ast.Node.Kind) !Ast.NodeIndex {
    try self.nodes.resize(self.alloc, self.nodes.len + 1);
    self.nodes.items(.kind)[self.nodes.len - 1] = kind;
    return @intCast(self.nodes.len - 1);
}

fn unreserveNode(self: *Self, idx: Ast.NodeIndex) void {
    if (idx == self.nodes.len) {
        self.nodes.resize(self.alloc, self.nodes.len - 1) catch unreachable;
    } else {
        // todo : mark this node as unreachable (or a no-op)
        //  so that it can be caught as an error later
    }
}

fn setNode(self: *Self, idx: usize, node: Ast.Node) Ast.NodeIndex {
    self.nodes.set(idx, node);
    return @intCast(idx);
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
    // todo : skip all leading comments if they exist

    var trailing: bool = false;
    while (true) {
        // consume any doc comments and save for later
        sw: switch (self.tokenKind(self.pos)) {
            // outermost container items
            .@"pub", .@"inline", .@"const", .@"fn" => |t| {
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
    // keyword like `inline`
    const modifier_token = self.next();
    var expect_fn: bool = false;

    switch(self.tokenKind(modifier_token)) {
        .@"inline" => expect_fn = true,
        else => self.pos -= 1,
    }

    const opt_fn_proto = try self.parseFnProto();
    if (opt_fn_proto) |fn_proto| {
        switch(self.tokenKind(self.pos)) {
            .l_brace => {
                const fn_decl = try self.reserveNode(.decl_fn);
                errdefer self.unreserveNode(fn_decl);

                const body = try self.parseExpr();
                return self.setNode(fn_decl, .{
                    .kind = .decl_fn,
                    .main_token = self.nodeMainToken(fn_proto),
                    .data = .{ .node_node = .{
                        fn_proto,
                        body,
                    }}
                });
            },
            else => {
                // todo : emit warning that we expected  {
                return null;
            }
        }
    }

    if (expect_fn) return error.ExpectedFn;
    return error.ExpectedPubItem;
}

fn parseFnProto(self: *Self) ParseError!?Ast.NodeIndex {
    _ = self;
    // todo : parse the fn proto into its appropriate proto kind.
    return error.OutOfMemory;
}

fn parseExpr(self: *Self) ParseError!Ast.NodeIndex {
    _ = self;
    // todo check for expr and dispatch, if we see a block we treat it as such,
    //  otherwise parse with precedence climbing.
    return error.OutOfMemory;
}

/// parse a container expr,
///
/// ex.
/// ```
/// struct { ... }
/// enum { ... }
/// union { ... }
/// trait { ... }
/// ```
fn parseContainer(self: *Self) ParseError!void {
    const main_token = self.pos;
    switch (self.tokenKind(main_token)) {
        .@"struct", .@"enum", .trait => {}, // todo : add union keyword
        else => return error.InvalidContainerType,
    }

    _ = try self.consume(.l_brace);
    _ = try self.parseContainerMembers();
    _ = try self.consume(.r_brace);
    // todo : maybe we expect a semicolon, but if we have syntax like
    // const T = struct { ... };
    // then it makes more sense for the cont decl parser to expect the semicolon.
}