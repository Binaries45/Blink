const std = @import("std");
const Token = @import("Token.zig");

pub const Ast = @This();

src: []const u8,
tokens: TokenList,
nodes: NodeList,
extra: std.ArrayList(NodeIndex),

pub const TokenIndex = u32;
pub const OptionalTokenIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn unwrap(self: @This()) ?TokenIndex {
        if (self == .none) null else @intFromEnum(self);
    }

    pub fn fromOptional(idx: ?TokenIndex) @This() {
        return if (idx) |i| @enumFromInt(i) else .none;
    }
};

pub const NodeIndex = u32;
pub const OptionalNodeIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn unwrap(self: @This()) ?NodeIndex {
        if (self == .none) null else @intFromEnum(self);
    }

    pub fn fromOptional(idx: ?NodeIndex) @This() {
        return if (idx) |i| @enumFromInt(i) else .none;
    }
};

pub const ExtraIndex = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Node = struct {
    kind: Kind,
    main_token: TokenIndex,
    data: Data,

    comptime {
        std.testing.assert(@sizeOf(Kind) == 1);
        std.testing.assert(@sizeOf(Data) == 8);
    }

    pub const Kind = enum {
        /// the root of the file
        root,

        /// data is unused, value stored in main_token
        literal_int,
        /// data is unused, value stored in main_token
        literal_float,
        /// data is unused, value stored in main_token
        literal_char,
        /// data is unused, value stored in main_token
        literal_string,
        /// data is unused, value stored in main_token
        literal_bool,

        /// data is unused, name stored in main_token
        expr_ident,
        /// data is .node_node
        expr_binary,
        /// data is .node
        expr_unary,
        /// data is .node_node, left is condition, right is body
        expr_if_simple,
        /// data is .node_node, left is condition,
        /// right is extra, holding the then and else bodies
        expr_if_else,
        /// data is .node_node, left is extra holding the iterator and the capture,
        /// right is the body
        expr_for,
        /// data is .node_node, left is condition, right is body
        expr_while,
        /// data is .node, pointing to the body
        expr_loop,
        /// data is .node_node, left is the value being switched on,
        /// right is extra holding the cases
        expr_switch,

        /// data is .opt_token_opt_node, with the token being the optional
        /// label to break, and the node being the value to break out with
        stmt_break,
        /// data is .opt_token_opt_node, with the token being the optional
        /// label to break, and the node being the value to break out with
        stmt_continue,
        /// data is .node, holding the value to be returned
        stmt_return,

        /// data is .opt_node_opt_node, left is the first parameter,
        /// if it exists, and right is the return type expression,
        /// if it exists
        proto_fn_simple,

        /// data is .opt_node_node, left is an optional type expression,
        /// right is the initial value
        decl_const,
        /// data is .opt_node_node, left is an optional type expression,
        /// right is the initial value
        decl_let,
        /// data is .opt_node_node, left is an optional type expression,
        /// right is the initial value
        decl_let_mut,
        /// data is .node_node, left is the fn_proto_*, right is the body
        decl_fn,
    };

    pub const Data = union {
        token: TokenIndex,
        opt_token: OptionalTokenIndex,

        node: NodeIndex,
        opt_node: OptionalNodeIndex,

        extra: ExtraIndex,

        node_node: struct { NodeIndex, NodeIndex },
        opt_node_node: struct { OptionalNodeIndex, NodeIndex },
        opt_token_opt_node: struct { OptionalTokenIndex, OptionalNodeIndex },
    };
};

/// get the kind of the node located at `idx`
pub fn nodeKind(self: Ast, idx: NodeIndex) Node.Kind {
    return self.nodes.items(.kind)[idx];
}

/// get the index of the main token of the node located at `idx`
pub fn nodeToken(self: Ast, idx: NodeIndex) TokenIndex {
    return self.nodes.items(.main_token)[idx];
}

/// get the data of the node located at `idx`
pub fn nodeData(self: Ast, idx: NodeIndex) Node.Data {
    return self.nodes.items(.data)[idx];
}