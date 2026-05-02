const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

pub const Ast = @This();

src: [:0]const u8,
tokens: TokenList.Slice,
nodes: NodeList.Slice,
extra: std.ArrayList(NodeIndex),
errors: []const Error,

pub const TokenIndex = u32;
pub const OptionalTokenIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn unwrap(self: @This()) ?TokenIndex {
        return if (self == .none) null else @intFromEnum(self);
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
        return if (self == .none) null else @intFromEnum(self);
    }

    pub fn fromOptional(idx: ?NodeIndex) @This() {
        return if (idx) |i| @enumFromInt(i) else .none;
    }
};

pub const ExtraIndex = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Error = struct {
    kind: Kind,

    pub const Kind = enum {
        invalid,
    };
};

pub const Node = struct {
    kind: Kind,
    main_token: TokenIndex,
    data: Data,

    comptime {
        std.testing.expect(@sizeOf(Kind) == 1)
            catch @compileError("Kind is larger than one byte");

        if (!std.debug.runtime_safety) {
            std.testing.expect(@sizeOf(Data) == 8)
                catch @compileError("Data is larger than 8 bytes");
        }
    }

    pub const Kind = enum {
        /// the root of the file, guaranteed to be at `NodeIndex` 0
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
        /// data is .node_node, field one is condition, field two is body
        expr_if_simple,
        /// data is .node_node, field one is condition,
        /// field two is extra, holding the then and else bodies
        expr_if_else,
        /// data is .node_node, field one is extra holding the iterator and the capture,
        /// field two is the body
        expr_for,
        /// data is .node_node, field one is condition, field two is body
        expr_while,
        /// data is .node, pointing to the body
        expr_loop,
        /// data is .node_node, field one is the value being switched on,
        /// field two is extra holding the cases
        expr_switch,
        /// data is .extra_opt_node, with extra being a SubRange of the args,
        /// and node is the callee
        expr_call,
        /// data is .node, holding the inner expression
        expr_grouped,
        /// data is .extra_range, holding the contents of the block
        expr_block,

        /// data is .opt_token_opt_node, with the token being the optional
        /// label to break, and the node being the value to break out with
        stmt_break,
        /// data is .opt_token_opt_node, with the token being the optional
        /// label to break, and the node being the value to break out with
        stmt_continue,
        /// data is .node, holding the value to be returned
        stmt_return,

        /// data is .opt_node_opt_node, field one is the first parameter,
        /// if it exists, and field two is the return type expression,
        /// if it exists
        ///
        /// main_token is the `fn` keyword token
        fn_proto_simple,
        /// data is .extra_opt_node, extra is an index to a SubRange
        /// containing each parameter type expression, with the optional
        /// node being the return type expression, if it exists
        ///
        /// main_token is the `fn` keyword token
        fn_proto_multi,


        /// data is .opt_node_node, field one is an optional type expression,
        /// field two is the initial value
        decl_const,
        /// data is .opt_node_node, field one is an optional type expression,
        /// field two is the initial value
        decl_let,
        /// data is .opt_node_node, field one is an optional type expression,
        /// field two is the initial value
        decl_let_mut,
        /// data is .node_node, field one is the fn_proto_*, field two is the body
        decl_fn,

        // todo : type exprs
        // todo : patterns
    };

    pub const Data = union {
        token: TokenIndex,
        opt_token: OptionalTokenIndex,

        node: NodeIndex,
        opt_node: OptionalNodeIndex,

        extra: ExtraIndex,
        extra_range: SubRange,
        extra_opt_node: struct { ExtraIndex, OptionalNodeIndex },

        node_node: struct { NodeIndex, NodeIndex },
        opt_node_node: struct { OptionalNodeIndex, NodeIndex },
        opt_node_opt_node: struct { OptionalNodeIndex, OptionalNodeIndex },

        opt_token_opt_node: struct { OptionalTokenIndex, OptionalNodeIndex },
    };
};

/// get the kind of the node located at `idx`
pub fn nodeKind(self: *const Ast, idx: NodeIndex) Node.Kind {
    return self.nodes.items(.kind)[idx];
}

/// get the index of the main token of the node located at `idx`
pub fn nodeToken(self: *const Ast, idx: NodeIndex) TokenIndex {
    return self.nodes.items(.main_token)[idx];
}

/// get the data of the node located at `idx`
pub fn nodeData(self: *const Ast, idx: NodeIndex) Node.Data {
    return self.nodes.items(.data)[idx];
}

/// get any extra data for the node located at `idx`, if the payload stored
/// does not refer to extra data, null is returned instead
pub fn extraData(self: *const Ast, comptime T: type, index: NodeIndex) T {
    var result: T = undefined;

    const fields = std.meta.fields(T);

    inline for (fields, 0..) |f, i| {
        @field(result, f.name) = self.extra.items[index + i];
    }

    return result;
}

pub fn extraRange(self: *const Ast, range: SubRange) []const NodeIndex {
    return self.extra[range.start..range.end];
}

/// get the name of a function from its index,
/// returns null if the function does not have a name
///
/// assumes that `idx` is the index of some `fn_proto_*`
pub fn fnProtoName(self: *const Ast, idx: NodeIndex) ?TokenIndex {
    const tok = self.nodeToken(idx) + 1;
    if (self.tokens.items(.kind)[tok] != .identifier) return null;
    return tok;
}

pub const SubRange = struct {
    start: ExtraIndex,
    end: ExtraIndex,
};

pub const If = struct {
    then: NodeIndex,
    @"else": NodeIndex,
};

// todo : other payload types

pub fn parse(alloc: std.mem.Allocator, src: [:0]const u8) !Ast {
    // build the token list
    var tokens = Ast.TokenList{};
    defer tokens.deinit(alloc);

    const estimated_tokens = src.len / 8;
    try tokens.ensureTotalCapacity(alloc, estimated_tokens);
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        try tokens.append(alloc, token);
        if (token.kind == .eof) break;
    }

    var token_slice = try tokens.toOwnedSlice();
    errdefer token_slice.deinit(alloc);

    // parse it
    return parseTokens(alloc, src, token_slice);
}

fn parseTokens(alloc: std.mem.Allocator, src: [:0]const u8, tokens: Ast.TokenList.Slice) !Ast {
    var parser: Parser = .{
        .alloc = alloc,
        .src = src,
        .tokens = tokens,
        .errors = .empty,
        .extra = .empty,
        .nodes = .empty,
        .scratch = .empty,
    };

    defer parser.errors.deinit(alloc);
    defer parser.extra.deinit(alloc);
    defer parser.nodes.deinit(alloc);
    defer parser.scratch.deinit(alloc);

    const estimated_nodes = tokens.len / 2 + 1;
    try parser.nodes.ensureTotalCapacity(alloc, estimated_nodes);

    parser.parseRoot();

    try parser.errors.shrinkRetainingCapacity(parser.errors.items.len);
    try parser.extra.shrinkRetainingCapacity(parser.extra.items.len);

    return .{
        .src = src,
        .tokens = tokens,
        .nodes = try parser.nodes.toOwnedSlice(),
        .extra = try parser.extra.toOwnedSlice(alloc),
        .errors = try parser.errors.toOwnedSlice(alloc),
    };
}