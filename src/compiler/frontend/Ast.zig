const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Ast = @This();

/// all of the top level nodes
root: []const Node,
/// all errors encountered during parsing
errors: []const Parser.ParseError,

pub const Node = union(enum) {

};

// pub const Node = struct {
//     kind: Kind,
//     main_token: TokenIndex,
//     data: Data,
//
//     comptime {
//         std.testing.expect(@sizeOf(Kind) == 1)
//             catch @compileError("Kind is larger than one byte");
//
//         if (!std.debug.runtime_safety) {
//             std.testing.expect(@sizeOf(Data) == 8)
//                 catch @compileError("Data is larger than 8 bytes");
//         }
//     }
//
//     pub const Kind = enum {
//         /// the root of the file, guaranteed to be at `NodeIndex` 0
//         root,
//
//         /// data is unused, value stored in main_token
//         literal_int,
//         /// data is unused, value stored in main_token
//         literal_float,
//         /// data is unused, value stored in main_token
//         literal_char,
//         /// data is unused, value stored in main_token
//         literal_string,
//         /// data is unused, value stored in main_token
//         literal_bool,
//
//         /// data is unused, name stored in main_token
//         expr_ident,
//         /// data is .node_node
//         expr_binary,
//         /// data is .node
//         expr_unary,
//         /// data is .node_node, field one is condition, field two is body
//         expr_if_simple,
//         /// data is .node_node, field one is condition,
//         /// field two is extra, holding the then and else bodies
//         expr_if_else,
//         /// data is .node_node, field one is extra holding the iterator and the capture,
//         /// field two is the body
//         expr_for,
//         /// data is .node_node, field one is condition, field two is body
//         expr_while,
//         /// data is .node, pointing to the body
//         expr_loop,
//         /// data is .node_node, field one is the value being switched on,
//         /// field two is extra holding the cases
//         expr_switch,
//         /// data is .extra_opt_node, with extra being a SubRange of the args,
//         /// and node is the callee
//         expr_call,
//         /// data is .node, holding the inner expression
//         expr_grouped,
//         /// data is .extra_range, holding the contents of the block
//         expr_block,
//
//         /// data is .opt_token_opt_node, with the token being the optional
//         /// label to break, and the node being the value to break out with
//         stmt_break,
//         /// data is .opt_token_opt_node, with the token being the optional
//         /// label to break, and the node being the value to break out with
//         stmt_continue,
//         /// data is .node, holding the value to be returned
//         stmt_return,
//
//         /// data is .opt_node_opt_node, field one is the first parameter,
//         /// if it exists, and field two is the return type expression,
//         /// if it exists
//         ///
//         /// main_token is the `fn` keyword token
//         fn_proto_simple,
//         /// data is .extra_opt_node, extra is an index to a SubRange
//         /// containing each parameter type expression, with the optional
//         /// node being the return type expression, if it exists
//         ///
//         /// main_token is the `fn` keyword token
//         fn_proto_multi,
//         /// data is .extra_opt_node, extra is a index into the actual fn_proto_*,
//         /// and the opt node is the return type expr, if this is null,
//         /// the return type is unspecified and therefore void.
//         ///
//         /// main_token is the `fn` keyword token
//         fn_proto,
//
//
//         /// data is .opt_node_node, field one is an optional type expression,
//         /// field two is the initial value
//         decl_const,
//         /// data is .opt_node_node, field one is an optional type expression,
//         /// field two is the initial value
//         decl_let,
//         /// data is .opt_node_node, field one is an optional type expression,
//         /// field two is the initial value
//         decl_let_mut,
//         /// data is .node_node, field one is the fn_proto_*, field two is the body
//         decl_fn,
//
//         // todo : type exprs
//         // todo : patterns
//     };
//
//     pub const Data = union {
//         token: TokenIndex,
//         opt_token: OptionalTokenIndex,
//
//         node: NodeIndex,
//         opt_node: OptionalNodeIndex,
//
//         extra: ExtraIndex,
//         extra_range: SubRange,
//         extra_opt_node: struct { ExtraIndex, OptionalNodeIndex },
//
//         node_node: struct { NodeIndex, NodeIndex },
//         opt_node_node: struct { OptionalNodeIndex, NodeIndex },
//         opt_node_opt_node: struct { OptionalNodeIndex, OptionalNodeIndex },
//
//         opt_token_opt_node: struct { OptionalTokenIndex, OptionalNodeIndex },
//     };
// };

/// parse an ast from the given source
pub fn parse(alloc: std.mem.Allocator, src: [:0]const u8) !Ast {
    var tokens = std.ArrayList(Token);
    defer tokens.deinit(alloc);

    const estimated_tokens = src.len / 8;
    try tokens.ensureTotalCapacity(alloc, estimated_tokens);
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        try tokens.append(alloc, token);
        if (token.kind == .eof) break;
    }

    var token_slice = tokens.toOwnedSlice();
    errdefer token_slice.deinit(alloc);

    return parseTokens(alloc, src, token_slice);
}

pub fn parseTokens(alloc: std.mem.Allocator, src: [:0]const u8, tokens: []const Token) !Ast {
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

    const root = try parser.parseRoot();

    return .{
        .errors = parser.errors.toOwnedSlice(alloc),
        .root = root,
    };
}