const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Ast = @This();

/// all of the top level declaration nodes
root: []const *Stmt,
/// all errors encountered during parsing
errors: []const Parser.ParseError,

/// a statement node of the `Ast`
pub const Stmt = union(enum) {
    let: LetStmt,
    let_mut: LetMutStmt,
    fn_decl: FnStmt,
    /// an expression used as a statement
    expr: *Expr,
    /// a public item declaration, can be a constant, or a function
    pub_item: *Stmt,

    const LetStmt = struct {
        // todo
    };

    const LetMutStmt = struct {
        // todo
    };

    const FnStmt = struct {
        // todo
    };
};

/// an expression node of the `Ast`
pub const Expr = union(enum) {
    // literals
    literal_int: Token,
    literal_float: Token,
    literal_char: Token,
    literal_string: Token,
    literal_bool: Token,
    ident: Token,

    // conventional expressions
    unary: Unary,
    binary: Binary,
    /// function call
    call: Call,
    /// call to a builtin function
    builtin_call: BuiltinCall,
    /// member access via the '.' operator
    access: Access,

    // types
    /// `struct { ... }`
    literal_struct: StructLit,
    /// `enum { ... }`
    /// or
    /// `enum(Int) { ... }`
    literal_enum: EnumLit,
    /// `union { ... }`
    /// or
    /// `union (Tag) { ... }`
    literal_union: UnionLit,
    // todo:  packed types `packed(Int) type_expr`
    /// `todo : trait syntax`
    literal_trait: TraitLit,

    // control flow
    @"if": IfExpr,
    @"switch": SwitchExpr,
    @"for": ForExpr,
    @"while": WhileExpr,
    @"loop": LoopExpr,
    block: BlockExpr,

    const Unary = struct {
        op: Token,
        operand: *Expr,
    };

    const Binary = struct {
        op: Token,
        left: *Expr,
        right: *Expr,
    };

    const Call = struct {
        name: Token,
        args: []const *Expr,
    };

    const BuiltinCall = struct {
        name: Token,
        args: []const *Expr,
    };

    const Access = struct {
        /// the expression yielding the container to access from
        container: *Expr,
        /// the name of the member to access
        member: Token,
    };

    const StructLit = struct {
        // todo
    };

    const EnumLit = struct {
        // todo
    };

    const UnionLit = struct {
        // todo
    };

    const TraitLit = struct {
        // todo
    };

    const IfExpr = struct {
        // todo
    };

    const SwitchExpr = struct {
        // todo
    };

    const ForExpr = struct {
        // todo
    };

    const WhileExpr = struct {
        // todo
    };

    const LoopExpr = struct {
        // todo
    };

    const BlockExpr = struct {
        // todo
    };
};

/// parse an ast from the given source
pub fn parse(alloc: std.mem.Allocator, src: [:0]const u8) !Ast {
    var tokens = try std.ArrayList(Token).initCapacity(alloc, 0);
    defer tokens.deinit(alloc);

    const estimated_tokens = src.len / 8;
    try tokens.ensureTotalCapacity(alloc, estimated_tokens);
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        try tokens.append(alloc, token);
        if (token.kind == .eof) break;
    }

    const token_slice = try tokens.toOwnedSlice(alloc);
    errdefer alloc.free(token_slice);

    return parseTokens(alloc, src, token_slice);
}

pub fn parseTokens(alloc: std.mem.Allocator, src: [:0]const u8, tokens: []const Token) !Ast {
    var parser: Parser = .{
        .alloc = alloc,
        .src = src,
        .tokens = tokens,
        .errors = .empty,
    };
    defer parser.errors.deinit(alloc);

    const root = try parser.parseRoot();

    return .{
        .errors = try parser.errors.toOwnedSlice(alloc),
        .root = root,
    };
}