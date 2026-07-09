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
    param: ParamStmt,
    field: FieldStmt,
    /// an expression used as a statement
    expr: *Expr,
    /// a public item declaration, can be a constant, or a function
    pub_item: *Stmt,

    const LetStmt = struct {
        name: Token,
        type_expr: ?*Expr,
        value_expr: *Expr,
    };

    const LetMutStmt = struct {
        name: Token,
        type_expr: ?*Expr,
        value_expr: *Expr,
    };

    const FnStmt = struct {
        name: Token,
        params: []const *Stmt,
        ret_ty: *Expr,
        body: *Expr,
    };

    const ParamStmt = struct {
        name: Token,
        type_expr: *Expr,
    };

    const FieldStmt = struct {
        name: Token,
        type_expr: ?*Expr,
        default: ?*Expr,
    };

    pub fn create(alloc: std.mem.Allocator, val: Stmt) *Stmt {
        const ptr = alloc.create(Stmt) catch unreachable;
        ptr.* = val;
        return ptr;
    }
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
    /// a type expression for an array of some elements
    array_of: ArrayOf,

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

    // control flow
    @"if": IfExpr,
    @"switch": SwitchExpr,
    @"for": ForExpr,
    @"while": WhileExpr,
    loop: LoopExpr,
    block: BlockExpr,
    @"break": BreakExpr,
    @"continue": ContinueExpr,

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

    const ArrayOf = struct {
        elem: *Expr,
    };

    const StructLit = struct {
        members: []const *Stmt,
    };

    const EnumLit = struct {
        members: []const *Stmt,
    };

    const UnionLit = struct {
        members: []const *Stmt,
    };

    const IfExpr = struct {
        clause: *Expr,
        then_body: *Expr,
        else_body: ?*Expr,
    };

    const SwitchExpr = struct {
        // todo
    };

    const ForExpr = struct {
        capture: *Expr,
        iterable: *Expr,
        body: *Expr,
    };

    const WhileExpr = struct {
        clause: *Expr,
        body: *Expr,
    };

    const LoopExpr = struct {
        body: *Expr,
    };

    const BlockExpr = struct {
        // todo : label and any other stuff
        content: []const *Stmt,
    };

    const BreakExpr = struct {
        label: ?Token,
        value: ?*Expr,
    };

    const ContinueExpr = struct {
        label: ?Token,
        value: ?*Expr,
    };

    pub fn create(alloc: std.mem.Allocator, val: Expr) *Expr {
        const ptr = alloc.create(Expr) catch unreachable;
        ptr.* = val;
        return ptr;
    }
};

pub fn deinit(ast: *Ast, alloc: std.mem.Allocator) void {
    alloc.free(ast.errors);
    // todo : this does need to recursively free all nodes
    alloc.free(ast.root);
}

/// parse an ast from the given source
pub fn parse(alloc: std.mem.Allocator, src: [:0]const u8) !Ast {
    var tokens = try std.ArrayList(Token).initCapacity(alloc, 0);
    defer tokens.deinit(alloc);

    const estimated_tokens = src.len / 8;
    try tokens.ensureTotalCapacity(alloc, estimated_tokens);
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        // TODO : actually handle doc comments,
        //        there will be build modes that output documentation,
        //        and eventually an lsp will need these as context
        if (token.kind == .doc_comment or token.kind == .container_doc_comment)
            continue;
        try tokens.append(alloc, token);
        if (token.kind == .eof) break;
    }

    const token_slice = try tokens.toOwnedSlice(alloc);
    defer alloc.free(token_slice);

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
