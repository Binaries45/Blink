const std = @import("std");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Ast = @This();

/// all of the top level declaration nodes
root: []const *Node,
/// all errors encountered during parsing
errors: []const Parser.ParseError,

pub const Tag = enum {
    let,
    let_mut,
    fn_decl,
    param,
    field,
    /// an expression used as a statement
    expr,
    /// a public item declaration, can be a constant, or a function
    pub_item,
    ret_stmt,
    
    // literals
    literal_int,
    literal_float,
    literal_char,
    literal_string,
    literal_bool,
    ident,

    // conventional expressions
    unary,
    binary,
    /// function call
    call,
    /// call to a builtin function
    builtin_call,
    /// member access via the '.' operator
    access,
    /// a type expression for an array of some elements
    array_of,

    // types
    /// `struct { ... }`
    literal_struct,
    /// `enum { ... }`
    /// or
    /// `enum(Int) { ... }`
    literal_enum,
    /// `union { ... }`
    /// or
    /// `union (Tag) { ... }`
    literal_union,

    // control flow
    @"if",
    @"switch",
    @"for",
    @"while",
    loop,
    block,
    @"break",
    @"continue",
};

/// a statement node of the `Ast`
pub const Node = union(Tag) {
    // statements
    let: LetStmt,
    let_mut: LetMutStmt,
    fn_decl: FnStmt,
    param: ParamStmt,
    field: FieldStmt,
    expr: *Node,
    pub_item: *Node,
    ret_stmt: *Node,

    // exprs
    literal_int: Token,
    literal_float: Token,
    literal_char: Token,
    literal_string: Token,
    literal_bool: Token,
    ident: Token,
    unary: Unary,
    binary: Binary,
    call: Call,
    builtin_call: BuiltinCall,
    access: Access,
    array_of: ArrayOf,
    literal_struct: StructLit,
    literal_enum: EnumLit,
    literal_union: UnionLit,
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
        operand: *Node,
    };

    const Binary = struct {
        op: Token,
        left: *Node,
        right: *Node,
    };

    const Call = struct {
        name: Token,
        args: []const *Node,
    };

    const BuiltinCall = struct {
        name: Token,
        args: []const *Node,
    };

    const Access = struct {
        /// the expression yielding the container to access from
        container: *Node,
        /// the name of the member to access
        member: Token,
    };

    const ArrayOf = struct {
        elem: *Node,
    };

    const StructLit = struct {
        members: []const *Node,
    };

    const EnumLit = struct {
        members: []const *Node,
    };

    const UnionLit = struct {
        members: []const *Node,
    };

    const IfExpr = struct {
        clause: *Node,
        then_body: *Node,
        else_body: ?*Node,
    };

    const SwitchExpr = struct {
        // todo
    };

    const ForExpr = struct {
        capture: *Node,
        iterable: *Node,
        body: *Node,
    };

    const WhileExpr = struct {
        clause: *Node,
        body: *Node,
    };

    const LoopExpr = struct {
        body: *Node,
    };

    const BlockExpr = struct {
        // todo : label and any other stuff
        content: []const *Node,
    };

    const BreakExpr = struct {
        label: ?Token,
        value: ?*Node,
    };

    const ContinueExpr = struct {
        label: ?Token,
        value: ?*Node,
    };
    

    const LetStmt = struct {
        name: Token,
        type_expr: ?*Node,
        value_expr: *Node,
    };

    const LetMutStmt = struct {
        name: Token,
        type_expr: ?*Node,
        value_expr: *Node,
    };

    const FnStmt = struct {
        name: Token,
        params: []const *Node,
        ret_ty: *Node,
        body: *Node,
    };

    const ParamStmt = struct {
        name: Token,
        type_expr: *Node,
    };

    const FieldStmt = struct {
        name: Token,
        type_expr: ?*Node,
        default: ?*Node,
    };

    pub fn create(alloc: std.mem.Allocator, val: Node) *Node {
        const ptr = alloc.create(Node) catch unreachable;
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
