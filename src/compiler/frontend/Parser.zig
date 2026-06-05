const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

const Parser = @This();

alloc: std.mem.Allocator,
/// non owned slice, containing the raw source code
src: []const u8,
tokens: []const Token,
pos: usize = 0,
errors: std.ArrayList(ParseError),

pub const ParseError = struct {
    err: Error,
    token: Token,
};

const Error = error {
    UnexpectedToken,
    ExpectedEof,
    ExpectedFn,
    ExpectedPubItem,
    InvalidContainerType,
    ExpectedIdentifier,
    ExpectedSemicolon,
};

/// return the next token to parse
fn next(p: *Parser) Token {
    const res = p.tokens[p.pos];
    p.pos += 1;
    return res;
}

fn peek(p: *Parser) Token {
    return p.tokens[p.pos];
}

/// advance the parser until the next decl
fn findNextDecl(p: *Parser) void {
    while (true) {
        switch (p.peek().kind) {
            // todo : this top arm may not be needed (except for the eof case)
            .eof, .@"fn", .@"pub", .let => return,
            .r_brace => { p.pos += 1; return; },
            else => p.pos += 1,
        }
    }
}

fn consume(p: *Parser, kind: Token.Kind) ?Token {
    return if (p.peek().kind == kind) p.next() else null;
}

fn reportError(p: *Parser, err: Error) void {
    const pe = ParseError {.err = err, .token = p.peek()};
    p.errors.append(p.alloc, pe) catch unreachable;
}

/// parse a file from the root, and return all top level nodes
pub fn parseRoot(p: *Parser) ![]const *Ast.Stmt {
    return p.parseContainerMembers();
}

fn parseContainerMembers(p: *Parser) []const *Ast.Stmt {
    var members = std.ArrayList(*Ast.Stmt).initCapacity(p.alloc, 0) catch unreachable;

    while (p.peek().kind != .r_brace and p.peek().kind != .eof) {
        const member = p.parseContainerMember() catch |err| {
            p.reportError(err);
            // todo : instead of breaking, skip to next decl and continue
            break;
        };
        members.append(p.alloc, member) catch unreachable;
    }

    return members.toOwnedSlice(p.alloc) catch unreachable;
}

fn parseContainerMember(p: *Parser) Error!*Ast.Stmt {
    const is_pub = p.consume(.@"pub") != null;

    const item = switch (p.peek().kind) {
        .identifier => try p.parseField(),
        .@"fn" => try p.parseFn(),
        .let => try p.parseLet(),
        else => return if (is_pub) error.ExpectedPubItem else error.UnexpectedToken,
    };

    return if (is_pub) Ast.Stmt.create(p.alloc, .{.pub_item = item}) else item;
}

fn parseStatement(p: *Parser) Error!*Ast.Stmt {
    return switch (p.peek().kind) {
        .let => try p.parseLet(),
        .@"fn" => try p.parseFn(),
        // defer
        // loops, switch, if, ... (these can be statements or exprs)
        // but maybe we just leave them to the else clause
        else => blk: {
            const expr = Ast.Stmt.create(p.alloc, .{ .expr = try p.parseExpr() });
            _ = p.consume(.semicolon) orelse return error.ExpectedSemicolon;
            break :blk expr;
        },
    };
}

fn parseField(p: *Parser) Error!*Ast.Stmt {
    const name = p.consume(.identifier) orelse return error.ExpectedIdentifier;
    _ = p.consume(.colon) orelse return error.UnexpectedToken;
    const ty = try p.parseTypeExpr();
    if (p.peek().kind != .r_brace and p.peek().kind != .eof) {
        _ = p.consume(.comma) orelse return error.UnexpectedToken;
    }

    return Ast.Stmt.create(p.alloc, .{.field = .{
        .name = name,
        .type_expr = ty,
    }});
}

fn parseLet(p: *Parser) Error!*Ast.Stmt {
    _ = p.consume(.let) orelse return error.UnexpectedToken;
    const mutable = if (p.peek().kind == .mut) blk: {
        _ = p.next();
        break :blk true;
    } else false;

    const name = p.consume(.identifier) orelse return error.ExpectedIdentifier;

    const type_expr = if (p.peek().kind == .colon) blk: {
        _ = p.next();
        break :blk try p.parseTypeExpr();
    } else null;

    _ = p.consume(.equal) orelse return error.UnexpectedToken;
    const value = try p.parseExpr();
    _ = p.consume(.semicolon) orelse return error.ExpectedSemicolon;

    return if (mutable) Ast.Stmt.create(p.alloc, .{ .let_mut = .{
        .name = name,
        .type_expr = type_expr,
        .value_expr = value,
    }}) else Ast.Stmt.create(p.alloc, .{ .let = .{
        .name = name,
        .type_expr = type_expr,
        .value_expr = value,
    }});
}

fn parseParamList(p: *Parser) Error![]const *Ast.Stmt {
    var params = std.ArrayList(*Ast.Stmt).initCapacity(p.alloc, 0) catch unreachable;
    defer params.deinit(p.alloc);

    while(p.peek().kind != .r_paren) {
        const name = p.consume(.identifier) orelse return error.ExpectedIdentifier;
        _ = p.consume(.colon);
        const ty = try p.parseTypeExpr();
        params.append(p.alloc, Ast.Stmt.create(p.alloc, .{ .param = .{
            .name = name,
            .type_expr = ty,
        }})) catch unreachable;
        if (p.peek().kind == .comma) _ = p.next();
    }

    return params.toOwnedSlice(p.alloc) catch unreachable;
}

fn parseFn(p: *Parser) Error!*Ast.Stmt {
    _ = p.consume(.@"fn") orelse return error.ExpectedFn;
    const name = p.consume(.identifier) orelse return error.ExpectedIdentifier;
    _ = p.consume(.l_paren) orelse return error.ExpectedFn;
    const params = try p.parseParamList();
    _ = p.consume(.r_paren) orelse return error.ExpectedFn;
    const ret_ty = try p.parseTypeExpr();
    _ = p.consume(.equal) orelse return error.UnexpectedToken;
    const body = try p.parseExpr();

    return Ast.Stmt.create(p.alloc, .{.fn_decl = .{
        .name = name,
        .params = params,
        .ret_ty = ret_ty,
        .body = body,
    }});
}

fn parseFnCall(p: *Parser) Error!*Ast.Expr {
    const name = p.consume(.identifier) orelse return error.ExpectedIdentifier;
    _ = p.consume(.l_paren);

    // todo : parse args
    var args = std.ArrayList(*Ast.Expr).initCapacity(p.alloc, 0) catch unreachable;
    while (p.peek().kind != .r_paren) {
        const expr = try p.parseExpr();
        args.append(p.alloc, expr) catch unreachable;
        if (p.peek().kind == .comma) _ = p.next();
    }

    _ = p.consume(.r_paren);
    return Ast.Expr.create(p.alloc, .{ .call = .{
        .name = name,
        .args = args.toOwnedSlice(p.alloc) catch unreachable,
    }});
}

fn parseExpr(p: *Parser) Error!*Ast.Expr {
    // switch on next tokens kind
    // numeric -> math expr
    // operator -> math or type Expr (type expr if '*' otherwise math)
    // keyword -> appropriate expression type
    // ident -> ambiguous, could be type or normal expr
    // builtin -> builtin fn call (we dont have these yet)
    // else -> Unexpected Token
    return switch (p.peek().kind) {
        .int_literal,
        .float_literal,
        .identifier,
        .asterisk,
        .minus,
        .plus,
        .l_paren => p.parsePrecedence(0),

        .@"struct",
        .@"enum",
        .@"union" => p.parseTypeExpr(),

        // todo : builtin calls, block expressions, others
        .l_brace => p.parseBlock(),

        else => error.UnexpectedToken,
    };
}

/// returns the precedence of a binary operator,
/// or null if the given token kind is not a valid operator
fn binaryPrecedence(kind: Token.Kind) ?u8 {
    return switch (kind) {
        .equal => 1,
        .pipe_pipe => 2,
        .ampersand_ampersand => 3,
        .pipe => 4,
        .caret => 5,
        .ampersand => 6,
        .equal_equal, .bang_equal => 7,
        .l_angled, .r_angled, .l_angled_equal, .r_angled_equal => 8,
        .l_angled_angled, .r_angled_angled => 9,
        .plus, .minus => 10,
        .asterisk, .slash, .percent => 11,
        // todo : since member access is its own expr,
        // we may want to rework this, or we can just have an if clause
        // for the .period kind
        .period => 255,
        else => null,
    };
}

fn unaryPrecedence(kind: Token.Kind) ?u8 {
    return switch (kind) {
        .plus,
        .minus,
        .asterisk,
        .ampersand,
        .bang,
        .tilde => 12,
        else => null,
    };
}

fn primary(p: *Parser) Error!*Ast.Expr {
    if (unaryPrecedence(p.peek().kind)) |_| {
        return Ast.Expr.create(p.alloc, .{ .unary = .{
            .op = p.next(),
            .operand = try p.primary(),
        }});
    }

    // todo : handle unary ops properly
    return state: switch(p.peek().kind) {
        .int_literal => Ast.Expr.create(p.alloc, .{ .literal_int = p.next() }),
        .float_literal => Ast.Expr.create(p.alloc, .{ .literal_float = p.next() }),
        .l_paren => {
            const expr = p.parsePrecedence(0);
            _ = p.consume(.r_paren) orelse return error.UnexpectedToken;
            break :state expr;
        },
        .identifier => {
            // todo : detect function calls
            if (p.tokens[p.pos + 1].kind == .l_paren) break :state p.parseFnCall();
            break :state Ast.Expr.create(p.alloc, .{ .ident = p.next() });
        },
        else => error.UnexpectedToken,
    };
}

fn parsePrecedence(p: *Parser, min_prec: u8) Error!*Ast.Expr {
    var lhs = try p.primary();

    while(true) {
        const current = p.peek();
        const prec = binaryPrecedence(current.kind) orelse break;
        if (prec < min_prec) break;
        _ = p.next();

        lhs = if (current.kind == .period) blk: {
            break :blk Ast.Expr.create(p.alloc,.{ .access = .{
                .container = lhs,
                .member = p.consume(.identifier) orelse return error.ExpectedIdentifier,
            }});
        } else blk: {
            break :blk Ast.Expr.create(p.alloc, .{ .binary = .{
                .op = current,
                .left = lhs,
                .right = try p.parsePrecedence(prec),
            }});
        };
    }

    return lhs;
}

fn parseTypeExpr(p: *Parser) Error!*Ast.Expr {
    return state: switch(p.peek().kind) {
        .question => Ast.Expr.create(p.alloc, .{ .unary = .{
            .op = p.next(),
            .operand = try p.parseTypeExpr(),
        }}),
        // todo : pointer modifiers like *const
        .asterisk => Ast.Expr.create(p.alloc, .{ .unary = .{
            .op = p.next(),
            .operand = try p.parseTypeExpr(),
        }}),
        .identifier => {
            // todo : comptime function call support
            const tok = p.next();
            var expr = Ast.Expr.create(p.alloc, .{ .ident = tok });
            while(p.peek().kind == .period) {
                _ = p.next();
                if(p.peek().kind != .identifier) break :state error.ExpectedIdentifier;
                expr = Ast.Expr.create(p.alloc, .{ .access = .{
                    .container = expr,
                    .member = p.next(),
                }});
            }
            break :state expr;
        },
        else => error.UnexpectedToken,
    };
}

fn parseBlock(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.l_brace) orelse return Error.UnexpectedToken;
    var statements = std.ArrayList(*Ast.Stmt).initCapacity(p.alloc, 0) catch unreachable;

    while(p.peek().kind != .r_brace) {
        const stmt = try p.parseStatement();
        statements.append(p.alloc, stmt) catch unreachable;
    }

    _ = p.consume(.r_brace) orelse return Error.UnexpectedToken;
    return Ast.Expr.create(p.alloc, .{ .block = .{
        .content = statements.toOwnedSlice(p.alloc) catch unreachable
    }});
}