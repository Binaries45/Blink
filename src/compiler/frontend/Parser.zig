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
    ExpectedTypeExpression,
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
    var level: u32 = 0;
    while (true) {
        const tok = p.next();
        switch(tok.kind) {
            .@"pub", .@"fn", .let => {
                if (level == 0) {
                    p.pos -= 1;
                    return;
                }
            },
            .identifier => {
                if (p.peek().kind == .comma and level == 0) {
                    p.pos -= 1;
                    return;
                }
            },
            .comma, .semicolon => if (level == 0) return,
            .l_paren, .l_bracket, .l_brace => level += 1,
            .r_paren, .r_bracket => if (level != 0) { level -= 1; },
            .r_brace => {
                if (level == 0) {
                    p.pos -= 1;
                    return;
                }
                level -= 1;
            },
            .eof => {
                p.pos -= 1;
                return;
            },
            else => {},
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
            p.findNextDecl();
            continue;
            // break;
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
    const ty = if (p.peek().kind == .colon) blk: {
        _ = p.consume(.colon) orelse return error.UnexpectedToken;
        break :blk try p.parseTypeExpr();
    } else null;

    const default = if (p.peek().kind == .equal) blk: {
        _ = p.consume(.equal) orelse return error.UnexpectedToken;
        break :blk try p.parseExpr();
    } else null;

    if (p.peek().kind != .r_brace and p.peek().kind != .eof) {
        _ = p.consume(.comma) orelse return error.UnexpectedToken;
    }

    return Ast.Stmt.create(p.alloc, .{.field = .{
        .name = name,
        .type_expr = ty,
        .default = default,
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

        .@"if" => p.parseIf(),
        .loop => p.parseLoop(),
        .@"while" => p.parseWhile(),
        .@"for" => p.parseFor(),
        .@"break" => p.parseBreak(),
        .@"continue" => p.parseContinue(),

        .true, .false => Ast.Expr.create(p.alloc, .{ .literal_bool = p.next() }),

        else => error.UnexpectedToken,
    };
}

/// returns the precedence of a binary operator,
/// or null if the given token kind is not a valid operator
fn binaryPrecedence(kind: Token.Kind) ?u8 {
    return switch (kind) {
        .equal, .ampersand_equal, .asterisk_equal,
        .carat_equal, .l_angled_angled_equal,
        .minus_equal, .percent_equal, .pipe_equal,
        .plus_equal, .r_angled_angled_equal, .slash_equal,
        .tilde_equal => 1,
        .range, .range_inclusive => 2,
        .pipe_pipe => 3,
        .ampersand_ampersand => 4,
        .pipe => 5,
        .caret => 6,
        .ampersand => 7,
        .equal_equal, .bang_equal => 8,
        .l_angled, .r_angled, .l_angled_equal, .r_angled_equal => 9,
        .l_angled_angled, .r_angled_angled => 10,
        .plus, .minus => 11,
        .asterisk, .slash, .percent => 12,
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
        .tilde => 13,
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
        .l_bracket => {
            _ = p.next();
            // todo : check for size or other modifiers
            _ = p.consume(.r_bracket) orelse return error.UnexpectedToken;
            const elem = try p.parseTypeExpr();
            return Ast.Expr.create(p.alloc, .{ .array_of = .{
                .elem = elem,
            }});
        },
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
        .@"struct" => p.parseStruct(),
        .@"enum" => p.parseEnum(),
        .@"union" => p.parseUnion(),
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

fn parseIf(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"if") orelse return error.UnexpectedToken;

    _ = p.consume(.l_paren) orelse return error.UnexpectedToken;
    const clause = try p.parseExpr();
    _ = p.consume(.r_paren) orelse return error.UnexpectedToken;

    const then_body = try p.parseExpr();

    const else_body = if (p.peek().kind == .@"else") blk: {
        _ = p.next();
        break :blk try p.parseExpr();
    } else null;

    return Ast.Expr.create(p.alloc, .{ .@"if" = .{
        .clause = clause,
        .then_body = then_body,
        .else_body = else_body,
    }});
}

fn parseLoop(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.loop) orelse return error.UnexpectedToken;
    const body = try p.parseExpr();
    return Ast.Expr.create(p.alloc, .{ .loop = .{
        .body = body,
    }});
}

fn parseWhile(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"while") orelse return error.UnexpectedToken;

    _ = p.consume(.l_paren) orelse return error.UnexpectedToken;
    const clause = try p.parseExpr();
    _ = p.consume(.r_paren) orelse return error.UnexpectedToken;

    const body = try p.parseExpr();

    return Ast.Expr.create(p.alloc, .{ .@"while" = .{
        .clause = clause,
        .body = body,
    }});
}

fn parseFor(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"for") orelse return error.UnexpectedToken;
    const capture = try p.parseExpr();
    _ = p.consume(.in) orelse return error.UnexpectedToken;
    _ = p.consume(.l_paren) orelse return error.UnexpectedToken;
    const iterable = try p.parseExpr();
    _ = p.consume(.r_paren) orelse return error.UnexpectedToken;

    const body = try p.parseExpr();

    return Ast.Expr.create(p.alloc, .{ .@"for" = .{
        .capture = capture,
        .iterable = iterable,
        .body = body,
    }});
}

// todo for break and cont. we can have a label without needing a value,
//  so this should be re-thought
fn parseBreak(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"break") orelse return error.UnexpectedToken;
    const label = if (p.peek().kind == .identifier) p.next() else null;
    const value = if (label) |_| try p.parseExpr() else null;
    return Ast.Expr.create(p.alloc, .{ .@"break" = .{
        .label = label,
        .value = value,
    }});
}

fn parseContinue(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"continue") orelse return error.UnexpectedToken;
    const label = if (p.peek().kind == .identifier) p.next() else null;
    const value = if (label) |_| try p.parseExpr() else null;
    return Ast.Expr.create(p.alloc, .{ .@"break" = .{
        .label = label,
        .value = value,
    }});
}

fn parseStruct(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"struct") orelse return error.ExpectedTypeExpression;
    _ = p.consume(.l_brace) orelse return error.UnexpectedToken;

    const members = p.parseContainerMembers();

    _ = p.consume(.r_brace) orelse return error.UnexpectedToken;
    return Ast.Expr.create(p.alloc, .{ .literal_struct = .{
        .members = members,
    }});
}

fn parseEnum(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"enum") orelse return error.ExpectedTypeExpression;
    _ = p.consume(.l_brace) orelse return error.UnexpectedToken;

    // todo : allow parseContainerMembers to support
    //  enum & union content by parsing variants
    const members = p.parseContainerMembers();

    _ = p.consume(.r_brace) orelse return error.UnexpectedToken;
    return Ast.Expr.create(p.alloc, .{ .literal_enum = .{
        .members = members,
    }});
}

fn parseUnion(p: *Parser) Error!*Ast.Expr {
    _ = p.consume(.@"union") orelse return error.ExpectedTypeExpression;
    _ = p.consume(.l_brace) orelse return error.UnexpectedToken;

    const members = p.parseContainerMembers();

    _ = p.consume(.r_brace) orelse return error.UnexpectedToken;
    return Ast.Expr.create(p.alloc, .{ .literal_union = .{
        .members = members,
    }});
}