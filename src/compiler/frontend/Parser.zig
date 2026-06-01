const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

const Parser = @This();

alloc: std.mem.Allocator,
/// non owned slice, containing the raw source code
src: []const u8,
tokens: []const Token,
pos: usize = 0,
// todo : maybe make ParseError a type that can hold onto more context,
//  like the offending token, and a tag representing error type, alongside
//  anything else that may help the compiler to generate a helpful message
errors: std.ArrayList(ParseError),

pub const ParseError = union(enum) {
    UnexpectedToken: Token,
    ExpectedEof: Token,
    ExpectedFn: Token,
    ExpectedPubItem: Token,
    InvalidContainerType: Token,
};

const Error = error {
    UnexpectedToken,
    ExpectedEof,
    ExpectedFn,
    ExpectedPubItem,
    InvalidContainerType,
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
    return if (p.tokens[p.pos].kind == kind) p.next() else null;
}

fn reportError(p: *Parser, err: Error) void {
    const pe = switch (err) {
        error.UnexpectedToken => ParseError { .UnexpectedToken = p.peek() },
        error.ExpectedEof => ParseError { .ExpectedEof = p.peek() },
        error.ExpectedFn => ParseError { .ExpectedFn = p.peek() },
        error.ExpectedPubItem => ParseError { .ExpectedPubItem = p.peek() },
        error.InvalidContainerType => ParseError { .InvalidContainerType = p.peek() },
    };
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

    const item = try switch (p.tokens[p.pos].kind) {
        .identifier => p.parseField(),
        .@"fn" => p.parseFn(),
        .let => p.parseLet(),
        else => if (is_pub) error.ExpectedPubItem else error.UnexpectedToken,
    };

    return if (is_pub) Ast.Stmt.create(p.alloc, .{.pub_item = item}) else item;
}

fn parseField(p: *Parser) !*Ast.Stmt {
    _ = p;
    // identifier
    // expect colon
    // Expr
    // expect comma (unless next token is eof or '}')
    return error.ExpectedEof;
}

fn parseLet(p: *Parser) !*Ast.Stmt {
    _ = p;
    // expect let
    // optional mut
    // optional colon
    // if colon is given: Expr
    // expect equal
    // Expr
    return error.ExpectedEof;
}

fn parseFn(p: *Parser) !*Ast.Stmt {
    _ = p;
    // expect fn
    // expect ident
    // expect lparen
    // param list
    // expect rparen
    // return type expr (if next token is '{' default to void)
    // expect '='
    // Expr
    return error.ExpectedEof;
}