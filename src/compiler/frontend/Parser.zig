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

pub const ParseError = error {
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
            // todo : this top arm might not be needed
            .eof, .@"fn", .@"pub", .let => return,
            .r_brace => { p.pos += 1; return; },
            else => p.pos += 1,
        }
    }
}

fn consume(p: *Parser, kind: Token.Kind) ?Token {
    return if (p.tokens[p.pos].kind == kind) p.next() else null;
}

/// parse a file from the root, and return all top level nodes
pub fn parseRoot(p: *Parser) ![]const *Ast.Stmt {
    return p.parseContainerMembers();
}

fn parseContainerMembers(p: *Parser) ![]const *Ast.Stmt {
    var members = try std.ArrayList(*Ast.Stmt).initCapacity(p.alloc, 0);

    // consume until end of file or r_brace is encountered. an r_brace
    // should signal the end of the container as any braces not corresponding
    // are expected to be consumed by parseContainerMember

    return members.toOwnedSlice(p.alloc);
}

fn parseContainerMember(p: *Parser) ![]const *Ast.Stmt {
    const is_pub = p.consume(.@"pub") != null;
    // state machine over next token:
    // ident -> Field
    // fn -> fnDecl
    // let -> LetStmt
    // ... other stuff I cant think of
    // else -> error : unexpected token, find start of next decl, return error
    switch (p.tokens[p.pos].kind) {
        .identifier => {},
        .@"fn" => {},
        .let => {},
        else => return if (is_pub) error.ExpectedPubItem else error.UnexpectedToken,
    }
}

fn parseField(p: *Parser) !*Ast.Stmt {
    _ = p;
    return error.ExpectedEof;
}

fn parseLet(p: *Parser) !*Ast.Stmt {
    _ = p;
    return error.ExpectedEof;
}

fn parseFn(p: *Parser) !*Ast.Stmt {
    _ = p;
    return error.ExpectedEof;
}