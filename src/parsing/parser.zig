const std = @import("std");
const Token = @import("../tokenization/token.zig").Token;
const TokenType = @import("../tokenization/token.zig").TokenType;
const AstNode = @import("ast.zig").AstNode;
const ParseError = @import("error.zig").ParseError;

/// the parser
pub const Parser = @This();
/// the allocator used by the parser
alloc: std.mem.Allocator,
/// the list of tokens to be parsed
tokens: []Token,
/// the number of tokens in the token list
num_tokens: usize,
/// the current position in the token list
pos: usize = 0,

pub fn init(tokens: []Token, num_tokens: usize, alloc: std.mem.Allocator) Parser {
    return Parser {
        .alloc = alloc,
        .tokens = tokens,
        .num_tokens = num_tokens,
    };
}

fn allocNode(self: *Parser, node: AstNode) !*AstNode {
    const ptr = try self.alloc.create(AstNode);
    ptr.* = node;
    return ptr;
}

inline fn end(self: *Parser) bool {
    return self.pos >= self.num_tokens;
}

/// return the next token in the stream if it exists, and advance the parser's position.
inline fn advance(self: *Parser) Token {
    const tok = self.tokens[self.pos];
    self.pos += 1;
    return tok;
}

/// return the next token in the stream if it exists, without advancing the parser's position.
inline fn peek(self: *Parser) ?Token {
    if (self.end()) return null;
    return self.tokens[self.pos];
}

/// expect the given token type, if it is not next in the token stream and
/// error is returned, if it is, its consumed and returned
fn expect(self: *Parser, ttype: TokenType) !Token {
    const peeked = self.peek();
    if (peeked == null) return ParseError.UnexpectedEndOfTokens;
    if (peeked.?.type != ttype) {
        std.debug.print("{f}", .{peeked.?});
        return ParseError.UnexpectedToken;
    }
    return self.advance();
}

/// returns the precedence of a binary operator.
fn binaryPrecedence(ttype: TokenType) ?u8 {
    return switch (ttype) {
        .Assign, .CompAdd, .CompSub,
        .CompMul, .CompDiv, .CompMod,
        .CompBitNot, .CompBitOr, .CompBitXor,
        .CompBitAnd, .CompLshift, .CompRshift => 1,
        .Or => 2,
        .And => 3,
        .BitOr => 4,
        .BitXor => 5,
        .BitAnd => 6,
        .Eq, .Neq => 7,
        .Gt, .Ge, .Lt, .Le, => 8,
        .Lshift, .Rshift => 9,
        .Add, .Sub => 10,
        .Mul, .Div, .Mod => 11,
        .Dot => 99, // temp value, but this must be the max lol
        else => null,
    };
}

/// parse the root file of a project, writing the parsed nodes to the given ArrayList.
pub fn parseRoot(self: *Parser) !*AstNode {
    var nodes = try std.ArrayList(*AstNode).initCapacity(self.alloc, self.num_tokens / 3);
    defer nodes.deinit(self.alloc);

    // parse until no tokens remain
    while (!self.end()) {
        const node = try switch (self.tokens[self.pos].type) {
            .Const => self.parseConst(),
            .Ident => self.parseFn(),
            else => {
                std.debug.print("{any}", .{self.peek()});
                return ParseError.UnexpectedToken;
            },
        };
        try nodes.append(self.alloc, node);
    }

    return self.allocNode(.{
        .root = AstNode.Root {
            .nodes = try nodes.toOwnedSlice(self.alloc)
        }
    });
}

fn parseParamList(self: *Parser) !*AstNode {
    var params = try std.ArrayList(*AstNode).initCapacity(self.alloc, 1);
    defer params.deinit(self.alloc);

    _ = try self.expect(.Lparen);
    while (true) {
        // this code is horrendous, i cant wait to come back and clean it up
        if (self.peek() == null) return ParseError.UnexpectedEndOfTokens;
        if (self.peek().?.type == .Rparen) break;

        const name = try self.expect(.Ident);
        _ = try self.expect(.Colon);
        const ty = try self.parseTypeExpr();
        try params.append(self.alloc, try self.allocNode(.{
            .param = .{
                .name = name,
                .type = ty,
            }
        }));
        if (self.peek() == null) return ParseError.UnexpectedEndOfTokens;
        if (self.peek().?.type == .Comma) _ = self.advance();
    }
    _ = try self.expect(.Rparen);

    return self.allocNode(.{
        .param_list = .{
            .params = try params.toOwnedSlice(self.alloc),
        }
    });
}

/// parse a function definition from the token stream
fn parseFn(self: *Parser) !*AstNode {
    const name = try self.expect(.Ident);
    _ = try self.expect(.Path);
    _ = try self.expect(.Fn);
    const params = try self.parseParamList();
    // todo : if type expr is omitted, default to void
    const ret = try self.parseTypeExpr();
    const body = try self.parseBlock();
    return self.allocNode(.{
        .@"fn" = .{
            .name = name,
            .params = params,
            .ret = ret,
            .body = body,
        }
    });
}

/// parse a constant definition from the token stream
fn parseConst(self: *Parser) !*AstNode {
    _ = try self.expect(.Const);
    const name = try self.expect(.Ident);

    // optional type expression
    const ty = if (self.peek()) |t| expr: {
        if (t.type == .Colon) {
            _ = self.advance();
            break :expr try self.parseTypeExpr();
        }
        break :expr null;
    } else null;

    _ = try self.expect(.Assign);
    const expr = try self.parseExpr();
    _ = try self.expect(.Semicolon);

    return self.allocNode(.{
        .@"const" = .{
            .name = name,
            .type_expr = ty,
            .value = expr,
        }
    });
}

/// parse a let statement from the token stream
fn parseLet(self: *Parser) !*AstNode {
    _ = try self.expect(.Let);
    const name = try self.expect(.Ident);

    const ty = if (self.peek()) |t| expr: {
        if (t.type == .Colon) {
            _ = self.advance();
            break :expr try self.parseTypeExpr();
        }
        break :expr null;
    } else null;

    _ = try self.expect(.Assign);
    const expr = try self.parseExpr();
    _ = try self.expect(.Semicolon);

    return self.allocNode(.{
        .let = .{
            .name = name,
            .type_expr = ty,
            .value = expr,
        }
    });
}

/// parse an if statement from the token stream
fn parseIf(self: *Parser) ParseError!*AstNode {
    _ = try self.expect(.If);
    const clause = try self.parseExpr();
    const then = try self.parseBlock();
    const peeked = self.peek();
    if (peeked == null) return ParseError.UnexpectedEndOfTokens;
    var @"else": ?*AstNode = null;
    if (peeked.?.type == .Else) {
        _ = self.advance();
        @"else" = switch (self.tokens[self.pos].type) {
            .If => try self.parseIf(),
            .Lbrace => try self.parseBlock(),
            else => return ParseError.UnexpectedToken,
        };
    }

    return self.allocNode(.{
        .@"if" = .{
            .clause = clause,
            .then = then,
            .@"else" = @"else",
        }}
    );
}

/// parse a return statement from the token stream
fn parseRet(self: *Parser) !*AstNode {
    _ = try self.expect(.Ret);
    const expr = try self.parseExpr();
    _ = try self.expect(.Semicolon);

    return self.allocNode(.{
        .ret = .{
            .value = expr,
        }
    });
}

fn primary(self: *Parser) ParseError!*AstNode {
    const tok = self.advance();
    return switch (tok.type) {
        .Numeric => return self.allocNode(.{ .literal = .{
            .val = tok,
        }}),
        .Ident => {
            if (self.peek() == null) return ParseError.UnexpectedEndOfTokens;
            if (self.peek().?.type == .Lparen) {
                var args = try std.ArrayList(*AstNode).initCapacity(self.alloc, 1);
                defer args.deinit(self.alloc);

                _ = try self.expect(.Lparen);
                while (true) {
                    // this code is horrendous, i cant wait to come back and clean it up
                    if (self.peek() == null) return ParseError.UnexpectedEndOfTokens;
                    if (self.peek().?.type == .Rparen) break;

                    try args.append(self.alloc, try self.parseExpr());
                    if (self.peek() == null) return ParseError.UnexpectedEndOfTokens;
                    if (self.peek().?.type == .Comma) _ = self.advance();
                }
                _ = try self.expect(.Rparen);

                return self.allocNode(.{
                    .call = .{
                        .name = tok,
                        .args = try args.toOwnedSlice(self.alloc),
                    }
                });
            }

            return self.allocNode(.{ .ident = .{
                .name = tok,
            }});
        },
        .Lparen => {
            const expr = self.expression(0);
            _ = try self.expect(.Rparen);
            return expr;
        },
        else => {
            std.debug.print("{f}", .{tok});
            return ParseError.UnexpectedToken;
        },
    };
}

// /// helper for expression parsing
fn expression(self: *Parser, min_prec: u8) !*AstNode {
    var result = try self.primary();

    while (true) {
        const current = self.peek();
        if (current == null) return ParseError.UnexpectedEndOfTokens;

        const prec = binaryPrecedence(current.?.type);
        if (prec == null or prec.? < min_prec) break;

        _ = self.advance();

        const rhs = try self.expression(prec.?);

        result = try self.allocNode(.{
            .binary = .{
                .op = current.?,
                .left = result,
                .right = rhs,
            }
        });
    }
    return result;
}

/// parse an expression from the token stream
fn parseExpr(self: *Parser) !*AstNode {
    return self.expression(0);
}

fn parseTypeExpr(self: *Parser) !*AstNode {
    const name = try self.expect(.Ident); 
    
    return self.allocNode(.{
        .type = .{
            // todo : actually handle nullable types, as well as errors.
            .nullable = false,
            .name = name,
        }
    });
}

/// parse a block from the token stream
fn parseBlock(self: *Parser) !*AstNode {
    _ = try self.expect(.Lbrace);

    var nodes = try std.ArrayList(*AstNode).initCapacity(self.alloc, 1);
    defer nodes.deinit(self.alloc);

    while (self.peek() != null and self.peek().?.type != .Rbrace) {
        const node = try switch (self.tokens[self.pos].type) {
            .Const => self.parseConst(),
            .Let => self.parseLet(),
            .If => self.parseIf(),
            .Ret => self.parseRet(),
            else => ex: {
                const e = self.parseExpr();
                _ = try self.expect(.Semicolon);
                break :ex e;
            },
        };
        try nodes.append(self.alloc, node);
    }

    _ = try self.expect(.Rbrace);

    return self.allocNode(.{
        .block = .{
            .statements = try nodes.toOwnedSlice(self.alloc),
        }
    });
}