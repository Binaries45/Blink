const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const TokenizationError = @import("error.zig").TokenizationError;

pub const Lexer = @This();
src: []const u8,
pos: usize = 0,

pub fn init(src: []const u8) Lexer {
    return Lexer{ .src = src };
}

/// returns true if the end of the file has been reached,
/// otherwise returns false
inline fn eof(self: *Lexer) bool {
    return self.pos >= self.src.len;
}

/// returns the next character in the input stream,
/// returns null if the end of the file has been reached
inline fn peek(self: *Lexer) ?u8 {
    if (self.eof()) return null;
    return self.src[self.pos];
}

/// return the next character in the input stream, and increment the
/// position of the lexer.
inline fn advance(self: *Lexer) u8 {
    const c = self.src[self.pos];
    self.pos += 1;
    return c;
}

/// returns if a character is a valid starting char for an identifier
inline fn isIdentStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

/// returns if a character is a valid char for an identifier
inline fn isIdentChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

/// ship any upcoming whitespace chars
inline fn skipWhitespace(self: *Lexer) void {
    while (!self.eof() and std.ascii.isWhitespace(self.src[self.pos])) {
        self.pos += 1;
    }
}

/// attempts to read some symbol from the input stream,
/// if a valid symbol cannot be read, an error is returned instead
fn symbol(self: *Lexer, start: usize) !Token {
    const max_len = @min(TokenType.longest_symbol, self.src.len - start);
    var len = max_len;
    while (len > 0) : (len -= 1) {
        const slice = self.src[start .. start + len];
        if (TokenType.symbol_map.get(slice)) |tok| {
            self.pos = start + len;
            return Token{
                .type = tok,
                .start = start,
                .raw = slice,
            };
        }
    }
    return TokenizationError.UnexpectedChar;
}

/// attempts to read some keyword or identifier from the input stream,
/// if a valid keyword cannot be read, then any valid chars
/// up until that point are treated as an identifier,
/// if there were no valid chars an error is returned instead.
fn keywordOrIdent(self: *Lexer, start: usize) !Token {
    // consume until no more valid chars
    while (!self.eof()) {
        const c = self.peek();
        if (c == null) return TokenizationError.UnexpectedEof;
        if (!Lexer.isIdentChar(c.?)) break;
        _ = self.advance();
    }

    const raw = self.src[start..self.pos];

    return Token{
        .raw = raw,
        .start = start,
        .type = TokenType.keyword_map.get(raw) orelse .Ident,
    };
}

/// attempts to read some number from the input stream,
/// if any invalid chars are found while constructing the number,
/// an error will be returned instead.
fn number(self: *Lexer, start: usize) !Token {
    while (!self.eof()) {
        const c = self.peek();
        if (c == null) return TokenizationError.UnexpectedEof;
        if (!std.ascii.isDigit(c.?)) break;
        _ = self.advance();
    }

    // check for fractional portion
    const next = self.peek();
    if (next != '.') {
        return Token{
            .raw = self.src[start..self.pos],
            .start = start,
            .type = .Numeric,
        };
    }
    // consume '.'
    _ = self.advance();

    // consume fractional portion
    while (!self.eof()) {
        const c = self.peek();
        if (c == null) return TokenizationError.UnexpectedEof;
        if (!std.ascii.isDigit(c.?)) break;
        _ = self.advance();
    }

    return Token{
        .raw = self.src[start..self.pos],
        .start = start,
        .type = .Numeric,
    };
}

/// Attempts to return the next token from the input stream,
/// will return any errors that are encountered
fn nextToken(self: *Lexer) !?Token {
    self.skipWhitespace();
    if (self.eof()) return null;

    const start = self.pos;
    const c = self.advance();

    switch (c) {
        'a'...'z', 'A'...'Z', '_' => return try self.keywordOrIdent(start),
        '0'...'9' => return try self.number(start),
        else => return try self.symbol(start),
    }
}

/// Tokenize the entire input stream,
/// writing all tokens to the provided buffer,
/// and returning the number of tokens written
pub fn tokenize(self: *Lexer, tokens: []Token) !usize {
    var i: usize = 0;
    while (!self.eof()) : (i += 1) {
        const next = try self.nextToken();
        if (next == null) break;
        tokens[i] = next.?;
    }
    return i;
}
