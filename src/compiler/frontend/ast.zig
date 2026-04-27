const std = @import("std");
const Token = @import("Token.zig");

pub const Ast = @This();

kind: Kind,
data: Data,

// todo : a flat AST structure similar to how zig does it
// we're going to be making a ton of passes over the ast,
// so optimizing for the cache early should save later headaches


pub const Kind = enum {

};

pub const Data = struct {

};