//! a flattened version of the AST

const std = @import("std");

const Ast = @import("../frontend/Ast.zig");
const Tag = Ast.Tag;

pub const NodeIndex = u32;
pub const TokenIndex = u32;

/// the flat collection of all nodes
nodes: []const Node,
/// a collection of indices for nodes which need more than two data fields
extra: []const u32,
/// a collection of indices for all top level declaration nodes
decls: []const NodeIndex,

pub const Node = struct {
    tag: Tag,
};
