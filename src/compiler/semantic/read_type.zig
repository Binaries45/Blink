const std = @import("std");
const AstNode = @import("../parsing/ast.zig").AstNode;
const Type = @import("../types/type.zig").Type;
const Token = @import("../tokenization/token.zig").Token;

/// reads the type of a node if it exists, helping to build context.
pub fn readType(alloc: std.mem.Allocator, node: AstNode) !Type {
    return switch (node) {
        .@"const" => |n| readConst(n),
        .@"fn" => |n| try readFn(alloc, n),
        else => .{ .primitive = .void },
    };
}

/// return a type given a name token
fn typeFromToken(tok: Token) Type {
    switch (tok.type) {
        .Ident => return .{ .unresolved = .{ .named = .{ .name = tok.raw } } },
        else => return .{ .unresolved = .Unknown },
    }
}

/// return the type given by some type expression
fn readTypeExpr(node: AstNode.TypeExpr) Type {
    if (node.nullable) {
        // todo : read expr from inner token
        return .{ .unresolved = .Unknown };
    }
    return typeFromToken(node.name);
}

/// return the type of a const statement
fn readConst(node: AstNode.ConstStmt) Type {
    if (node.type_expr) |t| {
        return readTypeExpr(t.type);
    }
    return .{ .unresolved = .Unknown };
}

fn readParam(node: AstNode.Param) Type {
    return readTypeExpr(node.type.type);
}

/// return the type of a function definition
fn readFn(alloc: std.mem.Allocator, node: AstNode.FnStmt) !Type {
    const params = node.params.param_list.params;
    var param_types = try alloc.alloc(*Type, params.len);
    for (params, 0..) |param, i| {
        const ty = try alloc.create(Type);
        ty.* = readParam(param.param);
        param_types[i] = ty;
    }
    
    const ret_ty = try alloc.create(Type);
    ret_ty.* = readTypeExpr(node.ret.type);

    return .{ .function = .{
        .@"return" = ret_ty,
        .params = param_types,
    }};
}