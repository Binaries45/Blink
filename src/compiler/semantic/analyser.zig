const std = @import("std");
const Scope = @import("scope.zig").Scope;
const AstNode = @import("../parsing/ast.zig").AstNode;
const SemanticError = @import("error.zig").SemanticError;
const Symbol = @import("symbol.zig").Symbol;
const Type = @import("../types/type.zig").Type;
const TypeTable = @import("../types/table.zig").TypeTable;
const readType = @import("read_type.zig").readType;

pub const SemanticAnalyser = @This();

alloc: std.mem.Allocator,

/// the global scope
global: *Scope,

/// type table for the program
types: TypeTable,

/// a stack of scopes, will contain all currently available scopes
/// from the current scope during the second pass of analysis.
stack: std.ArrayList(*Scope),

pub fn init(alloc: std.mem.Allocator) !SemanticAnalyser {
    const global = try alloc.create(Scope);
    global.* = Scope{
        .parent = null,
        .symbols = std.StringHashMap(Symbol).init(alloc),
    };

    return SemanticAnalyser{
        .alloc = alloc,
        .global = global,
        .types = try TypeTable.init(alloc),
        .stack = try std.ArrayList(*Scope).initCapacity(alloc, 1),
    };
}

/// two pass semantic analysis over all of the top level nodes,
/// expects the given node to be a file root
pub fn analyse(self: *SemanticAnalyser, root: *AstNode) !void {
    if (root.* != .root) { return SemanticError.ExpectedRoot; }

    // first pass
    for (root.*.root.nodes) |node| {
        try self.analyzeDecl(node);
    }

    // second pass
    for (root.*.root.nodes) |node| {
        try self.analyzeNode(node);
    }
}

/// analyze top level nodes to build context on types, functions, etc
fn analyzeDecl(self: *SemanticAnalyser, node: *AstNode) !void {
    const ty = try readType(self.alloc, node.*);
    switch (node.*) {
        .@"const" => |n| {
            const symbol = Symbol {
                .kind = .constant,
                .type = try Type.create(self.alloc, ty),
                .node = node,
            };
            try self.global.insert(n.name.raw, symbol);
        },
        .@"fn" => |n| {
            const symbol = Symbol {
                .kind = .function,
                .type = try Type.create(self.alloc, ty),
                .node = node,
            };
            try self.global.insert(n.name.raw, symbol);
        },
        else => return,
    }
}

/// attempts to resolve some type name into a primitive type, returns `null` on failure
fn primitiveFromName(self: *SemanticAnalyser, name: []const u8) ?*Type {
    if (std.mem.eql(u8, name, "void")) return self.types.void_ptr;
    if (std.mem.eql(u8, name, "bool")) return self.types.bool_ptr;
    if (std.mem.eql(u8, name, "f32")) return self.types.f32_ptr;
    if (std.mem.eql(u8, name, "f64")) return self.types.f64_ptr;
    if (std.mem.eql(u8, name, "f128")) return self.types.f128_ptr;

    const key = Type.PrimitiveType.IntType.fromName(name) orelse return null;
    return self.types.get(key);
}

/// given some pointer to a `Type` attempt to resolve it, updating the underlying value
fn resolveType(self: *SemanticAnalyser, ty: *Type) !*Type {
    switch (ty.*) {
        .primitive => return self.types.get(ty.*),
        .function => |t| {
            const ret = try self.resolveType(t.@"return");
            const params = try self.alloc.alloc(*Type, t.params.len);
            for (t.params, 0..) |param, i| {
                params[i] = try self.resolveType(param);
            }

            return self.types.get(.{
                .function = Type.FunctionType {
                    .@"return" = ret,
                    .params = params
                }
            });
        },
        .unresolved => |un| switch(un) {
            .named => |n| {
                if (self.primitiveFromName(n.name)) |p| {
                    return p;
                }
                return error.UnknownType;
            },
            .Unknown => return error.UnknownType,
        },
        else => return error.UnkownType,
    }
}

/// attempts to coerce the type of an expression into the target type, returns null on failure.
// fn coerceTypes(self: *SemanticAnalyser, expr: *AstNode, target: *Type) ?*Type {}

/// attempt to infer the type of some expression
fn InferType(self: *SemanticAnalyser, node: *AstNode) !*Type {
    switch (node.*) {
        .literal => |n| {
            switch (n.kind) {
                .numeric => {
                    if (std.mem.count(u8, n.val.raw, ".") > 0) {
                        return self.types.comptime_int_ptr;
                    }
                    // float literals are inferred as f128,
                    // we can always downcast them later on
                    return self.types.f128_ptr;
                },
                .bool => return self.types.bool_ptr,
                else => return error.NotImplemented,
            }
        },
        .ident => |n| {
            const sym = self.global.get(n.name.raw) orelse return error.SymbolNotDefined;
            return sym.type;
        },
        .unary => |n| {
            return try self.InferType(n.operand);
        },
        .binary => |n| {
            const lhs = try self.InferType(n.left);
            const rhs = try self.InferType(n.right);
            // todo : attempt to coerce types together
            if (lhs == rhs) return lhs;
            std.debug.print("{f} != {f}", .{lhs.*, rhs.*});
            return error.TypeMismatch;
        },
        else => return error.NotImplemented,
    }
}

/// recursively analyzes a node of the ast, performing full semantic analysis
fn analyzeNode(self: *SemanticAnalyser, node: *AstNode) !void {
    switch (node.*) {
        .@"const" => |n| {
            var sym = self.global.get(n.name.raw) orelse return error.UnknownSymbol;

            if (sym.type.*.isUnknown()) {
                const inferred = try self.InferType(n.value);
                sym.type = inferred;
            }

            sym.type = try self.resolveType(sym.type);
        },
        .@"fn" => |n| {
            var sym = self.global.get(n.name.raw) orelse return error.UnknownSymbol;
            sym.type = try self.resolveType(sym.type);

            // todo : analyse body
        },
        else => return error.CannotResolve,
    }
}