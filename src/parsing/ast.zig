pub const std = @import("std");
pub const Token = @import("../tokenization/token.zig").Token;
pub const TokenType = @import("../tokenization/token.zig").TokenType;

pub const AstNode = union(enum) {
    root: Root,

    // statements
    @"const": ConstStmt,
    let: LetStmt,
    @"if": IfStmt,
    ret: RetStmt,
    @"fn": FnStmt,
    param: Param,
    param_list: ParamList,

    // expressions
    type: TypeExpr,
    block: Block,
    literal: Literal,
    ident: Ident,
    call: FnCall,
    unary: UnOp,
    binary: BinOp,

    /// the root node of a file, contains pointers to all top level nodes.
    pub const Root = struct {
        nodes: []const *AstNode,
    };

    pub const ConstStmt = struct {
        name: Token,
        type_expr: ?*AstNode,
        value: *AstNode,
    };

    pub const LetStmt = struct {
        name: Token,
        type_expr: ?*AstNode,
        value: *AstNode,
    };

    pub const IfStmt = struct {
        clause: *AstNode,
        then: *AstNode,
        @"else": ?*AstNode,
    };

    pub const RetStmt = struct {
        value: *const AstNode,
    };

    pub const FnStmt = struct {
        name: Token,
        params: *AstNode,
        ret: *AstNode,
        body: *AstNode,
    };

    pub const Param = struct {
        name: Token,
        type: *AstNode,
    };

    pub const ParamList = struct {
        params: []const *AstNode,
    };

    pub const TypeExpr = struct {
        name: Token,
        nullable: bool,
    };

    pub const Block = struct {
        statements: []const *AstNode,
    };

    pub const Literal = struct {
        val: Token,
    };

    pub const Ident = struct {
        name: Token,
    };

    pub const FnCall = struct {
        name: Token,
        args: []const *AstNode,
    };

    pub const UnOp = struct {
        op: Token,
        operand: *AstNode,
    };

    pub const BinOp = struct {
        op: Token,
        left: *AstNode,
        right: *AstNode,
    };

    fn writeIndent(writer: *std.io.Writer, i: usize) !void {
        for (0..i) |_| {
            try writer.print("  ", .{});
        }
    }

    fn formatIndented(self: AstNode, writer: *std.io.Writer, indent: usize) !void {
        switch (self) {
            .root => |n| {
                for (n.nodes) |node| {
                    try node.formatIndented(writer, indent);
                    try writer.writeByte('\n');
                }
            },
            .@"const" => |n| {
                try writeIndent(writer, indent);
                if (n.type_expr) |t| {
                    try writer.print("const {f} {s} = {f}", .{t, n.name.raw, n.value});
                }  else {
                    try writer.print("const {s} = {f}", .{n.name.raw, n.value});
                }
            },
            .let => |n| {
                try writeIndent(writer, indent);
                if (n.type_expr) |t| {
                    try writer.print("let {f} {s} = {f}", .{t, n.name.raw, n.value});
                }  else {
                    try writer.print("let {s} = {f}", .{n.name.raw, n.value});
                }
            },
            .@"if" => |n| {
                try writeIndent(writer, indent);
                try writer.print("if ", .{});
                try n.clause.formatIndented(writer, indent);
                try writer.print(" then:\n", .{});

                try n.then.formatIndented(writer, indent);

                if (n.@"else") |else_block| {
                    try writer.writeByte('\n');
                    try writeIndent(writer, indent);
                    try writer.print("else:\n", .{});
                    try else_block.formatIndented(writer, indent);
                }
            },
            .ret => |n| {
                try writeIndent(writer, indent);
                try writer.print("ret {f}", .{n.value});
            },
            .@"fn" => |n| try writer.print("{s} :: fn({f}) -> {f} {{\n{f}\n}}", .{n.name.raw, n.params, n.ret, n.body}),
            .param => |n| try writer.print("{s}: {f}", .{n.name.raw, n.type}),
            .param_list => |n| {
                for (n.params, 0..) |param, i| {
                    if (i != n.params.len - 1) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{f}", .{param});
                }
            },
            .type => |n| {
                if (n.nullable) {
                    try writer.print("?", .{});
                }
                try writer.print("{s}", .{n.name.raw});
            },
            .block => |n| {
                for (n.statements, 0..) |stmt, i| {
                    if (i != 0) {
                        try writer.writeByte('\n');
                    }
                    try stmt.formatIndented(writer, indent + 1);
                }
            },
            .literal => |n| {
                try writeIndent(writer, indent);
                try writer.print("{s}", .{n.val.raw});
            },
            .ident => |n| {
                try writeIndent(writer, indent);
                try writer.print("{s}", .{n.name.raw});
            },
            .call => |n| {
                try writeIndent(writer, indent);
                try writer.print("{s}(", .{n.name.raw});
                for (n.args, 0..) |arg, i| {
                    try writer.print("{f}", .{arg});
                    if (i != n.args.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(")", .{});
            },
            .unary => |n| try writer.print("{s}({f})", .{@tagName(n.op.type), n.operand}),
            .binary => |n| try writer.print("{s}({f}, {f})", .{@tagName(n.op.type), n.left, n.right}),
        // else => try writer.print("TODO: impl format for {s}", .{@tagName(self)}),
        }
    }

    pub fn format(self: AstNode, writer: *std.io.Writer) !void {
        try self.formatIndented(writer, 0);
    }
};