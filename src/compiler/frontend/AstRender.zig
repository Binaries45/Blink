const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

/// render the Ast back as if it is source code
pub fn render(ast: *Ast, src: [:0]const u8) void {
    for (ast.root) |node| renderStmt(node, src);
}

fn renderStmt(stmt: *Ast.Stmt, src: [:0]const u8) void {
    switch(stmt.*) {
        .expr => |e| {
            renderExpr(e, src);
            std.debug.print(";\n", .{});
        },
        .field => |f| {
            std.debug.print("{s}: ", .{src[f.name.start..f.name.end]});
            renderExpr(f.type_expr, src);
            std.debug.print(",\n", .{});
        },
        .fn_decl => {},
        .let => |l| {
            std.debug.print("let {s}", .{src[l.name.start..l.name.end]});
            if (l.type_expr) |te| {
                std.debug.print(": ", .{});
                renderExpr(te, src);
            }
            std.debug.print(" = ", .{});
            renderExpr(l.value_expr, src);
            std.debug.print(";\n", .{});
        },
        .let_mut => {},
        .param => {},
        .pub_item => {},
    }
}

fn renderExpr(expr: *Ast.Expr, src: [:0]const u8) void {
    switch(expr.*) {
        .@"for" => {},
        .@"if" => {},
        .@"loop" => {},
        .@"switch" => {},
        .@"while" => {},
        .access => {},
        .binary => |b| {
            renderExpr(b.left, src);
            std.debug.print(" {s} ", .{ Token.lexeme(b.op.kind) orelse "ERR" });
            renderExpr(b.right, src);
        },
        .block => {},
        .builtin_call => {},
        .call => {},
        .ident => |i| std.debug.print("{s}", .{src[i.start..i.end]}),
        .literal_bool => |b| if (b.kind == .true) {
            std.debug.print("true", .{});
        } else std.debug.print("false", .{}),
        .literal_char => {},
        .literal_enum => {},
        .literal_float => |f| std.debug.print("{s}", .{src[f.start..f.end]}),
        .literal_int => |i| std.debug.print("{s}", .{src[i.start..i.end]}),
        .literal_string => |s| std.debug.print("\"{s}\"", .{src[s.start..s.end]}),
        .literal_struct => {},
        .literal_trait => {},
        .literal_union => {},
        .unary => |u| {
            std.debug.print("{s}", .{ Token.lexeme(u.op.kind) orelse "ERR" });
            renderExpr(u.operand, src);
        },
    }
}

/// render all errors in the Ast
pub fn renderErrors(ast: *Ast, src: [:0]const u8) void {
    for (ast.errors) |err| {
        switch (err) {
            .ExpectedEof => |e| std.debug.print("Expected EOF got {s} '{s}'\n", .{
                @tagName(e.kind) ,src[e.start..e.end]
            }),
            .ExpectedFn => |e| std.debug.print("Expected 'fn' got {s} '{s}'\n", .{
                @tagName(e.kind) ,src[e.start..e.end]
            }),
            .ExpectedIdentifier => |e| std.debug.print("Expected indetifier got {s} '{s}'\n", .{
                @tagName(e.kind) ,src[e.start..e.end]
            }),
            .ExpectedPubItem => |e| std.debug.print("Expected pub item got {s} '{s}'\n", .{
                @tagName(e.kind) ,src[e.start..e.end]
            }),
            .InvalidContainerType => |e| std.debug.print("{s} '{s}' is not a valid container type\n", .{
                @tagName(e.kind) ,src[e.start..e.end]
            }),
            .UnexpectedToken => |e| std.debug.print("Unexpected Token {s} '{s}'\n", .{
                @tagName(e.kind) ,src[e.start..e.end]
            }),
            .ExpectedSemicolon => |e| std.debug.print("Expected semicolon, got {s} '{s}'\n", .{
                @tagName(e.kind) ,src[e.start..e.end]
            }),
        }
    }
}