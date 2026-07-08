const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

var depth: u16 = 0;
var in_pub: bool = false;

const TerminalColor = struct {
    // fn clear() void { std.debug.print("\x1b[37m", .{}); }
    // fn red() void { std.debug.print("\x1b[31m", .{}); }
    // fn green() void { std.debug.print("\x1b[32m", .{}); }
    // fn yellow() void { std.debug.print("\x1b[33m", .{}); }
    // fn blue() void { std.debug.print("\x1b[34m", .{}); }
    // fn magenta() void { std.debug.print("\x1b[35m", .{}); }
    fn clear() void { std.debug.print("", .{}); }
    fn red() void { std.debug.print("", .{}); }
    fn green() void { std.debug.print("", .{}); }
    fn yellow() void { std.debug.print("", .{}); }
    fn blue() void { std.debug.print("", .{}); }
    fn magenta() void { std.debug.print("", .{}); }
};


// todo : different colors for different types of statements / expressions
/// render the Ast back as if it is source code
pub fn render(ast: *Ast, src: [:0]const u8) void {
    for (ast.root) |node| renderStmt(node, src);
    TerminalColor.clear();
}

fn indent() void { for (0..depth) |_| std.debug.print("    ", .{}); }

fn renderStmt(stmt: *Ast.Stmt, src: [:0]const u8) void {
    TerminalColor.red();
    if (!in_pub) indent();
    switch(stmt.*) {
        .expr => |e| {
            TerminalColor.clear();
            renderExpr(e, src);
            std.debug.print(";\n", .{});
        },
        .field => |f| {
            TerminalColor.clear();
            std.debug.print("{s}", .{src[f.name.start..f.name.end]});
            if (f.type_expr) |ty| {
                std.debug.print(": ", .{});
                renderExpr(ty, src);
            }
            if (f.default) |d| {
                std.debug.print(" = ", .{});
                renderExpr(d, src);
            }

            std.debug.print(",\n", .{});
        },
        .fn_decl => |f| {
            std.debug.print("fn ", .{});
            TerminalColor.clear();
            std.debug.print("{s}(", .{src[f.name.start..f.name.end]});
            for(f.params, 0..) |s, i| {
                renderStmt(s, src);
                if (i != f.params.len - 1) std.debug.print(", ", .{});
            }
            std.debug.print(") ", .{});
            renderExpr(f.ret_ty, src);
            std.debug.print(" = ", .{});
            renderExpr(f.body, src);
            std.debug.print("\n", .{});
        },
        .let => |l| {
            std.debug.print("let ", .{});
            TerminalColor.clear();
            std.debug.print("{s}", .{src[l.name.start..l.name.end]});
            if (l.type_expr) |te| {
                std.debug.print(": ", .{});
                renderExpr(te, src);
            }
            std.debug.print(" = ", .{});
            renderExpr(l.value_expr, src);
            std.debug.print(";\n", .{});
        },
        .let_mut => |lm| {
            std.debug.print("let mut ", .{});
            TerminalColor.clear();
            std.debug.print("{s}", .{src[lm.name.start..lm.name.end]});
            if (lm.type_expr) |te| {
                std.debug.print(": ", .{});
                renderExpr(te, src);
            }
            std.debug.print(" = ", .{});
            renderExpr(lm.value_expr, src);
            std.debug.print(";\n", .{});
        },
        .param => |p| {
            TerminalColor.clear();
            std.debug.print("{s}", .{src[p.name.start..p.name.end]});
            std.debug.print(": ", .{});
            renderExpr(p.type_expr, src);
        },
        .pub_item => |p| {
            in_pub = true;
            std.debug.print("pub ", .{});
            renderStmt(p, src);
            in_pub = false;
        },
    }
}

fn renderExpr(expr: *Ast.Expr, src: [:0]const u8) void {
    in_pub = false;
    switch(expr.*) {
        .@"break" => |b| {
            TerminalColor.red();
            std.debug.print("break ", .{});
            TerminalColor.clear();
            if (b.label) |l| {
                std.debug.print("{s} ", .{src[l.start..l.end]});
            }
            if (b.value) |v| {
                renderExpr(v, src);
            }
        },
        .@"continue" => |c| {
            TerminalColor.red();
            std.debug.print("continue ", .{});
            TerminalColor.clear();
            if (c.label) |l| {
                std.debug.print("{s} ", .{src[l.start..l.end]});
            }
            if (c.value) |v| {
                renderExpr(v, src);
            }
        },
        .@"for" => |f| {
            TerminalColor.red();
            std.debug.print("for ", .{});
            TerminalColor.clear();
            renderExpr(f.capture, src);
            TerminalColor.red();
            std.debug.print(" in ", .{});
            TerminalColor.clear();
            std.debug.print("(", .{});
            renderExpr(f.iterable, src);
            std.debug.print(") ", .{});
            renderExpr(f.body, src);
        },
        .@"if" => |i| {
            TerminalColor.red();
            std.debug.print("if ", .{});
            TerminalColor.clear();
            std.debug.print("(", .{});
            renderExpr(i.clause, src);
            std.debug.print(") ", .{});
            renderExpr(i.then_body, src);
            if (i.else_body) |e| {
                TerminalColor.red();
                std.debug.print(" else ", .{});
                TerminalColor.clear();
                renderExpr(e, src);
            }
        },
        .@"switch" => {},
        .@"while" => |w| {
            TerminalColor.red();
            std.debug.print("while ", .{});
            TerminalColor.clear();
            std.debug.print("(", .{});
            renderExpr(w.clause, src);
            std.debug.print(") ", .{});
            renderExpr(w.body, src);
        },
        .access => |a| {
            renderExpr(a.container, src);
            std.debug.print(".{s}", .{src[a.member.start..a.member.end]});
        },
        .array_of => |a| {
            std.debug.print("[] ", .{});
            renderExpr(a.elem, src);
        },
        .binary => |b| {
            renderExpr(b.left, src);
            std.debug.print(" {s} ", .{ Token.lexeme(b.op.kind) orelse "ERR" });
            renderExpr(b.right, src);
        },
        .block => |b| {
            std.debug.print("{{\n", .{});
            depth += 1;
            for (b.content) |s| renderStmt(s, src);
            depth -= 1;
            indent();
            std.debug.print("}}", .{});
        },
        .builtin_call => |b| {
            std.debug.print("{s}(", .{ src[b.name.start..b.name.end] });
            for (b.args, 0..) |e, i| {
                renderExpr(e, src);
                if (i != b.args.len - 1) std.debug.print(", ", .{});
            }
            std.debug.print(")", .{});
        },
        .call => |c| {
            std.debug.print("{s}(", .{src[c.name.start..c.name.end]});
            for (c.args, 0..) |e, i| {
                renderExpr(e, src);
                if (i != c.args.len - 1) std.debug.print(", ", .{});
            }
            std.debug.print(")", .{});
        },
        .ident => |i| {
            std.debug.print("{s}", .{src[i.start..i.end]});
        },
        .literal_bool => |b| {
            TerminalColor.red();
            if (b.kind == .true) {
                std.debug.print("true", .{});
            } else std.debug.print("false", .{});
        },
        .literal_char => |c| {
            TerminalColor.green();
            std.debug.print("'{s}'", .{src[c.start..c.end]});
        },
        .literal_enum => |e| {
            TerminalColor.red();
            std.debug.print("enum  ", .{});
            TerminalColor.clear();
            std.debug.print("{{\n", .{});
            depth += 1;
            for (e.members) |m| renderStmt(m, src);
            depth -= 1;
            std.debug.print("}}", .{});
        },
        .literal_float => |f| {
            TerminalColor.green();
            std.debug.print("{s}", .{src[f.start..f.end]});
        },
        .literal_int => |i| {
            TerminalColor.green();
            std.debug.print("{s}", .{src[i.start..i.end]});
        },
        .literal_string => |s| {
            TerminalColor.green();
            std.debug.print("\"{s}\"", .{src[s.start..s.end]});
        },
        .literal_struct => |s| {
            TerminalColor.red();
            std.debug.print("struct ", .{});
            TerminalColor.clear();
            std.debug.print("{{\n", .{});
            depth += 1;
            for (s.members) |m| renderStmt(m, src);
            depth -= 1;
            std.debug.print("}}", .{});
        },
        .literal_trait => {},
        .literal_union => |u| {
            TerminalColor.red();
            std.debug.print("union ", .{});
            TerminalColor.clear();
            std.debug.print("{{\n", .{});
            depth += 1;
            for (u.members) |m| renderStmt(m, src);
            depth -= 1;
            std.debug.print("}}", .{});
        },
        .loop => |l| {
            TerminalColor.red();
            std.debug.print("loop ", .{});
            TerminalColor.clear();
            renderExpr(l.body, src);
        },
        .unary => |u| {
            std.debug.print("{s}", .{ Token.lexeme(u.op.kind) orelse "ERR" });
            renderExpr(u.operand, src);
        },
    }
    TerminalColor.clear();
}

// todo : report error location and show the offending line with some helpful info
/// render all errors in the Ast
pub fn renderErrors(ast: *Ast, src: [:0]const u8) void {
    for (ast.errors) |err| {
        switch (err.err) {
            error.ExpectedEof => std.debug.print("Expected EOF got {s} '{s}'\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
            error.ExpectedFn => std.debug.print("Expected 'fn' got {s} '{s}'\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
            error.ExpectedIdentifier => std.debug.print("Expected indetifier got {s} '{s}'\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
            error.ExpectedPubItem => std.debug.print("Expected pub item got {s} '{s}'\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
            error.InvalidContainerType => std.debug.print("{s} '{s}' is not a valid container type\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
            error.UnexpectedToken => std.debug.print("Unexpected Token {s} '{s}'\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
            error.ExpectedSemicolon => std.debug.print("Expected semicolon, got {s} '{s}'\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
            error.ExpectedTypeExpression => std.debug.print("Expected type expression, got {s} '{s}'\n", .{
                @tagName(err.token.kind) ,src[err.token.start..err.token.end]
            }),
        }
    }
}
