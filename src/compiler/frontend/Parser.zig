const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

pub const Parser = @This();
const Self = @This();

alloc: std.mem.Allocator,
/// non owned slice, containing the raw source code
src: []const u8,
tokens: []const Token,
pos: usize = 0,
errors: std.ArrayList(Ast.Error),

pub const ParseError = error {
    OutOfMemory,
    ExpectedEof,
    ExpectedFn,
    ExpectedPubItem,
    InvalidContainerType,
};

// /// parse a file from the root
// pub fn parseRoot(self: *Self) ParseError!void {
//     self.nodes.appendAssumeCapacity(.{
//         .kind = .root,
//         .main_token = 0,
//         .data = undefined,
//     });
//     const root_members = try self.parseContainerMembers();
//     const root_decls = try root_members.toRange(self);
//
//     if (self.tokenKind(self.pos) != .eof) return ParseError.ExpectedEof;
//
//     self.nodes.items(.data)[0] = .{ .extra_range = root_decls };
// }
//
// fn parseContainerMembers(self: *Self) ParseError!Members {
//     const scratch_top = self.scratch.items.len;
//     defer self.scratch.shrinkRetainingCapacity(scratch_top);
//
//     var field_state: union(enum) {
//         /// no fields have been seen
//         none,
//         /// currently parsing fields
//         seen,
//         /// saw fields, and then a declaration after
//         /// payload is the first token of the previous declaration
//         end: Ast.TokenIndex,
//         /// there was a declaration between fields
//         err,
//     } = .none;
//
//     // todo : make var when we actually use it
//     const last_field: Ast.TokenIndex = undefined;
//     _ = last_field;
//     // todo : skip all leading comments if they exist
//
//     var trailing: bool = false;
//     while (true) {
//         // consume any doc comments and save for later
//         sw: switch (self.tokenKind(self.pos)) {
//             // outermost container items
//             .@"pub", .@"inline", .@"const", .@"fn" => |t| {
//                 if (t == .@"inline") {
//                     switch (self.tokenKind(self.pos + 1)) {
//                         .@"for", .@"while" => |ct| continue :sw ct,
//                         else => {},
//                     }
//                 }
//
//                 self.pos += @intFromBool(t == .@"pub");
//                 const decl = try self.parseTopLevelDecl();
//                 if (decl) |d| {
//                     if (field_state == .seen) field_state = .{ .end = d };
//                     try self.scratch.append(self.alloc, d);
//                 }
//                 trailing = self.tokenKind(self.pos - 1) == .semicolon;
//             },
//             .eof, .r_brace => {
//                 // todo : if theres a doc comment,
//                 //  emit a warning that it has nothing to attach to
//                 //  (this will only be needed when we support comments fully)
//                 break;
//             },
//             else => {
//                 // todo : this, although it may not be needed.
//                 //  Zig uses it for parsing C-style containers, which we wont
//                 //  need, but ill need to look more into it
//                 //  before making a decision.
//             }
//         }
//     }
//
//     const items = self.scratch.items[scratch_top..];
//
//     if (items.len <= 2) {
//         return Members {
//             .len = items.len,
//             .data = .{ .opt_node_opt_node = .{
//                 if (items.len >= 1) Ast.OptionalNodeIndex.fromOptional(items[0])
//                 else Ast.OptionalNodeIndex.none,
//                 if (items.len >= 2) Ast.OptionalNodeIndex.fromOptional(items[1])
//                 else Ast.OptionalNodeIndex.none,
//             }},
//             .trailing = trailing,
//         };
//     }
//     return Members {
//         .len = items.len,
//         .data = .{ .extra_range = try self.listToRange(items) },
//         .trailing = trailing,
//     };
// }
//
// // todo : this function should also be recoverable, if it errors out the parser
// //  should search for the next container member to try and parse, that way we
// //  can yield multiple errors if needed
// fn parseTopLevelDecl(self: *Self) ParseError!?Ast.NodeIndex {
//     // keywords like `inline`
//     const modifier_token = self.next();
//     var expect_fn: bool = false;
//
//     switch(self.tokenKind(modifier_token)) {
//         .@"inline" => expect_fn = true,
//         else => self.pos -= 1,
//     }
//
//     const opt_fn_proto = try self.parseFnProto();
//     if (opt_fn_proto) |fn_proto| {
//         switch(self.tokenKind(self.pos)) {
//             .l_brace => {
//                 const fn_decl = try self.reserveNode(.decl_fn);
//                 errdefer self.unreserveNode(fn_decl);
//
//                 const body = try self.parseExpr();
//                 return self.setNode(fn_decl, .{
//                     .kind = .decl_fn,
//                     .main_token = self.nodeMainToken(fn_proto),
//                     .data = .{ .node_node = .{
//                         fn_proto,
//                         body,
//                     }}
//                 });
//             },
//             else => {
//                 // todo : emit warning that we expected  {
//                 return null;
//             }
//         }
//     }
//
//     if (expect_fn) return error.ExpectedFn;
//
//     // todo : try to parse a top level constant decl,
//     //  by this point it is the only thing we can expect
//
//     return error.ExpectedPubItem;
// }
//
// /// parse a function signature
// fn parseFnProto(self: *Self) ParseError!?Ast.NodeIndex {
//     const fn_token = self.consume(.@"fn") orelse return null;
//
//     const proto_index = try self.reserveNode(.fn_proto);
//     errdefer self.unreserveNode(proto_index);
//
//     _ = self.consume(.identifier);
//
//     // todo parse params
//
//     _ = self.consume(.bang); // consume the bang for fallible functions
//
//     // todo : parse return type expr
//
//     // todo : parse the fn proto into its appropriate proto kind.
//     return error.OutOfMemory;
// }
//
// /// parse a top level const decl statement
// fn parseGlobalDecl(self: *Self) ParseError!?Ast.NodeIndex {
//      _ = self;
//     // todo : parse the const
//     return error.OutOfMemory;
// }
//
// fn parseExpr(self: *Self) ParseError!Ast.NodeIndex {
//     _ = self;
//     // todo check for expr and dispatch, if we see a block we treat it as such,
//     //  otherwise parse with precedence climbing.
//     return error.OutOfMemory;
// }
//
// /// parse a container expr,
// ///
// /// ex.
// /// ```
// /// struct { ... }
// /// enum { ... }
// /// union { ... }
// /// trait { ... }
// /// ```
// fn parseContainer(self: *Self) ParseError!void {
//     const main_token = self.pos;
//     switch (self.tokenKind(main_token)) {
//         .@"struct", .@"union", .@"enum", .trait => {}, // todo : add union keyword
//         else => return error.InvalidContainerType,
//     }
//
//     _ = try self.consume(.l_brace);
//     _ = try self.parseContainerMembers();
//     _ = try self.consume(.r_brace);
//     // todo : maybe we expect a semicolon, but if we have syntax like
//     // const T = struct { ... };
//     // then it makes more sense for the cont decl parser to expect the semicolon.
// }

/// return the next token to parse
pub fn next(self: *Self) Token {
    const res = self.tokens[self.pos];
    self.pos += 1;
    return res;
}

pub fn consume(self: *Self, kind: Token.Kind) ?Token {
    return if (self.tokens[self.pos].kind == kind) self.next() else null;
}

/// parse a file from the root, and return all top level nodes
pub fn parseRoot(self: *Self) ![]const Ast.Node {
    _ = self;
    // todo : parse container members
    return error.OutOfMemory;
}