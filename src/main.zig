const std = @import("std");
const cli = @import("cli.zig");
const Compiler = @import("compiler.zig");

// idea : if be want to break with values without requiring a label,
// then the break should target the outermost scope
// (not including global, or the containing function body) this way something like:
// while (cond) {
//   if (thing) { break value };
// }
//
// will actually break out of the loop and not the if body,
// or maybe we can just not have this functionality lol

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    var gpa = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    const proc = try cli.parseArgs(&args);
    // std.debug.print("{f}\n", .{proc}
    try Compiler.compile(alloc, proc);
}