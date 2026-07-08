const std = @import("std");
const Io = std.Io;
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

pub fn main(init: std.process.Init) !void {
    const alloc = init.arena.allocator();
    var args = try init.minimal.args.iterateAllocator(alloc);

    const proc = try cli.parseArgs(&args);
    // std.debug.print("{f}\n", .{proc}
    try Compiler.compile(alloc, init.io, proc);
}
