const std = @import("std");
const cli = @import("cli.zig");
const Compiler = @import("compiler.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    const proc = try cli.parseArgs(&args);
    // std.debug.print("{f}\n", .{proc});

    try Compiler.compile(alloc, proc);
}