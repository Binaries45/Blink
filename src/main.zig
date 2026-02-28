const std = @import("std");
const compile = @import("compiler/compiler.zig").compile;

pub fn main() !void {
    var gpa = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    try compile(alloc);
}