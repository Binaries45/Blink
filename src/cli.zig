const std = @import("std");

pub const ProcKind = enum {
    /// compile to an executable
    build,
    /// compile to an executable and run
    run,
    /// compile to an executable and run only tests
    @"test",
};

pub const Process = struct {
    kind: ProcKind,
    /// the path to main, relative to `loc`
    src: []const u8,

    pub fn format(self: @This(), writer: *std.Io.Writer) !void {
        try writer.print("{s} {s}", .{
            @tagName(self.kind), self.src
        });
    }
};

pub fn parseArgs(args: *std.process.ArgIterator) !Process {
    _ = args.next();
    const kind = args.next() orelse return error.ExpectedKind;
    const src = args.next() orelse return error.ExpectedSrc;
    return .{
        .kind = std.meta.stringToEnum(ProcKind, kind)
            orelse return error.InvalidKind,
        .src = src,
    };
}