const std = @import("std");

pub const ProcKind = enum {
    build,
    run,
    @"test",
};

pub const Process = struct {
    kind: ProcKind,
    /// the location where this command was called from
    loc: []const u8,
    /// the path to main, relative to `loc`
    src: []const u8,

    pub fn format(self: @This(), writer: *std.Io.Writer) !void {
        try writer.print("{s} $ {s} {s}", .{
            self.loc, @tagName(self.kind), self.src
        });
    }
};

pub fn parseArgs(args: *std.process.ArgIterator) !Process {
    const loc = args.next() orelse return error.ExpectedLoc;
    const kind = args.next() orelse return error.ExpectedKind;
    const src = args.next() orelse return error.ExpectedSrc;
    return .{
        .kind = std.meta.stringToEnum(ProcKind, kind)
            orelse return error.InvalidKind,
        .loc = loc,
        .src = src,
    };
}