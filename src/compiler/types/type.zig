const std = @import("std");

pub const Type = union(enum) {
    primitive: PrimitiveType,
    @"struct": StructType,
    function: FunctionType,
    optional: Optional,
    unresolved: Unresolved,

    pub const PrimitiveType = union(enum) {
        void,
        bool,
        comptime_int,
        int: IntType,
        f32,
        f64,
        f128,

        pub const IntType = struct {
            signed: bool,
            bits: u16,

            pub fn fromName(name: []const u8) ?Type {
                if (name.len < 2) return null;

                const signed = switch (name[0]) {
                    'i' => true,
                    'u' => false,
                    else => return null,
                };

                const bits = std.fmt.parseInt(u16, name[1..], 10) catch return null;

                if (bits == 0) return null;

                return Type {
                    .primitive = .{
                        .int = IntType {
                            .signed = signed,
                            .bits = bits,
                        }
                    }
                };
            }
        };
    };

    pub const StructType = struct {
        // todo : structs are not supported yet
    };

    pub const FunctionType = struct {
        params: []*Type,
        @"return": *Type,
    };

    pub const Optional = struct {
        inner: *Type,
    };

    pub const Unresolved = union(enum) {
        named: Named,
        Unknown,

        pub const Named = struct {
            name: []const u8,
        };

        pub fn format(self: Unresolved, writer: *std.io.Writer) !void {
            switch (self) {
                .named => |u| try writer.print("Named({s})", .{u.name}),
                .Unknown => try writer.print("Unknown", .{}),
            }
        }
    };

    pub fn create(alloc: std.mem.Allocator, ty: Type) !*Type {
        const ptr = try alloc.create(Type);
        ptr.* = ty;
        return ptr;
    }

    pub fn format(self: Type, writer: *std.io.Writer) !void {
        switch (self) {
            .@"struct" => try writer.print("TODO : FORMAT STRUCT", .{}),
            .function => |t| {
                try writer.print("(", .{});
                for (t.params, 0..) |param, i| {
                    try writer.print("{f}", .{param});
                    if (i != t.params.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(") {f}", .{t.@"return"});
            },
            .primitive => |t| {
                if (t == .int) {
                    switch (t.int.signed) {
                        true => try writer.print("i{d}", .{t.int.bits}),
                        false => try writer.print("u{d}", .{t.int.bits}),
                    }
                    return;
                }
                try writer.print("{s}", .{ @tagName(t) });
            },
            .optional => |t| try writer.print("Optional({f})", .{t.inner}),
            .unresolved => |t| try writer.print("{f}", .{t}),
        }
    }

    pub fn isUnknown(ty: Type) bool {
        return switch (ty) {
            .unresolved => |u| switch(u) {
                .Unknown => true,
                else => false,
            },
            else => false,
        };
    }
};