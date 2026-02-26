const std = @import("std");
const Symbol = @import("symbol.zig").Symbol;
const Type = @import("symbol.zig").Type;
const SematicError = @import("error.zig").SemanticError;

pub const Scope = struct {
    parent: ?*Scope,
    symbols: std.StringHashMap(Symbol),

    /// insert a new symbol with the given name, will error out on duplicate definitions
    pub fn insert(self: *Scope, name: []const u8, symbol: Symbol) !void {
        const dup = try self.symbols.fetchPut(name, symbol);
        if (dup) |_| {
            return SematicError.DuplicateDefinition;
        }
    }

    /// check for some definition of the given name, if it exists return a pointer to it.
    pub fn get(self: *Scope, name: []const u8) ?*Symbol {
        return self.symbols.getPtr(name);
    }

    pub fn format(self: Scope, writer: *std.io.Writer) !void {
        var iter = self.symbols.iterator();
        while (iter.next()) |entry| {
            try writer.print("{s} = {f}\n", .{entry.key_ptr.*, entry.value_ptr.*});
        }
    }
};
