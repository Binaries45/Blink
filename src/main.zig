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
// TODO : Function decls as expressions
// const main = () -> { ... }
//
// in order to keep things concise, we will have type inference operate on
// functions to allow devs to omit complex types, or declare the types inline.
// example:
// const map = (collection: C, f: fn(T) T) C -> { ... }
//
// this map function declared more explicitly looks like
// const map: fn(C, fn(T) T) C = (collection, f) -> { ... }
//
// however, this syntax can get bloated fast, so we allow users to omit the type decl
// and just have param and return types be added onto the capture group with return types being void,
// and empty capture types being an error.
//
// both should be allowed too, if someone decided to do that for whatever reason:
// const map: fn(C, fn(T) T) C = (collection: C, f) C -> { ... }
//
// in this case type checking will ensure the capture types match the signature types
//
// this syntax will also extend naturally to support closures, lets say we call our map function on a list
//
// const new_list = list.map((a) -> { ... })
//
// although this does raise one issue on omitted return types, as to whether they should be inferred from the body,
// or default to void. Inferrence does keep closures clean without breaking the "one right way" rule,
// this is likely the best way to go as all type info will be inferred from the signature of map
//
// is he cooking? ^^^^^ 
