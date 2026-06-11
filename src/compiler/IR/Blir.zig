//! Blink Intermediate Representation

const Blir = @This();
const Index = u32;

/// a BLIR instruction
pub const Inst = union(enum(u8)) {
    add: Binary,
    sub: Binary,
    mul: Binary,
    div: Binary,
    mod: Binary,
    shl: Binary,
    shr: Binary,
    bit_and: Binary,
    bit_or: Binary,
    bit_xor: Binary,
    bool_and: Binary,
    bool_or: Binary,
    bool_xor: Binary,
    cmp_lt: Binary,
    cmp_le: Binary,
    cmp_gt: Binary,
    cmp_ge: Binary,
    cmp_eq: Binary,
    cmp_ne: Binary,
    /// left is size, right is the contained type
    array_type: Binary,

    neg: Unary,
    pos: Unary,
    not: Unary,
    bit_not: Unary,
    bool_not: Unary,
    /// ?T where the operand is T
    optional_type: Unary,

    param: Param,

    // todo : support sentinel arrays in the language

    block: Block,
    /// let / let mut declaration
    declaration: Declaration,
    fn_decl: FnDecl,
    call: Call,

    /// an integer literal that fist within a u64
    int: u64,
    /// an integer literal too large to fit in a u64
    int_big: []const u8,
    /// a float literal that fits in an f64
    float: f64,
    /// a float literal that fits in an f128
    float_big: f128,

    const Binary = struct {
        left: Index,
        right: Index,
    };

    const Unary = struct {
        operand: Index,
    };

    const Param = struct {
        name: []const u8,
        @"type": Type,

        const Type = packed struct(u32) {
            body: u31,
            generic: bool,
        };
    };

    const Block = struct {};

    const Declaration = struct {};

    const FnDecl = struct {};

    const Call = struct {};
};