const std = @import("std");

pub const Token = @This();

kind: Kind,
start: usize,
end: usize,

pub const Kind = enum {
    unknown,
    eof,

    at,
    pound,
    dollar,
    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_bracket,
    r_bracket,
    backslash,
    semicolon,
    colon,
    single_quote,
    double_quote,
    comma,
    period,
    question,

    // operators
    equal,
    equal_equal,
    plus,
    plus_equal,
    minus,
    minus_equal,
    asterisk,
    asterisk_equal,
    slash,
    slash_equal,
    percent,
    percent_equal,
    ampersand,
    ampersand_ampersand,
    ampersand_equal,
    pipe,
    pipe_pipe,
    pipe_equal,
    carat,
    carat_equal,
    tilde,
    tilde_equal,
    bang,
    bang_equal,
    l_angled,
    l_angled_angled,
    l_angled_equal,
    l_angled_angled_equal,
    r_angled,
    r_angled_angled,
    r_angled_equal,
    r_angled_angled_equal,

    arrow,
    range,
    range_inclusive,

    // container tokens (their raw values matter)
    identifier,
    string,
    char,
    numeric,

    // keywords
    true,
    false,
    @"fn",
    ret,
    @"const",
    @"struct",
    @"enum",
    let,
    mut,
    @"if",
    @"switch",
    @"else",
    @"while",
    @"for",
    loop,
    impl,
    @"defer",
};

pub const longest_symbol = blk: {
    var max: usize = 0;
    for (symbol_map.keys()) |k| {
        max = @max(max, k.len);
    }
    break :blk max;
};

pub const symbol_map = std.StaticStringMap(Kind).initComptime(.{
    .{"@", .at},
    .{"#", .pound},
    .{"$", .dollar},
    .{"(", .l_paren},
    .{")", .r_paren},
    .{"{", .l_brace},
    .{"}", .r_brace},
    .{"[", .l_bracket},
    .{"]", .r_bracket},
    .{"\\", .backslash},
    .{";", .semicolon},
    .{":", .colon},
    .{"'", .single_quote},
    .{"\"", .double_quote},
    .{",", .comma},
    .{".", .period},
    .{"?", .question},
    .{"=", .equal},
    .{"==", .equal_equal},
    .{"+", .plus},
    .{"+=", .plus_equal},
    .{"-", .minus},
    .{"-=", .minus_equal},
    .{"*", .asterisk},
    .{"*=", .asterisk_equal},
    .{"/", .slash},
    .{"/=", .slash_equal},
    .{"%", .percent},
    .{"%=", .percent_equal},
    .{"&", .ampersand},
    .{"&&", .ampersand_ampersand},
    .{"&=", .ampersand_equal},
    .{"|", .pipe},
    .{"||", .pipe_pipe},
    .{"|=", .pipe_equal},
    .{"^", .carat},
    .{"^=", .carat_equal},
    .{"~", .tilde},
    .{"~=", .tilde_equal},
    .{"!", .bang},
    .{"!=", .bang_equal},
    .{"<", .l_angled},
    .{"<=", .l_angled_equal},
    .{">", .r_angled},
    .{">=", .r_angled_equal},
    .{"->", .arrow},
    .{"..", .range},
    .{"..=", .range_inclusive},
});

/// a map from string representations of keywords,
/// to their respective TokenType
pub const keyword_map = std.StaticStringMap(Kind).initComptime(.{
    .{"true", .true},
    .{"false", .false},
    .{"fn", .@"fn"},
    .{"ret", .ret},
    .{"const", .@"const"},
    .{"struct", .@"struct"},
    .{"enum", .@"enum"},
    .{"let", .let},
    .{"mut", .mut},
    .{"if", .@"if"},
    .{"switch", .@"switch"},
    .{"else", .@"else"},
    .{"whiel", .@"while"},
    .{"for", .@"for"},
    .{"loop", .loop},
    // not sure if i want to have impl blocks,
    // as i do also plan on extending the @This()
    // function in blink, which would clash with
    // rust style impl blocks.
    .{"impl", .impl},
    .{"defer", .@"defer"},
});

pub fn getKeyword(bytes: []const u8) ?Kind {
    return keyword_map.get(bytes);
}

pub fn lexeme(kind: Token.Kind) ?[]const u8 {
    return switch (kind) {
        // todo : actually fill this in
        else => null,
    };
}