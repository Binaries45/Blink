const std = @import("std");
const Token = @import("Token.zig");

pub const Lexer = @This();

buffer: [:0]const u8,
pos: usize,

const Self = @This();

const State = enum {
    invalid,
    start,
    expect_newline,

    at,
    pound,
    dollar,
    period,

    equal,
    plus,
    minus,
    asterisk,
    slash,
    percent,
    ampersand,
    pipe,
    carat,
    tilde,
    bang,
    l_angled,
    l_angled_angled,
    r_angled,
    r_angled_angled,

    range,

    identifier,
    builtin,
    string,
    char,
    int,
    int_period,
    float,
    floatExp,
};

pub fn init(src: [:0]const u8) Self {
    return .{
        .buffer = src,
        .pos = 0,
    };
}

pub fn next(self: *Self) Token {
    var result: Token = .{
        .kind = undefined,
        .start = self.pos,
        .end = undefined,
    };

    state: switch (State.start) {
        .invalid => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                0 => if (self.pos == self.buffer.len) {
                    result.kind = .unknown;
                } else continue :state .invalid,
                '\n' => result.kind = .unknown,
                else => continue :state .invalid,
            }
        },
        .start => switch (self.buffer[self.pos]) {
            0 => if (self.pos == self.buffer.len) {
                return .{
                    .kind = .eof,
                    .start = self.pos,
                    .end = self.pos,
                };
            } else continue :state .invalid,
            ' ', '\t', '\n', '\r' => {
                self.pos += 1;
                result.start = self.pos;
                continue :state .start;
            },
            '"' => {
                result.kind = .string;
                continue :state .string;
            },
            '\'' => {
                result.kind = .char;
                continue :state .char;
            },
            'a'...'z', 'A'...'Z', '_' => {
                result.kind = .identifier;
                continue :state .identifier;
            },
            '0'...'9' => {
                result.kind = .int_literal;
                self.pos += 1;
                continue :state .int;
            },
            '@' => continue :state .at,
            '#' => continue :state .pound,
            '$' => continue :state .dollar,
            '=' => continue :state .equal,
            '+' => continue :state .plus,
            '-' => continue :state .minus,
            '*' => continue :state .asterisk,
            '/' => continue :state .slash,
            '%' => continue :state .percent,
            '&' => continue :state .ampersand,
            '|' => continue :state .pipe,
            '^' => continue :state .carat,
            '~' => continue :state .tilde,
            '!' => continue :state .bang,
            '<' => continue :state .l_angled,
            '>' => continue :state .r_angled,
            '.' => continue :state .period,
            '(' => {
                result.kind = .l_paren;
                self.pos += 1;
            },
            ')' => {
                result.kind = .r_paren;
                self.pos += 1;
            },
            '[' => {
                result.kind = .l_bracket;
                self.pos += 1;
            },
            ']' => {
                result.kind = .r_brace;
                self.pos += 1;
            },
            '{' => {
                result.kind = .l_brace;
                self.pos += 1;
            },
            '}' => {
                result.kind = .r_brace;
                self.pos += 1;
            },
            ';' => {
                result.kind = .semicolon;
                self.pos += 1;
            },
            ':' => {
                result.kind = .colon;
                self.pos += 1;
            },
            ',' => {
                result.kind = .comma;
                self.pos += 1;
            },
            '?' => {
                result.kind = .question;
                self.pos += 1;
            },
             else => continue :state .invalid,
        },
        .expect_newline => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                0 => {
                    if (self.pos == self.buffer.len) {
                        result.kind = .unknown;
                    } else continue :state .invalid;
                },
                '\n' => {
                    self.pos += 1;
                    result.start = self.pos;
                    continue :state .start;
                },
                else => continue :state .invalid,
            }
        },
        .at => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                0, '\n' => result.kind = .unknown,
                'a'...'z', 'A'...'Z', '_' => {
                    result.kind = .builtin;
                    continue :state .builtin;
                },
                else => continue :state .invalid,
            }
        },
        .pound => unreachable,
        .dollar => unreachable,
        .identifier => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                'a'...'z', 'A'...'A', '0'...'9', '_' => continue :state .identifier,
                else => {
                    const ident = self.buffer[result.start..self.pos];
                    if (Token.getKeyword(ident)) |kword| {
                        result.kind = kword;
                    }
                },
            }
        },
        .builtin => {
            self.pos += 1;
            switch(self.buffer[self.pos]) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .builtin,
                else => {},
            }
        },
        .string => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                0 => if (self.pos != self.buffer.len) {
                    continue :state .invalid;
                } else {
                    result.kind = .unknown;
                },
                // todo : escape sequences
                '"' => self.pos += 1,
                else => continue :state .string,
            }
        },
        .char => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                0 => if (self.pos != self.buffer.len) {
                    continue :state .invalid;
                } else {
                    result.kind = .unknown;
                },
                // todo : escape sequences
                '\'' => self.pos += 1,
                else => continue :state .char,
            }
        },
        .period => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '.' => continue :state .range,
                else => result.kind = .period,
            }
        },
        .equal => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .equal_equal;
                    self.pos += 1;
                },
                else => result.kind = .equal,
            }
        },
        .plus => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .plus_equal;
                    self.pos += 1;
                },
                else => result.kind = .plus,
            }
        },
        .minus => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .minus_equal;
                    self.pos += 1;
                },
                '>' => {
                    result.kind = .arrow;
                    self.pos += 1;
                },
                else => result.kind = .minus,
            }
        },
        .asterisk => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .asterisk_equal;
                    self.pos += 1;
                },
                else => result.kind = .asterisk,
            }
        },
        .slash => {
            // todo : comment & doc comment support
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .slash_equal;
                    self.pos += 1;
                },
                else => result.kind = .slash,
            }
        },
        .percent => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .percent_equal;
                    self.pos += 1;
                },
                else => result.kind = .percent,
            }
        },
        .ampersand => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .ampersand_equal;
                    self.pos += 1;
                },
                '&' => {
                    result.kind = .ampersand_ampersand;
                    self.pos += 1;
                },
                else => result.kind = .ampersand,
            }
        },
        .pipe => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .pipe_equal;
                    self.pos += 1;
                },
                '|' => {
                    result.kind = .pipe_pipe;
                    self.pos += 1;
                },
                else => result.kind = .pipe,
            }
        },
        .carat => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .carat_equal;
                    self.pos += 1;
                },
                else => result.kind = .caret,
            }
        },
        .tilde => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .tilde_equal;
                    self.pos += 1;
                },
                else => result.kind = .tilde,
            }
        },
        .bang => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .bang_equal;
                    self.pos += 1;
                },
                else => result.kind = .bang,
            }
        },
        .l_angled => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .l_angled_equal;
                    self.pos += 1;
                },
                '<' => continue :state .l_angled_angled,
                else => result.kind = .l_angled,
            }
        },
        .l_angled_angled => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .l_angled_angled_equal;
                    self.pos += 1;
                },
                else => result.kind = .l_angled_angled,
            }
        },
        .r_angled => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .r_angled_equal;
                    self.pos += 1;
                },
                '<' => continue :state .r_angled_angled,
                else => result.kind = .l_angled,
            }
        },
        .r_angled_angled => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .r_angled_angled_equal;
                    self.pos += 1;
                },
                else => result.kind = .r_angled_angled,
            }
        },
        .range => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '=' => {
                    result.kind = .range_inclusive;
                    self.pos += 1;
                },
                else => result.kind = .range,
            }
        },
        .int => {
            switch (self.buffer[self.pos]) {
                '.' => {
                    result.kind = .float_literal;
                    continue :state .int_period;
                },
                'e' => {
                    result.kind = .float_literal;
                    continue :state .floatExp;
                },
                '0'...'9' => {
                    self.pos += 1;
                    continue :state .int;
                },
                else => {},
            }
        },
        .int_period => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                'e' => continue :state .floatExp,
                '0'...'9' => {
                    self.pos += 1;
                    continue :state .float;
                },
                else => self.pos -= 1,
            }
        },
        .float => {
            switch (self.buffer[self.pos]) {
                'e' => continue :state .floatExp,
                '0'...'9' => {
                    self.pos += 1;
                    continue :state .float;
                },
                else => {},
            }
        },
        .floatExp => {
            self.pos += 1;
            switch (self.buffer[self.pos]) {
                '-', '+' => {
                    self.pos += 1;
                    continue :state .float;
                },
                else => continue :state .float,
            }
        },
    }

    result.end = self.pos;
    return result;
}