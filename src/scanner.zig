const std = @import("std");

pub const TokenType = enum {
    ERROR,

    L_PAREN, // (
    R_PAREN, // )
    L_BRACK, // [ unused
    R_BRACK, // ] unused
    L_CURLY, // { unused
    R_CURLY, // } unused

    DOT, //     .
    COLON, //   : unused
    QUOTE, //   '
    BACKQ, //   `
    UNQ, //     ,
    SPLUNQ, //  ,@
    WILD, //    _ unused
    // NOTE ; does not need lexing, since comments are treated as whitespace and discarded

    // all of the below must be followed by a delimiter
    // which is any parenthesis, whitespace, or :

    IDENT, //   [A-Za-z+-/*=<>?][A-Za-z0-9+-/*=<>?]+ except if the prefix would parse as a number
    NUMBER, //  [+-]?\d+\.?\d* (alternatively, a hexadecimal if prefixed by #[xX])
    STRING, //  ".*" (should be utf8 maybe?) may contain \n for newline, \ is \\, \" is "
};

pub const Token = struct {
    type: TokenType,
    lexeme: u32 = 0,
};

pub const ScanResult = struct {
    src: []const u8,
    tokens: std.ArrayList(Token),
    lexemes: std.ArrayList([]const u8),

    pub fn lexeme(result: ScanResult, t: Token) []const u8 {
        return switch (t.type) {
            .L_PAREN => "(",
            .R_PAREN => ")",
            .L_BRACK => "[",
            .R_BRACK => "]",
            .L_CURLY => "{",
            .R_CURLY => "}",
            .DOT => ".",
            .COLON => ":",
            .QUOTE => "'",
            .BACKQ => "`",
            .UNQ => ",",
            .SPLUNQ => ",@",
            .WILD => "_",
            else => result.lexemes.items[t.lexeme],
        };
    }
};

/// use an arena s.t. the allocated memory can be freed later
pub fn scan(alloc: std.mem.Allocator, src: []const u8) ScanResult {
    var result = ScanResult{
        .src = src,
        .tokens = std.ArrayList(Token).init(alloc),
        .lexemes = std.ArrayList([]const u8).init(alloc),
    };
    result.lexemes.append("<MISSING LEXEME>") catch @panic("scan: Out of memory");

    var scanner = Scanner{
        .alloc = alloc,
        .src = src,
        .result = &result,
    };
    scanner.begin();

    return result;
}

test "scan" {
    // TODO an actual test suite

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const alloc = arena.allocator();
    defer arena.deinit();

    const result = scan(alloc,
        \\ (hello . +  world) ; this is a comment
        \\ (+ 1 2.4)
        \\ (/ +3.4 -5.6)
        \\ "text..." "esc\"ape"
        \\ '()
        \\  #x123 #XFF;another comment
        \\ ' ,@ , ` : [ ] { } _`
    );

    std.debug.print("\n", .{});
    for (result.tokens.items) |t| {
        std.debug.print("{s}\t{}\n", .{ result.lexeme(t), t.type });
    }
}

const Scanner = struct {
    alloc: std.mem.Allocator,
    src: []const u8,
    result: *ScanResult,
    cursor: usize = 0,
    start: usize = 0,

    message: []const u8 = &.{},

    fn begin(scanner: *Scanner) void {
        if (scanner.at_end()) {
            return;
        }

        const c = scanner.at();
        return switch (c) {
            '(', ')', '[', ']', '{', '}', '.', '\'', '`', ',', ':', '_' => @call(
                .always_tail,
                Scanner.symbol,
                .{scanner},
            ),
            '#' => @call(.always_tail, Scanner.dispatch, .{scanner}),
            '0'...'9' => @call(.always_tail, Scanner.number, .{scanner}),
            '"' => @call(.always_tail, Scanner.string, .{scanner}),
            '+', '-' => @call(.always_tail, Scanner.sign, .{scanner}),
            else => blk: {
                if (is_initial(c)) {
                    break :blk @call(.always_tail, Scanner.identifier, .{scanner});
                } else if (is_whitespace(c)) {
                    break :blk @call(.always_tail, Scanner.whitespace, .{scanner});
                }
                scanner.message = "unexpected character";
                break :blk @call(.always_tail, Scanner.err, .{scanner});
            },
        };
    }

    fn at(scanner: *Scanner) u8 {
        return scanner.src[scanner.cursor];
    }

    fn at_end(scanner: *Scanner) bool {
        return scanner.cursor >= scanner.src.len;
    }

    /// test whether the next symbol is a delimiter or EOF
    fn require_delimiter(scanner: *Scanner) bool {
        const d = scanner.at_end() or is_delimiter(scanner.at());
        if (!d) {
            scanner.message = "delimiter expected";
            @call(.always_tail, Scanner.err, .{scanner});
        }
    }

    fn symbol(scanner: *Scanner) void {
        const c = scanner.at();
        scanner.cursor += 1;
        var t = Token{
            .type = switch (c) {
                '(' => .L_PAREN,
                ')' => .R_PAREN,
                '[' => .L_BRACK,
                ']' => .R_BRACK,
                '{' => .L_CURLY,
                '}' => .R_CURLY,
                '.' => blk: {
                    if (!scanner.at_end() and is_digit(scanner.at())) {
                        scanner.message = "skipping the leading 0 in numbers is not supported";
                        @call(.always_tail, Scanner.err, .{scanner});
                    }
                    break :blk .DOT;
                },
                ':' => .COLON,
                '\'' => .QUOTE,
                '`' => .BACKQ,
                '_' => .WILD,
                ',' => blk: {
                    if (!scanner.at_end() and scanner.at() == '@') {
                        scanner.cursor += 1;
                        break :blk .SPLUNQ;
                    }
                    break :blk .UNQ;
                },
                else => unreachable,
            },
        };
        scanner.result.tokens.append(t) catch @panic("Scanner.symbol: out of memory");

        @call(.always_tail, Scanner.begin, .{scanner});
    }

    /// misc special syntax
    fn dispatch(scanner: *Scanner) void {
        if (scanner.cursor + 1 >= scanner.src.len) {
            scanner.message = "invalid dispatch";
            @call(.always_tail, Scanner.err, .{scanner});
        }
        const p = scanner.src[scanner.cursor + 1];
        if (p == 'x' or p == 'X') {
            if (scanner.cursor + 2 < scanner.src.len) {
                const p2 = scanner.src[scanner.cursor + 2];
                if (is_hexadecimal_digit(p2)) {
                    @call(.always_tail, Scanner.number, .{scanner});
                }
            }
        }
        scanner.message = "invalid dispatch";
        @call(.always_tail, Scanner.err, .{scanner});
    }

    fn number(scanner: *Scanner) void {
        scanner.start = scanner.cursor;
        if (scanner.at() == '#') {
            // hex number, dispatch has already made sure
            scanner.cursor += 2;
            while (!scanner.at_end() and
                is_hexadecimal_digit(scanner.at())) : (scanner.cursor += 1)
            {}
        } else {
            // regular number
            if (scanner.at() == '+' or scanner.at() == '-') {
                scanner.cursor += 1;
            }
            while (!scanner.at_end() and is_digit(scanner.at())) : (scanner.cursor += 1) {}
            if (!scanner.at_end() and scanner.at() == '.') {
                scanner.cursor += 1;
            }
            while (!scanner.at_end() and is_digit(scanner.at())) : (scanner.cursor += 1) {}
        }

        scanner.result.tokens.append(Token{
            .type = .NUMBER,
            .lexeme = @intCast(scanner.result.lexemes.items.len),
        }) catch @panic("Scanner.number: out of memory");
        scanner.result.lexemes.append(
            scanner.src[scanner.start..scanner.cursor],
        ) catch @panic("Scanner.number: out of memory");

        @call(.always_tail, Scanner.begin, .{scanner});
    }

    fn string(scanner: *Scanner) void {
        scanner.cursor += 1;
        scanner.start = scanner.cursor;
        var prev = scanner.at();
        while (!scanner.at_end()) : (scanner.cursor += 1) {
            if (scanner.at() == '"' and prev != '\\') {
                break;
            }
            prev = scanner.at();
        }

        // desugar special chars
        // we could optimize to do this only if they are present
        var s = scanner.alloc.dupe(
            u8,
            scanner.src[scanner.start..scanner.cursor],
        ) catch @panic("Scanner.string out of memory");
        var i: usize = 0;
        var j: usize = 0;
        while (i < s.len) : (i += 1) {
            if (s[i] == '\\') {
                if (i + 1 >= s.len) {
                    scanner.message = "invalid escape sequence in string";
                    @call(.always_tail, Scanner.err, .{scanner});
                }
                switch (s[i + 1]) {
                    '\\' => s[j] = '\\',
                    'n' => s[j] = '\n',
                    '"' => s[j] = '\"',
                    else => {
                        scanner.message = "invalid escape sequence in string";
                        @call(.always_tail, Scanner.err, .{scanner});
                    },
                }
                i += 1; // for the extra character
            } else {
                s[j] = s[i];
            }
            j += 1;
        }
        // note that since scanner.alloc should be an arena
        // we don't care about keeping track of the memory lost when slicing
        s = s[0..j];

        scanner.result.tokens.append(Token{
            .type = .STRING,
            .lexeme = @intCast(scanner.result.lexemes.items.len),
        }) catch @panic("Scanner.string: out of memory");
        scanner.result.lexemes.append(s) catch @panic("Scanner.string: out of memory");

        scanner.cursor += 1; // skip terminating "

        @call(.always_tail, Scanner.begin, .{scanner});
    }

    fn identifier(scanner: *Scanner) void {
        scanner.start = scanner.cursor;
        while (!scanner.at_end() and is_identifier(scanner.at())) : (scanner.cursor += 1) {}
        scanner.result.tokens.append(Token{
            .type = .IDENT,
            .lexeme = @intCast(scanner.result.lexemes.items.len),
        }) catch @panic("Scanner.identifier: out of memory");
        scanner.result.lexemes.append(
            scanner.src[scanner.start..scanner.cursor],
        ) catch @panic("Scanner.identifier: out of memory");

        @call(.always_tail, Scanner.begin, .{scanner});
    }

    fn sign(scanner: *Scanner) void {
        if (scanner.cursor + 1 >= scanner.src.len) {
            // just the sign left, cannot be a number
            @call(.always_tail, Scanner.identifier, .{scanner});
        }
        // in a number, the next char must be a digit
        const p = scanner.src[scanner.cursor + 1];
        if (p == '.') {
            scanner.message = "skipping the leading 0 in numbers is not supported";
            @call(.always_tail, Scanner.err, .{scanner});
        }

        if (is_digit(p)) {
            @call(.always_tail, Scanner.number, .{scanner});
        } else {
            // otherwise it's an identifier
            @call(.always_tail, Scanner.identifier, .{scanner});
        }
    }

    fn whitespace(scanner: *Scanner) void {
        const c = scanner.at();
        scanner.cursor += 1;
        if (is_comment(c)) {
            while (!scanner.at_end() and !is_line_ending(scanner.at())) : (scanner.cursor += 1) {}
        }
        while (!scanner.at_end() and is_whitespace(scanner.at()) and
            !is_comment(scanner.at())) : (scanner.cursor += 1)
        {}

        @call(.always_tail, Scanner.begin, .{scanner});
    }

    fn err(scanner: *Scanner) void {
        while (scanner.cursor < scanner.src.len and
            !is_delimiter(scanner.at())) : (scanner.cursor += 1)
        {}

        var t = Token{
            .type = .ERROR,
            .lexeme = @intCast(scanner.result.lexemes.items.len),
        };
        scanner.start = scanner.cursor;
        scanner.result.tokens.append(t) catch @panic("Scanner.err: out of memory");

        scanner.result.lexemes.append(std.fmt.allocPrint(
            scanner.alloc,
            "{s} in '{s}'",
            .{ scanner.message, scanner.src[scanner.start..scanner.cursor] },
        ) catch @panic("Scanner.err: out of memory")) catch @panic("Scanner.err: out of memory");

        @call(.always_tail, Scanner.begin, .{scanner});
    }
};

fn is_line_ending(c: u8) bool {
    return switch (c) {
        '\n', '\r' => true,
        else => false,
    };
}

fn is_comment(c: u8) bool {
    return switch (c) {
        ';' => true,
        else => false,
    };
}

fn is_whitespace(c: u8) bool {
    return is_line_ending(c) or is_comment(c) or switch (c) {
        ' ', '\t' => true,
        else => false,
    };
}

fn is_delimiter(c: u8) bool {
    return is_whitespace(c) or switch (c) {
        ':', '(', ')', '[', ']', '{', '}' => true,
        else => false,
    };
}

fn is_letter(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z' => true,
        else => false,
    };
}

fn is_digit(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        else => false,
    };
}

fn is_hexadecimal_digit(c: u8) bool {
    return is_digit(c) or switch (c) {
        'a'...'f', 'A'...'F' => true,
        else => false,
    };
}

/// any character that can start a symbol
fn is_initial(c: u8) bool {
    return is_letter(c) or switch (c) {
        '+', '-', '/', '*', '=', '<', '>', '?' => true,
        else => false,
    };
}

fn is_identifier(c: u8) bool {
    return is_initial(c) or is_digit(c);
}
