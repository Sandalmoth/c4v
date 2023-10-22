const std = @import("std");

pub const TokenType = enum {
    ERROR,

    L_PAREN, // (
    R_PAREN, // )
    L_BRACK, // [ unused
    R_BRACK, // ] unused
    L_CURLY, // {
    R_CURLY, // }

    DOT, //     .
    COLON, //   :
    QUOTE, //   '
    BACKQ, //   `
    UNQ, //     ,
    SPLUNQ, //  ,@
    // NOTE ; does not need lexing, since comments are treated as whitespace and discarded

    // all of the below must be followed by a delimiter
    // which is any parenthesis, whitespace, or :

    IDENT, //   [A-Za-z+-/*=<>?][A-Za-z0-9+-/*=<>?]+ except if the prefix would parse as a number
    CHAR, //    #\. (utf8) or the dot could be #\null #\newline
    NUMBER, //  [+-]?\d+\.?\d* (alternatively, a hexadecimal if prefixed by #[xX])
    STRING, //  ".*" (utf8) may not contain linebreaks, but may contain \n, \ is \\
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
};

pub const ScanResult = struct {
    src: []const u8,
    tokens: std.ArrayList(Token),
};

/// use an arena s.t. the allocated memory can be freed later
pub fn scan(alloc: std.mem.Allocator, src: []const u8) ScanResult {
    var result = ScanResult{
        .src = src,
        .tokens = std.ArrayList(Token).init(alloc),
    };
    var scanner = Scanner{
        .alloc = alloc,
        .src = src,
        .result = &result,
    };
    scanner.begin();

    return result;
}

test "scan" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const alloc = arena.allocator();
    defer arena.deinit();

    const result = scan(alloc,
        \\ (hello . +  world) ; this is a comment
        \\ (+ 1 2)
    );

    std.debug.print("\n", .{});
    for (result.tokens.items) |t| {
        std.debug.print("{s}\t{}\n", .{ t.lexeme, t.type });
    }
}

const Scanner = struct {
    alloc: std.mem.Allocator,
    src: []const u8,
    result: *ScanResult,
    cursor: usize = 0,
    start: usize = 0,

    fn begin(scanner: *Scanner) void {
        if (scanner.at_end()) {
            return;
        }

        const c = scanner.at();
        return switch (c) {
            '(', ')', '[', ']', '{', '}', '.', '\'', '`', ',', ':' => @call(
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
                break :blk @call(.always_tail, Scanner.err, .{ scanner, "unexpected character" });
            },
        };
    }

    fn at(scanner: *Scanner) u8 {
        return scanner.src[scanner.cursor];
    }

    fn at_end(scanner: *Scanner) bool {
        return scanner.cursor >= scanner.src.len;
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
                '.' => .DOT,
                ':' => .COLON,
                '\'' => .QUOTE,
                '`' => .BACKQ,
                ',' => blk: {
                    if (!scanner.at_end() and scanner.at() == '@') {
                        scanner.cursor += 1;
                        break :blk .SPLUNQ;
                    }
                    break :blk .UNQ;
                },
                else => unreachable,
            },
            .lexeme = scanner.src[scanner.start..scanner.cursor],
        };
        scanner.result.tokens.append(t) catch @panic("c4v scanner out of memory");
    }

    fn dispatch(scanner: *Scanner) void {
        _ = scanner;
    }

    fn number(scanner: *Scanner) void {
        _ = scanner;
    }

    fn string(scanner: *Scanner) void {
        _ = scanner;
    }

    fn identifier(scanner: *Scanner) void {
        _ = scanner;
    }

    fn sign(scanner: *Scanner) void {
        _ = scanner;
    }

    fn whitespace(scanner: *Scanner) void {
        _ = scanner;
    }

    fn err(scanner: *Scanner, message: []const u8) void {
        while (scanner.cursor < scanner.src.len and
            !is_delimiter(scanner.at())) : (scanner.cursor += 1)
        {}
        var t = Token{
            .type = .ERROR,
            .lexeme = std.fmt.allocPrint(
                scanner.alloc,
                "{s} in '{}'",
                .{ message, scanner.src[scanner.start..scanner.cursor] },
            ),
        };
        scanner.start = scanner.cursor;
        scanner.result.tokens.append(t);

        @call(.always_tail, Scanner.begin, .{scanner});
    }
};

fn is_line_ending(c: u8) bool {
    return switch (c) {
        '\n', '\r' => true,
        else => false,
    };
}

fn is_whitespace(c: u8) bool {
    return is_line_ending(c) or switch (c) {
        ' ', '\t' => true,
        else => false,
    };
}

fn is_delimiter(c: u8) bool {
    return is_whitespace(c) or switch (c) {
        ':', '(', ')', '[', ']', '{', '}', ';' => true,
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

fn is_initial(c: u8) bool {
    return is_letter(c) or switch (c) {
        '+', '-', '/', '*', '=', '<', '>', '?' => true,
        else => false,
    };
}
