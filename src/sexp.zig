const std = @import("std");

const Pool = @import("pool.zig").Pool;

// we have normal list structures like so
// (a b (c d) ())
// note that () is nil
// and then arrays
// [a b c]
// and they are both normal cons-lists
// and then finaly maps
// {k0 v0 k1 v1}
// which is actually a list of pairs like so
// ((k0 . v0) (k1 . v1))
// and to be able to disambiguate these structures (regardless of nesting)
// we just make them out of three types of cons, with identical semantics

pub const ValueType = enum { atom, cons, nil };
pub const Value = union(ValueType) {
    atom: u32, // key into interning table
    cons: [*]Value,
    acons: [*]Value, // annotate that we are parsing an array, not a list
    mcons: [*]Value, // annotate that we are parsing a dict, not a list
    nil: void,

    pub fn car(x: Value) Value {
        switch (x) {
            .cons => |cons| return cons[0],
            .acons => |acons| return acons[0],
            .mcons => |mcons| return mcons[0],
            else => @panic("car only works on a cons variant"),
        }
    }

    pub fn cdr(x: Value) Value {
        switch (x) {
            .cons => |cons| return cons[1],
            .acons => |acons| return acons[1],
            .mcons => |mcons| return mcons[1],
            else => @panic("cdr only works on a cons variant"),
        }
    }
};

const TokenType = enum {
    L_PAREN,
    R_PAREN,
    L_BRACK,
    R_BRACK,
    L_CURLY,
    R_CURLY,
    DOT,
    ATOM,
    ERROR,
    EOF,
};

const Token = struct {
    type: TokenType,
    start: u32,
};

const Scanner = struct {
    src: []const u8,
    // mark the start and end of the lexeme we're currently scanning over
    // in the sense that src[start..cursor] is the lexeme
    start: u32 = 0,
    cursor: u32 = 0,

    errtext: []const u8 = &.{},

    fn lexeme(scanner: Scanner, token: Token) []const u8 {
        return switch (token.type) {
            .L_PAREN => "(",
            .R_PAREN => ")",
            .L_BRACK => "[",
            .R_BRACK => "]",
            .L_CURLY => "{",
            .R_CURLY => "}",
            .DOT => ".",
            .ATOM => {
                var s: Scanner = .{
                    .src = scanner.src,
                    .start = token.start,
                    .cursor = token.start,
                };
                _ = s.scan();
                return scanner.src[token.start..s.cursor];
            },
            .ERROR => "<ERROR>",
            .EOF => "<EOF>",
            // else => "<UNPRINTABLE>",
        };
    }

    /// slow, intended for printing error messages
    fn line(scanner: Scanner, token: Token) u32 {
        var cursor: u32 = 0;
        var ln: u32 = 0;
        while (cursor < token.start) : (cursor += 1) {
            if (scanner.src[cursor] == '\n') {
                ln += 1;
            }
        }
        return ln;
    }

    /// slow, intended for printing error messages
    fn column(scanner: Scanner, token: Token) u32 {
        var cursor: u32 = 0;
        var col: u32 = 0;
        while (cursor < token.start) : (cursor += 1) {
            col += 1;
            if (scanner.src[cursor] == '\n') {
                col = 0;
            }
        }
        return col;
    }

    fn scan(scanner: *Scanner) Token {
        scanner.skip_whitespace();
        scanner.start = scanner.cursor;

        if (scanner.at_end()) {
            return Token{ .type = .EOF, .start = scanner.start };
        }

        const c = scanner.advance();
        return switch (c) {
            '(' => Token{ .type = .L_PAREN, .start = scanner.start },
            ')' => Token{ .type = .R_PAREN, .start = scanner.start },
            '[' => Token{ .type = .L_BRACK, .start = scanner.start },
            ']' => Token{ .type = .R_BRACK, .start = scanner.start },
            '{' => Token{ .type = .L_CURLY, .start = scanner.start },
            '}' => Token{ .type = .R_CURLY, .start = scanner.start },
            '.' => Token{ .type = .DOT, .start = scanner.start },
            'a'...'z', 'A'...'Z', '0'...'9' => scanner.atom(),
            '"' => scanner.string(), // also an atom, but different lexing
            else => scanner.err("Unexpected character"),
        };
    }

    fn is_atom_char(c: u8) bool {
        // NOTE what do I want to allow?
        return switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9' => true,
            '-', '?' => true,
            // '-', '+', '*', '/' => true,
            // '=', '<', '>' => true,
            // '!', '?', '_', '.' => true,
            else => false,
        };
    }

    fn atom(scanner: *Scanner) Token {
        while (is_atom_char(scanner.peek())) : (_ = scanner.advance()) {}
        return Token{ .type = .ATOM, .start = scanner.start };
    }

    fn string(scanner: *Scanner) Token {
        while (!scanner.at_end() and scanner.peek() != '"') : (_ = scanner.advance()) {}
        if (scanner.at_end()) {
            return scanner.err("Unterminated string");
        }
        _ = scanner.advance();
        return Token{ .type = .ATOM, .start = scanner.start };
    }

    fn err(scanner: *Scanner, text: []const u8) Token {
        scanner.errtext = text;
        return Token{ .type = .ERROR, .start = scanner.start };
    }

    fn peek(scanner: *Scanner) u8 {
        if (scanner.at_end()) {
            return 0;
        }
        return scanner.src[scanner.cursor];
    }

    fn advance(scanner: *Scanner) u8 {
        const c = scanner.src[scanner.cursor];
        scanner.cursor += 1;
        return c;
    }

    fn at_end(scanner: Scanner) bool {
        return scanner.cursor == scanner.src.len;
    }

    fn skip_whitespace(scanner: *Scanner) void {
        while (true) {
            const c = scanner.peek();
            switch (c) {
                ' ', '\n', '\r', '\t' => _ = scanner.advance(),
                ';' => {
                    while (!scanner.at_end() and scanner.peek() != '\n') : (_ = scanner.advance()) {}
                },
                else => return,
            }
        }
    }
};

test "Scanner" {
    var s: Scanner = .{ .src = "(hello?\n\"world\" [" };

    try std.testing.expectEqual(TokenType.L_PAREN, s.scan().type);
    try std.testing.expectEqual(TokenType.ATOM, s.scan().type);
    try std.testing.expectEqual(TokenType.ATOM, s.scan().type);
    try std.testing.expectEqual(TokenType.L_BRACK, s.scan().type);

    // while (true) {
    //     const token = s.scan();
    //     if (token.type == .EOF) break;
    //     std.debug.print("{}\t{}\t{}\t{s}\n", .{ s.line(token), s.column(token), token.type, s.lexeme(token) });
    // }
}

// pub const Parser = struct {
//     const nil = Value{ .nil = {} };
//     const car = Value.car;
//     const cdr = Value.cdr;

//     alloc: std.mem.Allocator,
//     conses: Pool([2]Value),
//     interned: std.StringHashMap(u32),
//     intern_data: std.ArrayList([]const u8),

//     scanner: Scanner,

//     pub fn init(alloc: std.mem.Allocator, src: []const u8) Parser {
//         return .{
//             .alloc = alloc,
//             .conses = Pool([2]Value).init(alloc),
//             .interned = std.StringHashMap(u32).init(alloc),
//             .intern_data = std.ArrayList([]const u8).init(alloc),
//             .scanner = Scanner{ .src = src },
//         };
//     }

//     pub fn deinit(parser: *Parser) void {
//         parser.intern_data.deinit();
//         parser.interned.deinit();
//         parser.conses.deinit();
//         parser.* = undefined;
//     }

//     // each call to parse reads one s-expression
//     pub fn parse(parser: *Parser) ?Value {
//         while (parser.scanner.scan()) {
//             if (parser.scanner.is("(")) {
//                 parser.scanner.consume("(") catch @panic("expect (");
//                 return parser.parselist();
//             } else {
//                 return Value{ .atom = parser.interned.get(parser.scanner.token) orelse blk: {
//                     std.debug.assert(parser.interned.count() == parser.intern_data.items.len);
//                     const i = parser.interned.count();
//                     parser.interned.put(parser.scanner.token, i) catch @panic("out of memory");
//                     parser.intern_data.append(parser.scanner.token) catch @panic("out of memory");
//                     break :blk i;
//                 } };
//             }
//         }

//         return null;
//     }

//     fn parselist(parser: *Parser) Value {
//         // parse a list
//         if (parser.scanner.is(")")) {
//             return nil;
//         } else if (parser.scanner.is(".")) {
//             const x = parser.parse() orelse @panic("unexpected end of list");
//             parser.scanner.consume(")") catch @panic("expect )");
//             return x;
//         } else {
//             const x = parser.parse() orelse @panic("unexpected end of list");
//             return parser.cons(x, parser.parselist());
//         }
//     }

//     pub fn print(parser: Parser, x: Value) void {
//         switch (x) {
//             .atom => |atom| std.debug.print("atom:{s}", .{parser.intern_data.items[atom]}),
//             .cons => parser.printlist(x),
//             .nil => std.debug.print("nil", .{}),
//         }
//     }

//     fn printlist(parser: Parser, _x: Value) void {
//         var x = _x;
//         std.debug.print("(", .{});
//         while (true) : (std.debug.print(" ", .{})) {
//             parser.print(car(x));
//             x = cdr(x);
//             if (x == .nil) {
//                 break;
//             }
//             if (x != .cons) {
//                 std.debug.print(" . ", .{});
//                 parser.print(x);
//                 break;
//             }
//         }
//         std.debug.print(")", .{});
//     }

//     fn cons(parser: *Parser, a: Value, d: Value) Value {
//         var pair = parser.conses.create() catch @panic("out of memory");
//         pair[0] = a;
//         pair[1] = d;
//         return Value{ .cons = pair.ptr };
//     }

//     // fn peek
// };

// test "basic parsing" {
//     std.debug.print("\n", .{});

//     var parser = Parser.init(
//         std.testing.allocator,
//         "(+ 1 2)",
//     );
//     defer parser.deinit();

//     while (parser.parse()) |x| {
//         parser.print(x);
//         std.debug.print("\n", .{});
//     }
// }
