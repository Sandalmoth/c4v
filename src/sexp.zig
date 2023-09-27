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

pub const ValueType = enum { atom, cons, acons, mcons, nil };
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

    errbuf: []const u8 = &.{},

    fn lexeme(scanner: Scanner, token: Token) []const u8 {
        return switch (token.type) {
            .L_PAREN => "(",
            .R_PAREN => ")",
            .L_BRACK => "[",
            .R_BRACK => "]",
            .L_CURLY => "{",
            .R_CURLY => "}",
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
    fn line_nr(scanner: Scanner, token: Token) u32 {
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
    fn line(scanner: Scanner, token: Token) []const u8 {
        var cursor: u32 = 0;
        var line_start: u32 = 0;
        while (cursor < token.start) : (cursor += 1) {
            if (scanner.src[cursor] == '\n') {
                line_start = cursor + 1;
            }
        }
        while (cursor < token.start) : (cursor += 1) {
            if (scanner.src[cursor] == '\n') {
                break;
            }
        }
        return scanner.src[line_start..cursor];
    }

    /// slow, intended for printing error messages
    fn column_nr(scanner: Scanner, token: Token) u32 {
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
            'a'...'z', 'A'...'Z', '0'...'9' => scanner.atom(),
            '"' => scanner.string(), // also an atom, but different lexing
            else => scanner.err("Unexpected character"),
        };
    }

    fn is_atom_char(c: u8) bool {
        // NOTE what do I want to allow?
        return switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9' => true,
            '-', '?', '.' => true,
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
        scanner.errbuf = text;
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

    // {
    //     var s2: Scanner = .{ .src = "(add 1 2)" };
    //     while (true) {
    //         const token = s2.scan();
    //         if (token.type == .EOF) break;
    //         std.debug.print("{}\t{}\t{}\t{s}\n", .{ s2.line_nr(token), s2.column_nr(token), token.type, s2.lexeme(token) });
    //     }
    // }
}

pub const Parser = struct {
    const ParserErrors = error{ParseFail};
    const nil = Value{ .nil = {} };
    const car = Value.car;
    const cdr = Value.cdr;

    alloc: std.mem.Allocator,
    conses: Pool([2]Value),
    interned: std.StringHashMap(u32),
    intern_data: std.ArrayList([]const u8),

    scanner: Scanner,
    current: Token,
    previous: Token,

    pub fn init(alloc: std.mem.Allocator, src: []const u8) Parser {
        var parser: Parser = .{
            .alloc = alloc,
            .conses = Pool([2]Value).init(alloc),
            .interned = std.StringHashMap(u32).init(alloc),
            .intern_data = std.ArrayList([]const u8).init(alloc),
            .scanner = Scanner{ .src = src },
            .current = Token{ .type = .ERROR, .start = 0 },
            .previous = Token{ .type = .ERROR, .start = 0 },
        };
        parser.advance();
        return parser;
    }

    pub fn deinit(parser: *Parser) void {
        parser.intern_data.deinit();
        parser.interned.deinit();
        parser.conses.deinit();
        parser.* = undefined;
    }

    // each call to parse reads one s-expression from source
    pub fn parse(parser: *Parser) ParserErrors!?Value {
        if (parser.match(.EOF)) {
            return null;
        }

        if (parser.match(.R_PAREN) or parser.match(.R_BRACK) or parser.match(.R_CURLY)) {
            // we cannot end a list or similar without first starting one
            parser.err(parser.current, "sequence closed without a matching opener");
            return error.ParseFail;
        }

        if (parser.match(.L_PAREN)) {
            return try parser.parse_list();
        } else if (parser.match(.L_BRACK)) {
            return try parser.parse_array();
        } else if (parser.match(.L_CURLY)) {
            return try parser.parse_map();
        } else {
            const result = Value{
                .atom = parser.interned.get(parser.scanner.lexeme(parser.current)) orelse blk: {
                    // we have yet to inter this string so do it now
                    std.debug.assert(parser.interned.count() == parser.intern_data.items.len);
                    const i = parser.interned.count();
                    const lexeme = parser.scanner.lexeme(parser.current);
                    parser.interned.put(lexeme, i) catch @panic("out of memory");
                    parser.intern_data.append(lexeme) catch @panic("out of memory");
                    break :blk i;
                },
            };
            parser.advance();
            return result;
        }

        unreachable;
    }

    fn parse_list(parser: *Parser) !Value {
        // parse a list
        if (parser.match(.R_PAREN)) {
            return nil;
        } else {
            const x = try parser.parse() orelse {
                parser.err(parser.previous, "unexpected end of list");
                return error.ParseFail;
            };
            return parser.cons(.cons, x, try parser.parse_list());
        }
    }

    fn parse_array(parser: *Parser) !Value {
        // parse a list
        if (parser.match(.R_BRACK)) {
            return nil;
        } else {
            const x = try parser.parse() orelse {
                parser.err(parser.previous, "unexpected end of array");
                return error.ParseFail;
            };
            return parser.cons(.acons, x, try parser.parse_array());
        }
    }

    fn parse_map(parser: *Parser) !Value {
        // parse a list
        if (parser.match(.R_CURLY)) {
            return nil;
        } else {
            const k = try parser.parse() orelse {
                parser.err(parser.previous, "unexpected end of map");
                return error.ParseFail;
            };
            const v = try parser.parse() orelse {
                parser.err(parser.previous, "unexpected end of map");
                return error.ParseFail;
            };
            return parser.cons(.mcons, parser.cons(.mcons, k, v), try parser.parse_map());
        }
    }

    pub fn print(parser: Parser, x: Value) void {
        switch (x) {
            .atom => |atom| std.debug.print("{s}", .{parser.intern_data.items[atom]}),
            .cons => parser.print_list(x),
            .acons => parser.print_array(x),
            .mcons => parser.print_map(x),
            .nil => std.debug.print("nil", .{}),
        }
    }

    fn print_list(parser: Parser, _x: Value) void {
        var x = _x;
        std.debug.print("(", .{});
        while (true) : (std.debug.print(" ", .{})) {
            parser.print(car(x));
            x = cdr(x);
            if (x == .nil) {
                break;
            }
            if (x != .cons) {
                std.debug.print(" . ", .{});
                parser.print(x);
                break;
            }
        }
        std.debug.print(")", .{});
    }

    fn print_array(parser: Parser, _x: Value) void {
        var x = _x;
        std.debug.print("[", .{});
        while (true) : (std.debug.print(" ", .{})) {
            parser.print(car(x));
            x = cdr(x);
            if (x == .nil) {
                break;
            }
        }
        std.debug.print("]", .{});
    }

    fn print_map(parser: Parser, _x: Value) void {
        var x = _x;
        std.debug.print("{{", .{});
        while (true) : (std.debug.print(" ", .{})) {
            parser.print(car(car(x)));
            std.debug.print(" ", .{});
            parser.print(cdr(car(x)));
            x = cdr(x);
            if (x == .nil) {
                break;
            }
        }
        std.debug.print("}}", .{});
    }

    fn consume(parser: *Parser, t: TokenType, message: []const u8) void {
        if (!(parser.current.type == t)) {
            parser.advance();
            return;
        }
        parser.err(parser.current, message);
    }

    fn match(parser: *Parser, t: TokenType) bool {
        if (!(parser.current.type == t)) {
            return false;
        }
        parser.advance();
        return true;
    }

    /// move forwards one token, report scanner errors if found
    fn advance(parser: *Parser) void {
        parser.previous = parser.current;
        parser.current = parser.scanner.scan();
        if (parser.current.type == .ERROR) {
            parser.err(parser.current, parser.scanner.errbuf);
        }
    }

    fn cons(parser: *Parser, comptime t: ValueType, a: Value, d: Value) Value {
        var pair = parser.conses.create() catch @panic("out of memory");
        pair[0] = a;
        pair[1] = d;
        return switch (t) {
            .cons => Value{ .cons = pair.ptr },
            .acons => Value{ .acons = pair.ptr },
            .mcons => Value{ .mcons = pair.ptr },
            else => @compileError("cons must be a cons type"),
        };
    }

    fn err(parser: Parser, token: Token, message: []const u8) void {
        std.debug.print("{}\n", .{token});
        std.debug.print("Error: {s}\n", .{message});
        std.debug.print("{}\t: {s}\n", .{ parser.scanner.line_nr(token), parser.scanner.line(token) });
        std.debug.print("\t  ", .{});
        var i = parser.scanner.column_nr(token);
        while (i > 1) : (i -= 1) {
            std.debug.print(" ", .{});
        }
        std.debug.print("^\n", .{});
    }
};

test "basic parsing" {
    std.debug.print("\n", .{});

    var parser = Parser.init(
        std.testing.allocator,
        "(add 1 (mul 2 2)) (conj \"hello\" \"world\")\n[1 2 3]\n{k0 v0 k1 v1}",
    );
    defer parser.deinit();

    while (try parser.parse()) |x| {
        parser.print(x);
        std.debug.print("\n", .{});
    }
}
