const std = @import("std");

const Pool = @import("pool.zig").Pool;
const ValueType = @import("value.zig").ValueType;
const Value = @import("value.zig").Value;
const Compiler = @import("compiler.zig").Compiler;

const TokenType = enum {
    // syntax
    L_PAREN,
    R_PAREN,
    DOT,
    QUOTE,
    // atoms
    NUMBER,
    STRING,
    IDENTIFIER,
    // keywords
    TRUE,
    FALSE,
    NIL,
    // signals
    ERROR,
    EOF,
};

const identifier_type = std.ComptimeStringMap(TokenType, .{
    .{ "true", .TRUE },
    .{ "false", .FALSE },
    .{ "nil", .NIL },
});

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
            .QUOTE => "'",
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
    /// returns the line number of a token
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
    /// returns the (literal) line where a token occurred
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
    /// return the column of a token (i.e. what num char in the line)
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
            '.' => Token{ .type = .DOT, .start = scanner.start },
            '\'' => Token{ .type = .QUOTE, .start = scanner.start },
            '0'...'9' => scanner.maybe_number(), // if not a number, calls identifier
            '|' => scanner.identifier(), // form an id between ||
            '"' => scanner.string(), // form a string between ""
            else => blk: {
                if (is_atom_char(c)) {
                    break :blk scanner.identifier();
                }
                break :blk scanner.err("Unexpected character");
            },
        };
    }

    fn is_atom_char(c: u8) bool {
        return switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9' => true,
            '!', '$', '%', '&' => true,
            '*', '+', '-', '.', '/' => true,
            ':', '<', '=', '>' => true,
            '?', '@', '^', '_', '~' => true,
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

// test "Scanner" {
//     var s: Scanner = .{ .src = "(hello?\n\"world\" [" };

//     try std.testing.expectEqual(TokenType.L_PAREN, s.scan().type);
//     try std.testing.expectEqual(TokenType.ATOM, s.scan().type);
//     try std.testing.expectEqual(TokenType.ATOM, s.scan().type);
//     try std.testing.expectEqual(TokenType.L_BRACK, s.scan().type);

//     // {
//     //     var s2: Scanner = .{ .src = "(add 1 2)" };
//     //     while (true) {
//     //         const token = s2.scan();
//     //         if (token.type == .EOF) break;
//     //         std.debug.print("{}\t{}\t{}\t{s}\n", .{ s2.line_nr(token), s2.column_nr(token), token.type, s2.lexeme(token) });
//     //     }
//     // }
// }

pub const Parser = struct {
    const ParserErrors = error{ParseFail};
    const nil = Value{ .nil = {} };
    const car = Value.car;
    const cdr = Value.cdr;

    compiler: *Compiler,

    scanner: Scanner,
    current: Token,
    previous: Token,

    pub fn init(src: []const u8, compiler: *Compiler) Parser {
        var parser: Parser = .{
            .compiler = compiler,
            .scanner = Scanner{ .src = src },
            .current = Token{ .type = .ERROR, .start = 0 },
            .previous = Token{ .type = .ERROR, .start = 0 },
        };
        parser.advance();
        return parser;
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

    pub fn print(parser: Parser, x: Value) void {
        switch (x) {
            .atom => |atom| std.debug.print("{s}", .{parser.intern_data.items[atom]}),
            .cons => parser.print_list(x),
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

// test "basic parsing" {
//     std.debug.print("\n", .{});

//     var parser = Parser.init(
//         std.testing.allocator,
//         "(add 1 (mul 2 2)) (conj \"hello\" \"world\")\n[1 2 3]\n{k0 v0 k1 v1}",
//     );
//     defer parser.deinit();

//     while (try parser.parse()) |x| {
//         parser.print(x);
//         std.debug.print("\n", .{});
//     }
// }
