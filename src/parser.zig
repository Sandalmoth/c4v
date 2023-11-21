const std = @import("std");

const Pool = @import("pool.zig").Pool;
const ValueType = @import("value.zig").ValueType;
const Value = @import("value.zig").Value;
const Compiler = @import("compiler.zig").Compiler;

const keywords = std.ComptimeStringMap(Value, .{
    .{ "true", Value{ .TRUE = {} } },
    .{ "false", Value{ .FALSE = {} } },
    .{ "nil", Value{ .NIL = {} } },
});

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
