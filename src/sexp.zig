const std = @import("std");

const Pool = @import("pool.zig").Pool;

pub const ValueType = enum { atom, cons, nil };
pub const Value = union(ValueType) {
    atom: u32, // key into interning table
    cons: [*]Value,
    nil: void,

    pub fn car(x: Value) Value {
        std.debug.assert(x == .cons);
        return x.cons[0];
    }

    pub fn cdr(x: Value) Value {
        std.debug.assert(x == .cons);
        return x.cons[1];
    }
};

const Scanner = struct {
    src: []const u8,
    cursor: u32 = 0,
    token: []const u8 = &.{},

    fn at(scanner: Scanner) u8 {
        return scanner.src[scanner.cursor];
    }

    fn atWhitespace(scanner: Scanner) bool {
        std.debug.assert(scanner.cursor < scanner.src.len);
        return switch (scanner.src[scanner.cursor]) {
            ' ', '\t', '\n', '\r' => true,
            else => false,
        };
    }

    fn is(scanner: Scanner, token: []const u8) bool {
        return std.mem.eql(u8, token, scanner.token);
    }

    // advance and read the next token into the buffer
    fn scan(scanner: *Scanner) bool {
        while (scanner.cursor < scanner.src.len and scanner.atWhitespace()) : (scanner.cursor += 1) {}
        if (scanner.cursor == scanner.src.len) {
            return false;
        }
        if (scanner.at() == '(' or scanner.at() == ')') {
            scanner.token = scanner.src[scanner.cursor .. scanner.cursor + 1];
            scanner.cursor += 1;
            return true;
        } else {
            const start = scanner.cursor;
            while (scanner.cursor < scanner.src.len and !scanner.atWhitespace() and scanner.at() != '(' and scanner.at() != ')') : (scanner.cursor += 1) {}
            scanner.token = scanner.src[start..scanner.cursor];
            return true;
        }
        return false;
    }

    fn remaining(scanner: *Scanner) []const u8 {
        return scanner.src[scanner.cursor..];
    }

    fn consume(scanner: *Scanner, token: []const u8) !void {
        if (!std.mem.eql(u8, token, scanner.token)) {
            return error.FailedConsume;
        }
        _ = scanner.scan();
    }
};

pub const Parser = struct {
    const nil = Value{ .nil = {} };
    const car = Value.car;
    const cdr = Value.cdr;

    alloc: std.mem.Allocator,
    conses: Pool([2]Value),
    interned: std.StringHashMap(u32),
    intern_data: std.ArrayList([]const u8),

    scanner: Scanner,

    pub fn init(alloc: std.mem.Allocator, src: []const u8) Parser {
        return .{
            .alloc = alloc,
            .conses = Pool([2]Value).init(alloc),
            .interned = std.StringHashMap(u32).init(alloc),
            .intern_data = std.ArrayList([]const u8).init(alloc),
            .scanner = Scanner{ .src = src },
        };
    }

    pub fn deinit(parser: *Parser) void {
        parser.intern_data.deinit();
        parser.interned.deinit();
        parser.conses.deinit();
        parser.* = undefined;
    }

    // each call to parse reads one s-expression
    pub fn parse(parser: *Parser) ?Value {
        while (parser.scanner.scan()) {
            if (parser.scanner.is("(")) {
                parser.scanner.consume("(") catch @panic("expect (");
                return parser.parselist();
            } else {
                return Value{ .atom = parser.interned.get(parser.scanner.token) orelse blk: {
                    std.debug.assert(parser.interned.count() == parser.intern_data.items.len);
                    const i = parser.interned.count();
                    parser.interned.put(parser.scanner.token, i) catch @panic("out of memory");
                    parser.intern_data.append(parser.scanner.token) catch @panic("out of memory");
                    break :blk i;
                } };
            }
        }

        return null;
    }

    fn parselist(parser: *Parser) Value {
        // parse a list
        if (parser.scanner.is(")")) {
            return nil;
        } else if (parser.scanner.is(".")) {
            const x = parser.parse() orelse @panic("unexpected end of list");
            parser.scanner.consume(")") catch @panic("expect )");
            return x;
        } else {
            const x = parser.parse() orelse @panic("unexpected end of list");
            return parser.cons(x, parser.parselist());
        }
    }

    pub fn print(parser: Parser, x: Value) void {
        switch (x) {
            .atom => |atom| std.debug.print("atom:{s}", .{parser.intern_data.items[atom]}),
            .cons => parser.printlist(x),
            .nil => std.debug.print("nil", .{}),
        }
    }

    fn printlist(parser: Parser, _x: Value) void {
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

    fn cons(parser: *Parser, a: Value, d: Value) Value {
        var pair = parser.conses.create() catch @panic("out of memory");
        pair[0] = a;
        pair[1] = d;
        return Value{ .cons = pair.ptr };
    }

    // fn peek
};

test "basic parsing" {
    std.debug.print("\n", .{});

    var parser = Parser.init(
        std.testing.allocator,
        "(+ 1 2)",
    );
    defer parser.deinit();

    while (parser.parse()) |x| {
        parser.print(x);
        std.debug.print("\n", .{});
    }
}
