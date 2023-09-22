const std = @import("std");

// roughly based on https://github.com/Robert-van-Engelen/tinylisp

const Config = struct {
    stack_size: u32 = 1024,
};

const ValueType = enum { number, atom, string, primitive, cons, closure, false, nil };
const Value = union(ValueType) {
    number: f64,
    atom: u32, // heap offset to first byte (interned, null terminated)
    string: u32, // heap offset to first byte (null terminated) -- do we need strings? how should they work?
    primitive: u32, // index into array of primitive functions
    cons: u32, // stack offset
    closure: u32, // stack offset
    false: void, // if we want a separate false-type (like scheme), this is probably where to put it
    nil: void,
};

const Interpreter = struct {
    const nil = Value{ .nil = {} };

    alloc: std.mem.Allocator,

    sp: u32,
    hp: u32,
    stack: []Value,
    heap: [*:0]u8, // actually just &stack[0], stores null terminated strings

    pub fn init(alloc: std.mem.Allocator, config: Config) !Interpreter {
        var ipt = Interpreter{
            .alloc = alloc,
            .sp = config.stack_size,
            .hp = 0,
            .stack = try alloc.alloc(Value, config.stack_size),
            .heap = undefined,
        };
        ipt.heap = @ptrCast(ipt.stack.ptr);
        return ipt;
    }

    pub fn deinit(ipt: *Interpreter) void {
        ipt.alloc.free(ipt.stack);
        ipt.* = undefined;
    }

    inline fn heapStr(ipt: *Interpreter, offset: u32) []u8 {
        return std.mem.span(ipt.heap + offset);
    }
    inline fn heapStrlen(ipt: *Interpreter, offset: u32) u32 {
        return @intCast(std.mem.len(ipt.heap + offset));
    }

    fn atom(ipt: *Interpreter, token: []const u8) Value {
        // see if we have already interned the string
        var i: u32 = 0;
        while (i < ipt.hp and
            !std.mem.eql(u8, token, ipt.heapStr(i))) : (i += ipt.heapStrlen(i) + 1)
        {}

        // if we didn't find it (we checked the whole heap)
        // then copy it to the heap
        if (i == ipt.hp) {
            @memcpy(ipt.heap + ipt.hp, token);
            ipt.hp += @intCast(token.len);
            ipt.heap[ipt.hp] = 0; // null terminator
            ipt.hp += 1;
            if (ipt.hp > ipt.sp / @sizeOf(Value)) {
                @panic("interpreter out of memory");
            }
        }

        return Value{ .atom = i };
    }

    fn cons(ipt: *Interpreter, a: Value, d: Value) Value {
        ipt.stack[ipt.sp - 1] = a;
        ipt.stack[ipt.sp - 2] = d;
        ipt.sp -= 2;
        if (ipt.hp > ipt.sp / @sizeOf(Value)) {
            @panic("interpreter out of memory");
        }
        return .{ .cons = ipt.sp };
    }

    fn car(ipt: *Interpreter, c: Value) Value {
        switch (c) {
            .cons, .closure => |a| return ipt.stack[a + 1],
            else => @panic("car must be called on a cons or a closure"),
        }
    }

    fn cdr(ipt: *Interpreter, c: Value) Value {
        switch (c) {
            .cons, .closure => |d| return ipt.stack[d],
            else => @panic("car must be called on a cons or a closure"),
        }
    }

    const ScannerContext = struct {
        src: []const u8,
        cursor: u32 = 0,
        token: []const u8 = &.{},

        fn isWhitespace(ctx: ScannerContext) bool {
            std.debug.assert(ctx.cursor < ctx.src.len);
            return switch (ctx.src[ctx.cursor]) {
                ' ', '\t', '\n', '\r' => true,
                else => false,
            };
        }

        fn at(ctx: ScannerContext) u8 {
            return ctx.src[ctx.cursor];
        }
    };

    /// reads the next token into the ctx.token buffer
    fn scan(ctx: *ScannerContext) void {
        while (ctx.isWhitespace()) : (ctx.cursor += 1) {}
        if (ctx.at() == '(' or ctx.at() == ')' or ctx.at() == '\'') {
            ctx.token = ctx.src[ctx.cursor .. ctx.cursor + 1];
            ctx.cursor += 1;
        } else {
            const start = ctx.cursor;
            while (!ctx.isWhitespace() and ctx.at() != '(' and ctx.at() != ')') : (ctx.cursor += 1) {}
            ctx.token = ctx.src[start..ctx.cursor];
        }
    }

    fn list(ipt: *Interpreter, ctx: *ScannerContext) Value {
        scan(ctx);
        if (ctx.token[0] == ')') {
            return nil;
        }

        if (ctx.token[0] == '.') {
            const pair = ipt.read(ctx.src[ctx.cursor..]);
            scan(ctx);
            return pair;
        } else {
            const val = ipt.parse(ctx);
            return ipt.cons(val, ipt.list(ctx));
        }

        unreachable;
    }

    fn parse(ipt: *Interpreter, ctx: *ScannerContext) Value {
        if (ctx.token[0] == '(') {
            return ipt.list(ctx);
        } else if (ctx.token[0] == '\'') {
            return ipt.cons(ipt.atom("quote"), ipt.cons(ipt.read(ctx.src[ctx.cursor..]), nil));
        } else {
            // TODO strings
            const number = std.fmt.parseFloat(f64, ctx.token) catch {
                // not a number
                return ipt.atom(ctx.token);
            };
            return .{ .number = number };
        }

        unreachable;
    }

    pub fn read(ipt: *Interpreter, src: []const u8) Value {
        var ctx: ScannerContext = .{ .src = src };
        scan(&ctx);
        return ipt.parse(&ctx);
    }

    pub fn print(ipt: *Interpreter, val: Value) void {
        switch (val) {
            .nil => std.debug.print("()", .{}),
            .atom => |offset| std.debug.print("{s}", .{ipt.heapStr(offset)}),
            .cons => ipt.printlist(val),
            .number => |n| std.debug.print("{}", .{n}),
            else => @panic("unprintable"),
        }
    }

    fn printlist(ipt: *Interpreter, _l: Value) void {
        // we know that l is a cons
        var l = _l;
        std.debug.print("(", .{});
        while (true) : (std.debug.print(" ", .{})) {
            ipt.print(ipt.car(l));
            l = ipt.cdr(l);
            if (l == .nil) {
                break;
            }
            if (l != .cons) {
                std.debug.print(" . ", .{});
                ipt.print(l);
                break;
            }
        }
        std.debug.print(")", .{});
    }
};

test "read" {
    std.debug.print("\n", .{});

    var ipt = try Interpreter.init(std.testing.allocator, .{});
    defer ipt.deinit();

    ipt.print(ipt.read("(+ 1 2)"));
    std.debug.print("\n", .{});

    ipt.print(ipt.read("(3 . 4)"));
    std.debug.print("\n", .{});

    ipt.print(ipt.read("(lambda (x) (* x x))"));
    std.debug.print("\n", .{});

    // _ = ipt.read("(+ 1 2)");
    // std.debug.print("{}\n", .{ipt.atom("howdy")});
    // std.debug.print("{s}\n", .{ipt.heap[0..64]});
    // std.debug.print("{}\n", .{ipt.atom("howdy")});
    // std.debug.print("{s}\n", .{ipt.heap[0..64]});
    // std.debug.print("{}\n", .{ipt.atom("everybody")});
    // std.debug.print("{s}\n", .{ipt.heap[0..64]});
    // std.debug.print("{}\n", .{ipt.atom("yo")});
    // std.debug.print("{s}\n", .{ipt.heap[0..64]});
    // std.debug.print("{}\n", .{ipt.atom("everybody")});
    // std.debug.print("{s}\n", .{ipt.heap[0..64]});
    // std.debug.print("{}\n", .{ipt.atom("howdy")});
    // std.debug.print("{s}\n", .{ipt.heap[0..64]});
    // std.debug.print("{}\n", .{ipt.atom("yo")});
    // std.debug.print("{s}\n", .{ipt.heap[0..64]});

    // for (ipt.heap[0..32]) |c| {
    //     std.debug.print("{} ", .{c});
    // }
    // std.debug.print("\n", .{});
}
