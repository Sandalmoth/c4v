const std = @import("std");

// roughly based on https://github.com/Robert-van-Engelen/tinylisp

const Config = struct {
    stack_size: u32 = 1024,
};

const ValueType = enum { number, atom, primitive, cons, closure, nil };
const Value = union(ValueType) {
    number: f64,
    atom: u32, // heap offset to first byte (interned, null terminated)
    primitive: u32, // index into array of primitive functions
    cons: u32, // stack offset
    closure: u32, // stack offset
    nil: void,
};

const Interpreter = struct {
    const Primitive = struct {
        name: []const u8,
        function: *const (fn (*Interpreter, Value, Value) Value),
    };
    const primitives: [18]Primitive = .{
        .{ .name = "eval", .function = &f_eval },
        .{ .name = "quote", .function = &f_quote },
        .{ .name = "cons", .function = &f_cons },
        .{ .name = "car", .function = &f_car },
        .{ .name = "cdr", .function = &f_cdr },
        .{ .name = "+", .function = &f_add },
        .{ .name = "-", .function = &f_sub },
        .{ .name = "*", .function = &f_mul },
        .{ .name = "/", .function = &f_div },
        .{ .name = "int", .function = &f_int },
        .{ .name = "<", .function = &f_lt },
        .{ .name = "=", .function = &f_eq },
        .{ .name = "not", .function = &f_not },
        .{ .name = "or", .function = &f_or },
        .{ .name = "and", .function = &f_and },
        .{ .name = "cond", .function = &f_cond },
        .{ .name = "if", .function = &f_if },
        .{ .name = "define", .function = &f_define },
    };

    alloc: std.mem.Allocator,

    sp: u32,
    hp: u32,
    stack: []Value,
    heap: [*:0]u8, // actually just &stack[0], stores null terminated strings

    nil: Value,
    tru: Value,
    err: Value,
    env: Value,

    pub fn init(alloc: std.mem.Allocator, config: Config) !Interpreter {
        var ipt = Interpreter{
            .alloc = alloc,
            .sp = config.stack_size,
            .hp = 0,
            .stack = try alloc.alloc(Value, config.stack_size),
            .heap = undefined,
            .nil = undefined,
            .tru = undefined,
            .err = undefined,
            .env = undefined,
        };
        ipt.heap = @ptrCast(ipt.stack.ptr);

        ipt.nil = Value{ .nil = {} };
        ipt.tru = ipt.atom("#t");
        ipt.err = ipt.atom("ERR");
        ipt.env = ipt.cons(ipt.cons(ipt.tru, ipt.tru), ipt.nil);

        for (primitives, 0..) |p, i| {
            ipt.env = ipt.cons(ipt.cons(
                ipt.atom(p.name),
                Value{ .primitive = @intCast(i) },
            ), ipt.env);
        }

        return ipt;
    }

    pub fn deinit(ipt: *Interpreter) void {
        ipt.alloc.free(ipt.stack);
        ipt.* = undefined;
    }

    pub fn read(ipt: *Interpreter, src: []const u8) Value {
        var ctx: ScannerContext = .{ .src = src };
        scan(&ctx);
        return ipt.parse(&ctx);
    }

    // pub fn eval(ipt: *Interpreter, x: Value, env: Value) Value {
    //     return switch (x) {
    //         .atom => ipt.assoc(x, env),
    //         .cons => ipt.apply(ipt.eval(ipt.car(x), env), ipt.cdr(x), env),
    //         else => x,
    //     };
    // }

    pub fn step(ipt: *Interpreter, x: Value, env: Value) Value {
        return switch (x) {
            .atom => ipt.assoc(x, env),
            .cons => ipt.apply(ipt.eval(ipt.car(x), env), ipt.cdr(x), env),
            else => x,
        };
    }

    pub fn eval(ipt: *Interpreter, x: Value, env: Value) Value {
        const y = ipt.step(x, env);
        std.debug.print("{}:\t", .{ipt.sp});
        ipt.print(x);
        std.debug.print(" => ", .{});
        ipt.print(y);
        std.debug.print("\n", .{});
        return y;
    }

    pub fn print(ipt: *Interpreter, val: Value) void {
        switch (val) {
            .nil => std.debug.print("()", .{}),
            .atom => |offset| std.debug.print("{s}", .{ipt.heapStr(offset)}),
            .primitive => |p| std.debug.print("<{s}>", .{primitives[p].name}),
            .cons => ipt.printlist(val),
            .number => |n| std.debug.print("{}", .{n}),
            else => @panic("unprintable"),
        }
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
            if (ipt.hp > ipt.sp * @sizeOf(Value)) {
                @panic("interpreter out of memory");
            }
        }

        return Value{ .atom = i };
    }

    fn cons(ipt: *Interpreter, a: Value, d: Value) Value {
        ipt.stack[ipt.sp - 1] = a;
        ipt.stack[ipt.sp - 2] = d;
        ipt.sp -= 2;
        if (ipt.hp > ipt.sp * @sizeOf(Value)) {
            @panic("interpreter out of memory");
        }
        return .{ .cons = ipt.sp };
    }

    fn car(ipt: *Interpreter, c: Value) Value {
        return switch (c) {
            .cons, .closure => |a| ipt.stack[a + 1],
            else => ipt.err,
        };
    }

    fn cdr(ipt: *Interpreter, c: Value) Value {
        return switch (c) {
            .cons, .closure => |d| ipt.stack[d],
            else => ipt.err,
        };
    }

    fn assoc(ipt: *Interpreter, x: Value, _env: Value) Value {
        // so the definitions in an environment are a series of pairs like
        // ((name definition) ((name definition) (...)))
        // so we walk down the cdr, checking the car each time for a match
        var env = _env;
        while (env == .cons and !std.meta.eql(x, ipt.car(ipt.car(env)))) {
            env = ipt.cdr(env);
        }
        if (env == .cons) {
            // we found something before the end of the environment
            return ipt.cdr(ipt.car(env));
        }
        std.debug.print("{} not in environment {}\n", .{ x, _env });
        return ipt.err;
    }

    fn reduce(ipt: *Interpreter, clos: Value, x: Value, env: Value) Value {
        _ = ipt;
        _ = clos;
        _ = x;
        _ = env;
        @panic("closures are not implemented");
    }

    fn apply(ipt: *Interpreter, clos: Value, x: Value, env: Value) Value {
        return switch (clos) {
            .primitive => |p| primitives[p].function(ipt, x, env),
            .closure => ipt.reduce(clos, x, env),
            else => ipt.err,
        };
    }

    fn evlis(ipt: *Interpreter, x: Value, env: Value) Value {
        // there's an inefficenty here with quote
        // as with quote, we shouldn't try to evalate the quoted term
        // but as is, we do and then ignore it
        // i think this could cause bugs too if it has side effects?
        return switch (x) {
            .cons => ipt.cons(
                ipt.eval(ipt.car(x), env),
                ipt.evlis(ipt.cdr(x), env),
            ),
            .atom => ipt.assoc(x, env),
            else => ipt.nil,
        };
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
            return ipt.nil;
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
            return ipt.cons(
                ipt.atom("quote"),
                ipt.cons(ipt.read(ctx.src[ctx.cursor..]), ipt.nil),
            );
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

    fn f_eval(ipt: *Interpreter, x: Value, env: Value) Value {
        return ipt.eval(ipt.car(ipt.evlis(x, env)), env);
    }

    fn f_quote(ipt: *Interpreter, x: Value, env: Value) Value {
        _ = env;
        return ipt.car(x);
    }

    fn f_cons(ipt: *Interpreter, x: Value, env: Value) Value {
        const y = ipt.evlis(x, env);
        return ipt.cons(ipt.car(y), ipt.car(ipt.cdr(y)));
    }

    fn f_car(ipt: *Interpreter, x: Value, env: Value) Value {
        return ipt.car(ipt.car(ipt.evlis(x, env)));
    }

    fn f_cdr(ipt: *Interpreter, x: Value, env: Value) Value {
        return ipt.cdr(ipt.car(ipt.evlis(x, env)));
    }

    fn f_add(ipt: *Interpreter, x: Value, env: Value) Value {
        var y = ipt.evlis(x, env);
        var n = ipt.car(y);
        if (n != .number) {
            return ipt.err;
        }
        while (true) {
            y = ipt.cdr(y);
            if (y == .nil) {
                break;
            }
            const n2 = ipt.car(y);
            if (n2 != .number) {
                return ipt.err;
            }
            n.number += n2.number;
        }
        return n;
    }

    fn f_sub(ipt: *Interpreter, x: Value, env: Value) Value {
        var y = ipt.evlis(x, env);
        var n = ipt.car(y);
        if (n != .number) {
            return ipt.err;
        }
        while (true) {
            y = ipt.cdr(y);
            if (y == .nil) {
                break;
            }
            const n2 = ipt.car(y);
            if (n2 != .number) {
                return ipt.err;
            }
            n.number -= n2.number;
        }
        return n;
    }

    fn f_mul(ipt: *Interpreter, x: Value, env: Value) Value {
        var y = ipt.evlis(x, env);
        var n = ipt.car(y);
        if (n != .number) {
            return ipt.err;
        }
        while (true) {
            y = ipt.cdr(y);
            if (y == .nil) {
                break;
            }
            const n2 = ipt.car(y);
            if (n2 != .number) {
                return ipt.err;
            }
            n.number *= n2.number;
        }
        return n;
    }

    fn f_div(ipt: *Interpreter, x: Value, env: Value) Value {
        var y = ipt.evlis(x, env);
        var n = ipt.car(y);
        if (n != .number) {
            return ipt.err;
        }
        while (true) {
            y = ipt.cdr(y);
            if (y == .nil) {
                break;
            }
            const n2 = ipt.car(y);
            if (n2 != .number) {
                return ipt.err;
            }
            n.number /= n2.number;
        }
        return n;
    }

    fn f_int(ipt: *Interpreter, x: Value, env: Value) Value {
        const y = ipt.car(ipt.evlis(x, env));
        if (y != .number) {
            return ipt.err;
        }
        const n: i64 = @intFromFloat(y.number);
        return Value{ .number = @floatFromInt(n) };
    }

    fn f_lt(ipt: *Interpreter, x: Value, env: Value) Value {
        const y = ipt.evlis(x, env);
        const n = ipt.car(y);
        const n2 = ipt.car(ipt.cdr(y));
        if (n != .number or n2 != .number) {
            return ipt.err;
        }
        return if (n.number - n2.number < 0) ipt.tru else ipt.nil;
    }

    fn f_eq(ipt: *Interpreter, x: Value, env: Value) Value {
        const y = ipt.evlis(x, env);
        return if (std.meta.eql(ipt.car(y), ipt.car(ipt.cdr(y)))) ipt.tru else ipt.nil;
    }

    fn f_not(ipt: *Interpreter, x: Value, env: Value) Value {
        const y = ipt.evlis(x, env);
        return if (ipt.car(y) == .nil) ipt.tru else ipt.nil;
    }

    fn f_or(ipt: *Interpreter, _x: Value, env: Value) Value {
        var x = _x;
        var y = ipt.nil;
        while (x != .nil) {
            y = ipt.eval(ipt.car(x), env);
            if (y != .nil) {
                break;
            }
            x = ipt.cdr(x);
        }
        return y;
    }

    fn f_and(ipt: *Interpreter, _x: Value, env: Value) Value {
        var x = _x;
        var y = ipt.nil;
        while (x != .nil) {
            y = ipt.eval(ipt.car(x), env);
            if (y == .nil) {
                break;
            }
            x = ipt.cdr(x);
        }
        return y;
    }

    fn f_cond(ipt: *Interpreter, _x: Value, env: Value) Value {
        var x = _x;
        // so we step through a list
        // and evaluate an expression each step
        // and when we find a true
        // we break and evaluate the cdr of the cons that had that expression
        while (x != .nil) {
            if (ipt.eval(ipt.car(ipt.car(x)), env) != .nil) {
                break;
            }
            x = ipt.cdr(x);
        }
        return ipt.eval(ipt.car(ipt.cdr(ipt.car(x))), env);
    }

    fn f_if(ipt: *Interpreter, x: Value, env: Value) Value {
        return if (ipt.eval(ipt.car(x), env) != .nil)
            ipt.eval(ipt.car(ipt.cdr(ipt.cdr(x))), env)
        else
            ipt.eval(ipt.car(ipt.cdr(x)), env);
    }

    // fn f_(ipt: *Interpreter, x: Value, env: Value) Value {}
    // fn f_(ipt: *Interpreter, x: Value, env: Value) Value {}

    fn f_define(ipt: *Interpreter, x: Value, env: Value) Value {
        ipt.env = ipt.cons(ipt.cons(
            ipt.car(x),
            ipt.eval(ipt.car(ipt.cdr(x)), env),
        ), ipt.env);
        return ipt.car(x);
    }
};

test "read" {
    std.debug.print("\n", .{});

    var ipt = try Interpreter.init(std.testing.allocator, .{});
    defer ipt.deinit();

    ipt.print(ipt.eval(ipt.read("(+ 1 2 3)"), ipt.env));
    std.debug.print("\n", .{});

    ipt.print(ipt.eval(ipt.read("(< 3 (/ 4 2))"), ipt.env));
    std.debug.print("\n", .{});

    ipt.print(ipt.eval(ipt.read("(not (= 1 2))"), ipt.env));
    std.debug.print("\n", .{});

    ipt.print(ipt.eval(ipt.read("(if (< 3 2) #t (+ 2 2))"), ipt.env));
    std.debug.print("\n", .{});

    ipt.print(ipt.eval(ipt.read("(cond ((< 3 2) 1) (() 2) ((< 2 3) 3) (#t 4))"), ipt.env));
    std.debug.print("\n", .{});

    ipt.print(ipt.eval(ipt.read("(define my-var 3)"), ipt.env));
    std.debug.print("\n", .{});
    ipt.print(ipt.eval(ipt.read("(+ my-var 2)"), ipt.env));
    std.debug.print("\n", .{});
}
