const std = @import("std");
const BlockPool = @import("blockpool.zig").BlockPool(.{});

const GC = @import("gc7.zig").GC;
const Kind = @import("gc7.zig").Kind;
const Object = @import("gc7.zig").Object;

pub const RT = struct {
    gc: GC,

    pub fn init(pool: *BlockPool) !RT {
        return .{ .gc = try GC.init(pool) };
    }

    pub fn deinit(rt: *RT) void {
        rt.gc.deinit();
        rt.* = undefined;
    }

    pub fn newReal(rt: *RT, value: f64) *Object {
        const obj = rt.gc.alloc(.real, 0) catch @panic("GC allocation failure");
        obj.data = value;
        return rt.gc.commit(.real, obj);
    }

    pub fn newCons(rt: *RT, car: ?*Object, cdr: ?*Object) *Object {
        const obj = rt.gc.alloc(.cons, 0) catch @panic("GC allocation failure");
        obj.car = car;
        obj.cdr = cdr;
        return rt.gc.commit(.cons, obj);
    }

    pub fn newHamt(rt: *RT) *Object {
        const obj = rt.gc.alloc(.hamt, 0) catch @panic("GC allocation failure");
        obj.mask = 0;
        return rt.gc.commit(.hamt, obj);
    }

    pub fn newString(rt: *RT, value: []const u8) *Object {
        const obj = rt.gc.alloc(.string, value.len) catch @panic("GC allocation failure");
        obj.len = value.len;
        @memcpy(obj.data(), value);
        return rt.gc.commit(.string, obj);
    }
};

pub fn print(obj: ?*Object, writer: anytype) !void {
    try _printImpl(obj, writer);
    try writer.print("\n", .{});
}

fn _printImpl(_obj: ?*Object, writer: anytype) !void {
    const obj = _obj orelse return writer.print("nil", .{});
    switch (obj.getKind()) {
        .real => try writer.print("{d}", .{obj.as(.real).data}),
        .cons => {
            const cons = obj.as(.cons);
            // TODO list printing
            try writer.print("(", .{});
            try _printImpl(cons.car, writer);
            try writer.print(" . ", .{});
            try _printImpl(cons.cdr, writer);
            try writer.print(")", .{});
        },
        .hamt => {
            try writer.print("{{", .{});
            try _printHamt(obj, writer);
            try writer.print("}}", .{});
        },
        .string => {
            const str = obj.as(.string);
            try writer.print("\"{s}\"", .{str.data()[0..str.len]});
        },
    }
}

fn _printHamt(obj: *Object, writer: anytype) !void {
    std.debug.assert(obj.getKind() == .hamt);
    const hamt = obj.as(.hamt);
    const children = hamt.data();
    for (children[0..hamt.len()]) |child| {
        switch (child.getKind()) {
            .cons => {},
            .hamt => {},
            else => unreachable,
        }
    }
    _ = writer;
}

pub fn main() !void {
    try scratch();
}

fn scratch() !void {
    const stdout = std.io.getStdOut().writer();

    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();
    var rt = try RT.init(&pool);
    defer rt.deinit();

    const a = rt.newReal(1.23);
    const b = rt.newString("hello world");
    const c = rt.newCons(a, b);
    const d = rt.newHamt();

    try print(null, stdout);
    try print(a, stdout);
    try print(b, stdout);
    try print(c, stdout);
    try print(d, stdout);
}
