const std = @import("std");
const BlockPool = @import("blockpool.zig").BlockPool(.{});

const GC = @import("gc7.zig").GC;
const Kind = @import("gc7.zig").Kind;
const Object = @import("gc7.zig").Object;
const NILHASH = @import("gc7.zig").NILHASH;

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

    pub fn hamtAssoc(rt: *RT, objhamt: ?*Object, objkey: ?*Object, objval: ?*Object) *Object {
        // inserting into nil just creates a new map
        const obj = objhamt orelse rt.newHamt();
        return rt._hamtAssocImpl(obj, objkey, objval, 0);
    }
    fn _hamtAssocImpl(
        rt: *RT,
        objhamt: *Object,
        objkey: ?*Object,
        objval: ?*Object,
        depth: usize,
    ) *Object {
        const hamt = objhamt.as(.hamt);
        const slot = if (objkey) |k| k.getHashAtDepth(depth) else NILHASH & 0b11_1111;
        const packed_index = @popCount(hamt.mask & ((@as(u64, 1) << @intCast(slot)) - 1));
        if (hamt.mask & (@as(u64, 1) << @intCast(slot)) == 0) {
            // empty slot, insert here
            const new = rt.gc.alloc(.hamt, hamt.len() + 1) catch @panic("GC allocation failure");
            new.mask = hamt.mask | (@as(u64, 1) << @intCast(slot));
            // shuffle the entries in the old hamt to make space
            @memcpy(new.data(), hamt.data()[0..packed_index]);
            new.data()[packed_index] = rt.newCons(objkey, objval);
            @memcpy(new.data()[packed_index + 1 ..], hamt.data()[packed_index..hamt.len()]);
            return rt.gc.commit(.hamt, new);
        }

        // occupied slot, either insert, traverse, or create a new subtree and traverse
        const child = hamt.data()[packed_index];
        const new = rt.gc.alloc(.hamt, hamt.len()) catch @panic("GC allocation failure");
        new.mask = hamt.mask;
        @memcpy(new.data(), hamt.data()[0..hamt.len()]);
        switch (child.getKind()) {
            .cons => {
                const cons = child.as(.cons);
                if (eql(cons.car, objkey)) {
                    // replace key in this slot
                    new.data()[packed_index] = rt.newCons(objkey, objval);
                } else {
                    // generate new subtree and traverse
                    // TODO? try fastpath if cons.car and objkey doesn't collide at depth + 1
                    const subslot = if (cons.car) |car|
                        car.getHashAtDepth(depth + 1)
                    else
                        NILHASH & 0b11_1111;
                    const sub = rt.gc.alloc(.hamt, 1) catch @panic("GC allocation failure");
                    sub.mask = @as(u64, 1) << @intCast(subslot);
                    sub.data()[0] = child;
                    new.data()[packed_index] = rt._hamtAssocImpl(
                        rt.gc.commit(.hamt, sub),
                        objkey,
                        objval,
                        depth + 1,
                    );
                }
            },
            .hamt => {
                new.data()[packed_index] = rt._hamtAssocImpl(child, objkey, objval, depth + 1);
            },
            else => unreachable,
        }
        return rt.gc.commit(.hamt, new);
    }

    pub fn hamtGet(rt: *RT, objhamt: ?*Object, objkey: ?*Object) ?*Object {
        const obj = objhamt orelse return null;
        return rt._hamtGetImpl(obj, objkey, 0);
    }
    pub fn _hamtGetImpl(rt: *RT, objhamt: *Object, objkey: ?*Object, depth: usize) ?*Object {
        const hamt = objhamt.as(.hamt);
        const slot = if (objkey) |k| k.getHashAtDepth(depth) else NILHASH & 0b11_1111;
        if (hamt.mask & (@as(u64, 1) << @intCast(slot)) == 0) return null;
        const packed_index = @popCount(hamt.mask & ((@as(u64, 1) << @intCast(slot)) - 1));
        const child = hamt.data()[packed_index];
        return switch (child.getKind()) {
            .cons => blk: {
                const cons = child.as(.cons);
                break :blk if (eql(cons.car, objkey)) cons.cdr else null;
            },
            .hamt => rt._hamtGetImpl(child, objkey, depth + 1),
            else => unreachable,
        };
    }

    pub fn hamtContains(rt: *RT, objhamt: ?*Object, objkey: ?*Object) bool {
        const obj = objhamt orelse return false;
        return rt._hamtGetImpl(obj, objkey, 0) != null;
    }

    pub fn hamtDissoc(rt: *RT, objhamt: ?*Object, objkey: ?*Object) ?*Object {
        const obj = objhamt orelse return null;
        const result = rt._hamtDissocImpl(obj, objkey, 0);
        if (result == null) return rt.newHamt();
        if (result.?.getKind() == .cons) {
            const cons = result.?.as(.cons);
            const slot = if (cons.car) |k| k.getHashAtDepth(0) else NILHASH & 0b11_1111;
            const new = rt.gc.alloc(.hamt, 1) catch @panic("GC allocation failure");
            new.mask = (@as(u64, 1) << @intCast(slot));
            new.data()[0] = result.?;
            return rt.gc.commit(.hamt, new);
        }
        return result.?;
    }
    pub fn _hamtDissocImpl(rt: *RT, objhamt: *Object, objkey: ?*Object, depth: usize) ?*Object {
        const hamt = objhamt.as(.hamt);
        const slot = if (objkey) |k| k.getHashAtDepth(depth) else NILHASH & 0b11_1111;
        // if child slot for key is not occupied, return with no changes
        if (hamt.mask & (@as(u64, 1) << @intCast(slot)) == 0) return objhamt;
        const packed_index = @popCount(hamt.mask & ((@as(u64, 1) << @intCast(slot)) - 1));
        const child = hamt.data()[packed_index];

        switch (child.getKind()) {
            .cons => {
                const cons = child.as(.cons);
                if (!eql(cons.car, objkey)) return objhamt; // key is not present
                // key is present, actually delete something
                // handle cases where this hamt level isn't needed after the delete
                if (hamt.len() == 1) return null;
                if (hamt.len() == 2) return hamt.data()[if (packed_index == 0) 1 else 0];
                // delete and preserve level
                const new = rt.gc.alloc(.hamt, hamt.len() - 1) catch
                    @panic("GC allocation failure");
                new.mask = hamt.mask & ~(@as(u64, 1) << @intCast(slot));
                @memcpy(new.data(), hamt.data()[0..packed_index]);
                @memcpy(new.data()[packed_index..], hamt.data()[packed_index + 1 .. hamt.len()]);
                return rt.gc.commit(.hamt, new);
            },
            .hamt => {
                // recur and if a deletion happened at the sublevel, update to include it
                const result = rt._hamtDissocImpl(child, objkey, depth + 1);
                if (eql(child, result)) return objhamt;
                if (result == null) {
                    // if the sublevel deletion resulted in a node deletion
                    // we need to downsize (and possibly delete) ourselves as well
                    if (hamt.len() == 1) return null;
                    if (hamt.len() == 2) return hamt.data()[if (packed_index == 0) 1 else 0];
                    const new = rt.gc.alloc(.hamt, hamt.len()) catch
                        @panic("GC allocation failure");
                    new.mask = hamt.mask & ~(@as(u64, 1) << @intCast(slot));
                    @memcpy(new.data(), hamt.data()[0..packed_index]);
                    @memcpy(new.data()[packed_index..], hamt.data()[packed_index + 1 .. hamt.len()]);
                    return rt.gc.commit(.hamt, new);
                }
                const new = rt.gc.alloc(.hamt, hamt.len()) catch
                    @panic("GC allocation failure");
                new.mask = hamt.mask;
                @memcpy(new.data(), hamt.data()[0..hamt.len()]);
                new.data()[packed_index] = result.?;
                return rt.gc.commit(.hamt, new);
            },
            else => unreachable,
        }
    }
};

pub fn eql(obj1: ?*Object, obj2: ?*Object) bool {
    if (obj1 == obj2) return true;
    if (obj1 == null or obj2 == null) return false;
    if (obj1.?._property != obj2.?._property) return false;
    // NOTE _property equality implies both kind and hash are equal
    return switch (obj1.?.getKind()) {
        .real => obj1.?.as(.real).data == obj2.?.as(.real).data,
        .cons => blk: {
            const cons1 = obj1.?.as(.cons);
            const cons2 = obj2.?.as(.cons);
            break :blk eql(cons1.car, cons2.car) and eql(cons1.cdr, cons2.cdr);
        },
        .hamt => blk: {
            const hamt1 = obj1.?.as(.hamt);
            const hamt2 = obj2.?.as(.hamt);
            if (hamt1.mask != hamt2.mask) break :blk false;
            for (0..hamt1.len()) |i| {
                if (!eql(hamt1.data()[i], hamt2.data()[i])) break :blk false;
            }
            break :blk true;
        },
        .string => blk: {
            const string1 = obj1.?.as(.string);
            const string2 = obj2.?.as(.string);
            if (string1.len != string2.len) break :blk false;
            break :blk std.mem.eql(
                u8,
                string1.data()[0..string1.len],
                string2.data()[0..string2.len],
            );
        },
    };
}

pub fn print(obj: ?*Object, writer: anytype) anyerror!void {
    try _printImpl(obj, writer);
    try writer.print("\n", .{});
}

fn _printImpl(_obj: ?*Object, writer: anytype) anyerror!void {
    const obj = _obj orelse return writer.print("nil", .{});
    switch (obj.getKind()) {
        .real => try writer.print("{d}", .{obj.as(.real).data}),
        .cons => {
            var cons = obj.as(.cons);
            try writer.print("(", .{});
            while (true) {
                try _printImpl(cons.car, writer);
                if (cons.cdr == null) {
                    break;
                } else if (cons.cdr.?.getKind() != .cons) {
                    try writer.print(" . ", .{});
                    try _printImpl(cons.cdr, writer);
                    break;
                }
                try writer.print(" ", .{});
                cons = cons.cdr.?.as(.cons);
            }
            try writer.print(")", .{});
        },
        .hamt => {
            try writer.print("{{", .{});
            try _printHamt(obj, true, writer);
            try writer.print("}}", .{});
        },
        .string => {
            const str = obj.as(.string);
            try writer.print("\"{s}\"", .{str.data()[0..str.len]});
        },
    }
}

fn _printHamt(obj: *Object, first: bool, writer: anytype) anyerror!void {
    std.debug.assert(obj.getKind() == .hamt);
    const hamt = obj.as(.hamt);
    const children = hamt.data();
    for (0..hamt.len()) |i| {
        const child = children[i];
        switch (child.getKind()) {
            .cons => {
                if (!first or i > 0) try writer.print(", ", .{});
                const cons = child.as(.cons);
                try _printImpl(cons.car, writer);
                try writer.print(" ", .{});
                try _printImpl(cons.cdr, writer);
            },
            .hamt => try _printHamt(child, first and i == 0, writer),
            else => unreachable,
        }
    }
}

pub fn main() !void {
    try scratch();
    try fuzz();
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
    const e = rt.newCons(d, null);
    const f = rt.newCons(c, e);
    const g = rt.hamtAssoc(d, a, b);
    const h = rt.hamtAssoc(g, b, a);
    const i = rt.hamtDissoc(h, a);
    const j = rt.hamtDissoc(i, b);

    try print(null, stdout);
    try print(a, stdout);
    try print(b, stdout);
    try print(c, stdout);
    try print(d, stdout);
    try print(e, stdout);
    try print(f, stdout);
    try print(g, stdout);
    try print(h, stdout);
    try print(i, stdout);
    try print(j, stdout);
}

fn fuzz() !void {
    const stdout = std.io.getStdOut().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072 };
    const m = 10000;

    for (ns) |n| {
        std.debug.print("n = {}\n", .{n});
        var rt = try RT.init(&pool);
        defer rt.deinit();
        var h = rt.newHamt();

        var s = std.AutoHashMap(u32, u32).init(alloc);
        defer s.deinit();

        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));

            try print(h, stdout);
            std.debug.print("key = {} ({})\n", .{ x, rt.hamtContains(h, a) });

            std.debug.assert(rt.hamtContains(h, a) == s.contains(x));
            if (rt.hamtContains(h, a)) {
                std.debug.assert(
                    @as(u32, @intFromFloat(rt.hamtGet(h, a).?.as(.real).data)) == s.get(x).?,
                );
                const h2 = rt.hamtDissoc(h, a);
                h = h2.?;
                _ = s.remove(x);
            } else {
                const h2 = rt.hamtAssoc(h, a, b);
                h = h2;
                try s.put(x, y);
            }
        }
    }
}
