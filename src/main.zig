const std = @import("std");

const _gc = @import("gc.zig");
const Ref = _gc.Ref;
const nil = _gc.nil;

pub fn main() !void {
    // try scratch();
    try benchmark();
    try benchmarkReference();
    // try fuzz();
}

const RT = struct {
    gc: _gc.GC,

    fn init(alloc: std.mem.Allocator) !RT {
        return .{
            .gc = try _gc.GC.init(alloc),
        };
    }

    fn deinit(rt: *RT) void {
        rt.gc.deinit();
        rt.* = undefined;
    }

    fn create(rt: *RT, comptime kind: _gc.Kind, val: _gc.KindType(kind)) Ref {
        return rt.gc.create(kind, val) catch @panic("GC allocation failure");
    }

    // fn dup(rt: *RT, comptime kind: _gc.Kind, ref: *Ref) !Ref {
    //     return rt.create(kind, rt.get(kind, ref));
    // }

    // fn dupUnk(rt: *RT, ref: *Ref) !Ref {
    //     return switch (ref.kind()) {
    //         .real => rt.dup(.real, ref),
    //         .cons => rt.dup(.cons, ref),
    //         .hamt => rt.dup(.hamt, ref),
    //     };
    // }

    fn get(rt: *RT, comptime kind: _gc.Kind, ref: Ref) _gc.KindType(kind) {
        return rt.gc.get(kind, ref);
    }

    fn eql(rt: *RT, aref: Ref, bref: Ref) bool {
        if (std.meta.eql(aref, bref)) return true;
        if (aref.hash() != bref.hash()) return false;
        if (aref.kind() != bref.kind()) return false;
        return switch (aref.kind()) {
            .real => rt.get(.real, aref) == rt.get(.real, bref),
            .cons => blk: {
                const a = rt.get(.cons, aref);
                const b = rt.get(.cons, bref);
                break :blk rt.eql(a[0], b[0]) and rt.eql(a[1], b[1]);
            },
            .hamt => blk: {
                const a = rt.get(.hamt, aref);
                const b = rt.get(.hamt, bref);
                for (a, b) |x, y| if (!rt.eql(x, y)) break :blk false;
                break :blk true;
            },
        };
    }

    fn hamtAssoc(rt: *RT, hamtref: Ref, keyref: Ref, valref: Ref) Ref {
        return rt._hamtAssocImpl(hamtref, keyref, valref, 0);
    }
    fn _hamtAssocImpl(rt: *RT, hamtref: Ref, keyref: Ref, valref: Ref, depth: u32) Ref {
        std.debug.assert(hamtref.kind() == .hamt);
        if (depth >= 6) @panic("max depth"); // TODO switch to an alist at max depth
        var node = rt.get(.hamt, hamtref); // NOTE, this is a by-value copy, not a reference
        const i = (hamtref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (node[i].isNil()) {
            node[i] = rt.create(.cons, .{ keyref, valref });
        } else {
            switch (node[i].kind()) {
                .cons => {
                    const cons = rt.get(.cons, node[i]);
                    if (rt.eql(cons[0], keyref)) {
                        // key is already present, replace val
                        node[i] = rt.create(.cons, .{ keyref, valref });
                    } else {
                        const k = (cons[0].hash() >> @intCast((depth + 1) * 3)) & 0b1111;
                        var contents = [_]Ref{nil} ** 16;
                        contents[k] = node[i];
                        const h = rt.create(.hamt, contents);
                        node[i] = rt._hamtAssocImpl(h, keyref, valref, depth + 1);
                    }
                },
                .hamt => return rt._hamtAssocImpl(node[i], keyref, valref, depth + 1),
                else => unreachable,
            }
        }
        return rt.create(.hamt, node);
    }

    fn hamtContains(rt: *RT, hamtref: Ref, keyref: Ref) bool {
        return rt._hamtContainsImpl(hamtref, keyref, 0);
    }
    fn _hamtContainsImpl(rt: *RT, hamtref: Ref, keyref: Ref, depth: u32) bool {
        std.debug.assert(hamtref.kind() == .hamt);
        if (depth >= 6) @panic("max depth"); // TODO switch to an alist at max depth
        var node = rt.get(.hamt, hamtref);
        const i = (hamtref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (node[i].isNil()) return false;
        return switch (node[i].kind()) {
            .cons => blk: {
                const cons = rt.get(.cons, node[i]);
                break :blk rt.eql(cons[0], keyref);
            },
            .hamt => rt._hamtContainsImpl(hamtref, keyref, depth + 1),
            else => unreachable,
        };
    }

    fn hamtGet(rt: *RT, hamtref: Ref, keyref: Ref) Ref {
        return rt._hamtGetImpl(hamtref, keyref, 0);
    }
    fn _hamtGetImpl(rt: *RT, hamtref: Ref, keyref: Ref, depth: u32) bool {
        std.debug.assert(hamtref.kind() == .hamt);
        if (depth >= 6) @panic("max depth"); // TODO switch to an alist at max depth
        var node = rt.get(.hamt, hamtref);
        const i = (hamtref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (node[i].isNil()) return nil;
        return switch (node[i].kind()) {
            .cons => blk: {
                const cons = rt.get(.cons, node[i]);
                break :blk if (rt.eql(cons[0], keyref)) cons[1] else nil;
            },
            .hamt => rt._hamtGetImpl(hamtref, keyref, depth + 1),
            else => unreachable,
        };
    }

    fn hamtDissoc(rt: *RT, hamtref: Ref, keyref: Ref) Ref {
        return rt._hamtDissocImpl(hamtref, keyref, 0);
    }
    fn _hamtDissocImpl(rt: *RT, hamtref: Ref, keyref: Ref, depth: u32) Ref {
        std.debug.assert(hamtref.kind() == .hamt);
        if (depth >= 6) @panic("max depth"); // TODO switch to an alist at max depth
        var node = rt.get(.hamt, hamtref);
        const i = (hamtref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (node[i].isNil()) return hamtref;
        switch (node[i].kind()) {
            .cons => {
                const cons = rt.get(.cons, node[i]);
                if (!rt.eql(cons[0], keyref)) return hamtref;

                // if we're deleting the last child of this node, delete the whole node
                if (depth > 0) {
                    var n: u32 = 0;
                    for (node) |c| {
                        if (!c.isNil()) n += 1;
                    }
                    std.debug.assert(n > 0);
                    if (n == 1) return nil;
                }

                node[i] = nil;
                return rt.create(.hamt, node);
            },
            .hamt => {
                const result = rt._hamtDissocImpl(node[i], keyref, depth + 1);
                if (rt.eql(node[i], result)) return hamtref;
                node[i] = result;
                return rt.create(.hamt, node);
            },
            else => unreachable,
        }
    }
};

fn benchmark() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var timer = try std.time.Timer.start();

    const ns = [_]u32{ 32, 64, 128, 256, 612, 1024, 2048, 4096, 8192 };
    const m = 30000;
    std.debug.print("cap\tns\tn_ass\tn_diss\n", .{});

    for (ns) |n| {
        var rt = try RT.init(alloc);
        defer rt.deinit();

        timer.reset();
        var h = rt.create(
            .hamt,
            .{ nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil },
        );

        var n_assoc: u32 = 0;
        var n_dissoc: u32 = 0;

        for (0..m) |k| {
            const a = rt.create(.real, @floatFromInt(rand.intRangeLessThan(u32, 0, n)));
            const b = rt.create(.real, @floatFromInt(rand.intRangeLessThan(u32, 0, n)));

            if (rt.hamtContains(h, a)) {
                const h2 = rt.hamtDissoc(h, a);
                h = h2;
                n_dissoc += 1;
            } else {
                const h2 = rt.hamtAssoc(h, a, b);
                h = h2;
                n_assoc += 1;
            }

            if (k % 1000 == 0) {
                rt.gc.trace(h);
                rt.gc.sweep();
                rt.gc.compact();
            }
        }

        std.debug.print("{}\t{}\t{}\t{}\n", .{ n, timer.read() / m, n_assoc, n_dissoc });
    }
}

fn benchmarkReference() !void {
    // obviously, this isn't persistent so not a perfect comparison
    // I just want some idea of how a normal hashmap performs
    // (looks like we're about 5-10x slower with the hamt)
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var timer = try std.time.Timer.start();
    const ns = [_]u32{ 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192 };
    const m = 30000;
    std.debug.print("cap\tns\tn_ass\tn_diss\n", .{});

    for (ns) |n| {
        timer.reset();
        var s = std.AutoHashMap(u64, u64).init(alloc);
        defer s.deinit();
        var n_assoc: u32 = 0;
        var n_dissoc: u32 = 0;
        for (0..m) |_| {
            const a = rand.intRangeLessThan(u64, 0, n);
            const b = rand.intRangeLessThan(u64, 0, n);

            if (s.contains(a)) {
                _ = s.remove(a);
                n_dissoc += 1;
            } else {
                try s.put(a, b);
                n_assoc += 1;
            }
        }
        std.debug.print("{}\t{}\t{}\t{}\n", .{ n, timer.read() / m, n_assoc, n_dissoc });
    }
}
