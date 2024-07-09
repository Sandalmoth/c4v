const std = @import("std");

const _gc = @import("gc.zig");
const Ref = _gc.Ref;
const nil = _gc.nil;

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
        var node = rt.get(.hamt, hamtref); // NOTE, this is a by-value copy, not a reference
        const i = (keyref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (depth < 5) {
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
                            // slot is already occupied, create new level of hamt
                            const k = (cons[0].hash() >> @intCast((depth + 1) * 4)) & 0b1111;
                            var contents = [_]Ref{nil} ** 16;
                            // NOTE since we're creating a next level,
                            // we need to take into consideration that that level could be a leaf
                            // and if so use an alist instead of a direct cons
                            if (depth < 4) {
                                contents[k] = node[i];
                            } else {
                                contents[k] = rt.create(.cons, .{ node[i], nil });
                            }
                            const h = rt.create(.hamt, contents);
                            node[i] = rt._hamtAssocImpl(h, keyref, valref, depth + 1);
                        }
                    },
                    .hamt => node[i] = rt._hamtAssocImpl(node[i], keyref, valref, depth + 1),
                    else => unreachable,
                }
            }
        } else {
            // we've reached the leaf level alist
            if (node[i].isNil()) {
                node[i] = rt.create(.cons, .{ rt.create(.cons, .{ keyref, valref }), nil });
            } else {
                std.debug.assert(node[i].kind() == .cons);
                // first delete our key from the alist (noop-ish if not present)
                // then we can just add it
                const alist = rt._alistDissoc(node[i], keyref);
                node[i] = rt.create(.cons, .{ rt.create(.cons, .{ keyref, valref }), alist });
            }
        }
        return rt.create(.hamt, node);
    }

    fn hamtContains(rt: *RT, hamtref: Ref, keyref: Ref) bool {
        return rt._hamtContainsImpl(hamtref, keyref, 0);
    }
    fn _hamtContainsImpl(rt: *RT, hamtref: Ref, keyref: Ref, depth: u32) bool {
        std.debug.assert(hamtref.kind() == .hamt);
        var node = rt.get(.hamt, hamtref);
        const i = (keyref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (node[i].isNil()) return false;
        if (depth < 5) {
            return switch (node[i].kind()) {
                .cons => blk: {
                    const cons = rt.get(.cons, node[i]);
                    break :blk rt.eql(cons[0], keyref);
                },
                .hamt => rt._hamtContainsImpl(node[i], keyref, depth + 1),
                else => unreachable,
            };
        } else {
            std.debug.assert(node[i].kind() == .cons);
            var walk = rt.get(.cons, node[i]);
            while (true) {
                if (rt.eql(rt.get(.cons, walk[0])[0], keyref)) return true;
                if (walk[1].isNil()) return false;
                walk = rt.get(.cons, walk[1]);
            }
        }
    }

    fn hamtGet(rt: *RT, hamtref: Ref, keyref: Ref) Ref {
        return rt._hamtGetImpl(hamtref, keyref, 0);
    }
    fn _hamtGetImpl(rt: *RT, hamtref: Ref, keyref: Ref, depth: u32) Ref {
        std.debug.assert(hamtref.kind() == .hamt);
        var node = rt.get(.hamt, hamtref);
        const i = (keyref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (node[i].isNil()) return nil;
        if (depth < 5) {
            return switch (node[i].kind()) {
                .cons => blk: {
                    const cons = rt.get(.cons, node[i]);
                    break :blk if (rt.eql(cons[0], keyref)) cons[1] else nil;
                },
                .hamt => rt._hamtGetImpl(node[i], keyref, depth + 1),
                else => unreachable,
            };
        } else {
            std.debug.assert(node[i].kind() == .cons);
            var walk = rt.get(.cons, node[i]);
            while (true) {
                const a = rt.get(.cons, walk[0]);
                if (rt.eql(a[0], keyref)) return a[1];
                if (walk[1].isNil()) return nil;
                walk = rt.get(.cons, walk[1]);
            }
        }
    }

    fn hamtDissoc(rt: *RT, hamtref: Ref, keyref: Ref) Ref {
        return rt._hamtDissocImpl(hamtref, keyref, 0);
    }
    fn _hamtDissocImpl(rt: *RT, hamtref: Ref, keyref: Ref, depth: u32) Ref {
        std.debug.assert(hamtref.kind() == .hamt);
        if (depth >= 6) @panic("max depth"); // TODO switch to an alist at max depth
        var node = rt.get(.hamt, hamtref);
        const i = (keyref.hash() >> @intCast(depth * 4)) & 0b1111;
        if (node[i].isNil()) return hamtref;
        if (depth < 5) {
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
                },
                .hamt => {
                    const result = rt._hamtDissocImpl(node[i], keyref, depth + 1);
                    if (rt.eql(node[i], result)) return hamtref;
                    node[i] = result;
                },
                else => unreachable,
            }
        } else {
            node[i] = rt._alistDissoc(node[i], keyref);
            // if we emptied the alist and this node is otherwise empty, delete the node
            if (depth > 0) {
                var n: u32 = 0;
                for (node) |c| {
                    if (!c.isNil()) n += 1;
                }
                if (n == 0) return nil;
            }
        }
        return rt.create(.hamt, node);
    }
    fn _alistDissoc(rt: *RT, alistref: Ref, keyref: Ref) Ref {
        std.debug.assert(alistref.kind() == .cons);
        // if we have found the key  return the cdr
        // else create a new cons of the car and recur
        // this adds a bit of unneeded gc churn, but hash collisions shouldn't be that common
        const cons = rt.get(.cons, alistref);
        if (rt.eql(rt.get(.cons, cons[0])[0], keyref)) return cons[1];
        if (cons[1].isNil()) return nil;
        return rt.create(.cons, .{ cons[0], rt._alistDissoc(cons[0], keyref) });
    }

    fn debugPrint(rt: *RT, ref: Ref) void {
        if (ref.isNil()) {
            std.debug.print("nil", .{});
        } else {
            rt._debugPrintImpl(ref);
        }
        std.debug.print("\n", .{});
    }
    fn _debugPrintImpl(rt: *RT, ref: Ref) void {
        switch (ref.kind()) {
            .real => std.debug.print("{}", .{rt.get(.real, ref)}),
            .cons => {
                const cons = rt.get(.cons, ref);
                // TODO special case for list printing?
                std.debug.print("(", .{});
                if (cons[0].isNil())
                    std.debug.print("nil", .{})
                else
                    rt._debugPrintImpl(cons[0]);
                std.debug.print(" . ", .{});
                if (cons[1].isNil())
                    std.debug.print("nil", .{})
                else
                    rt._debugPrintImpl(cons[1]);
                std.debug.print(")", .{});
            },
            .hamt => {
                std.debug.print("{{", .{});
                rt._debugPrintHamtImpl(ref);
                std.debug.print("}}", .{});
            },
        }
    }

    fn _debugPrintHamtImpl(rt: *RT, ref: Ref) void {
        std.debug.assert(ref.kind() == .hamt);
        const hamt = rt.get(.hamt, ref);

        for (hamt) |child| {
            if (child.isNil()) {
                // std.debug.print(", ", .{});
                continue;
            }
            switch (child.kind()) {
                .cons => {
                    const cons = rt.get(.cons, child);
                    if (cons[0].isNil())
                        std.debug.print("nil", .{})
                    else
                        rt._debugPrintImpl(cons[0]);
                    std.debug.print(" ", .{});
                    if (cons[1].isNil())
                        std.debug.print("nil", .{})
                    else
                        rt._debugPrintImpl(cons[1]);
                    std.debug.print(",", .{});
                },
                .hamt => {
                    rt._debugPrintHamtImpl(child);
                },
                else => unreachable,
            }
            std.debug.print(" ", .{});
        }
    }
};

pub fn main() !void {
    // try scratch();
    // try fuzz();
    try benchmark();
    try benchmarkReference();
}

fn scratch() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rt = try RT.init(alloc);
    defer rt.deinit();

    const h0 = rt.create(.hamt, [_]Ref{nil} ** 16);
    rt.debugPrint(h0);

    const h1a = rt.hamtAssoc(h0, rt.create(.real, 55.0), rt.create(.real, 1.0));
    rt.debugPrint(h1a);
    const h1b = rt.hamtAssoc(h0, rt.create(.real, 1843.0), rt.create(.real, 1.0));
    rt.debugPrint(h1b);

    const h2a = rt.hamtAssoc(h1a, rt.create(.real, 1843.0), rt.create(.real, 2.0));
    rt.debugPrint(h2a);
    const h2b = rt.hamtAssoc(h1b, rt.create(.real, 55.0), rt.create(.real, 2.0));
    rt.debugPrint(h2b);
}

fn fuzz() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();

    const ns = [_]u32{ 32, 64, 128, 256, 612, 1024, 2048, 4096, 8192, 16384 };
    const m = 30000;

    for (ns) |n| {
        std.debug.print("{}\n", .{n});
        var rt = try RT.init(alloc);
        defer rt.deinit();

        var h = rt.create(
            .hamt,
            .{ nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil },
        );
        var s = std.AutoHashMap(u32, u32).init(alloc);
        defer s.deinit();

        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.create(.real, @floatFromInt(x));
            const b = rt.create(.real, @floatFromInt(y));

            std.debug.print("{} {}\n", .{ rt.hamtContains(h, a), s.contains(x) });
            std.debug.assert(rt.hamtContains(h, a) == s.contains(x));
            if (rt.hamtContains(h, a)) {
                std.debug.assert(
                    @as(u32, @intFromFloat(rt.get(.real, rt.hamtGet(h, a)))) == s.get(x).?,
                );
                const h2 = rt.hamtDissoc(h, a);
                h = h2;
                _ = s.remove(x);
                std.debug.assert(!rt.hamtContains(h, a));
                std.debug.assert(rt.hamtContains(h, a) == s.contains(x));
            } else {
                const h2 = rt.hamtAssoc(h, a, b);
                h = h2;
                try s.put(x, y);
                std.debug.assert(rt.hamtContains(h, a));
                std.debug.assert(rt.hamtContains(h, a) == s.contains(x));
            }
        }
    }
}

fn benchmark() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var timer = try std.time.Timer.start();

    const ns = [_]u32{ 32, 64, 128, 256, 612, 1024, 2048, 4096, 8192, 16384 };
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
            const x: f64 = @floatFromInt(rand.intRangeLessThan(u32, 0, n));
            const a = rt.create(.real, x);
            const b = rt.create(.real, @floatFromInt(rand.intRangeLessThan(u32, 0, n)));

            // std.debug.print("{d:.2}\t-> {}\n", .{ x, a.hash() });

            if (rt.hamtContains(h, a)) {
                const h2 = rt.hamtDissoc(h, a);
                h = h2;
                n_dissoc += 1;
            } else {
                const h2 = rt.hamtAssoc(h, a, b);
                h = h2;
                n_assoc += 1;
            }

            // _ = k;
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
    const ns = [_]u32{ 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384 };
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
