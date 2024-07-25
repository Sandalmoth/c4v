const std = @import("std");
const BlockPool = @import("blockpool.zig").BlockPool(.{});

const _gc = @import("gc6.zig");
const Object = _gc.Object;

const RT = struct {
    gc: _gc.GC,

    fn init(pool: *BlockPool) !RT {
        return .{
            .gc = try _gc.GC.init(pool),
        };
    }

    fn deinit(rt: *RT) void {
        rt.gc.deinit();
        rt.* = undefined;
    }

    fn create(rt: *RT, comptime kind: _gc.Kind, val: _gc.ValueType(kind)) *Object {
        return rt.gc.create(kind, val) catch @panic("GC allocation failure");
    }

    fn eql(rt: *RT, aref: ?*Object, bref: ?*Object) bool {
        if (aref == bref) return true;
        if (aref == null or bref == null) return false;
        if (aref.?.hash != bref.?.hash) return false;
        if (aref.?.kind != bref.?.kind) return false;
        return switch (aref.?.kind) {
            .real => aref.?.value(.real) == bref.?.value(.real),
            .cons => blk: {
                const a = aref.?.value(.cons);
                const b = bref.?.value(.cons);
                break :blk rt.eql(a[0], b[0]) and rt.eql(a[1], b[1]);
            },
            .hamt => blk: {
                const a = aref.?.value(.hamt);
                const b = bref.?.value(.hamt);
                // FIXME this won't correctly equate hamt's with differently ordered alist leaves
                for (a, b) |x, y| if (!rt.eql(x, y)) break :blk false;
                break :blk true;
            },
            .string => std.mem.eql(u8, aref.?.value(.string), bref.?.value(.string)),
        };
    }

    fn hamtAssoc(rt: *RT, hamtref: *Object, keyref: *Object, valref: *Object) *Object {
        return rt._hamtAssocImpl(hamtref, keyref, valref, 0);
    }
    fn _hamtAssocImpl(rt: *RT, hamtref: *Object, keyref: *Object, valref: *Object, depth: u32) *Object {
        std.debug.assert(hamtref.kind == .hamt);
        var node = hamtref.value(.hamt); // NOTE, this is a by-value copy, not a reference
        const i = (keyref.hash >> @intCast(depth * 4)) & 0b1111;
        if (depth < 7) {
            if (node[i] == null) {
                node[i] = rt.create(.cons, .{ keyref, valref });
            } else {
                switch (node[i].?.kind) {
                    .cons => {
                        const cons = node[i].?.value(.cons);
                        if (rt.eql(cons[0], keyref)) {
                            // key is already present, replace val
                            node[i] = rt.create(.cons, .{ keyref, valref });
                        } else {
                            // slot is already occupied, create new level of hamt
                            const k = (cons[0].?.hash >> @intCast((depth + 1) * 4)) & 0b1111;
                            var contents = [_]?*Object{null} ** 16;
                            // NOTE since we're creating a next level,
                            // we need to take into consideration that that level could be a leaf
                            // and if so use an alist instead of a direct cons
                            if (depth < 6) {
                                contents[k] = node[i];
                            } else {
                                contents[k] = rt.create(.cons, .{ node[i], null });
                            }
                            const h = rt.create(.hamt, contents);
                            node[i] = rt._hamtAssocImpl(h, keyref, valref, depth + 1);
                        }
                    },
                    .hamt => node[i] = rt._hamtAssocImpl(node[i].?, keyref, valref, depth + 1),
                    else => unreachable,
                }
            }
        } else {
            // we've reached the leaf level alist
            if (node[i] == null) {
                node[i] = rt.create(.cons, .{ rt.create(.cons, .{ keyref, valref }), null });
            } else {
                std.debug.assert(node[i].?.kind == .cons);
                // first delete our key from the alist (noop-ish if not present)
                // then we can just add it
                const alist = rt._alistDissoc(node[i].?, keyref);
                node[i] = rt.create(.cons, .{ rt.create(.cons, .{ keyref, valref }), alist });
            }
        }
        return rt.create(.hamt, node);
    }

    fn hamtContains(rt: *RT, hamtref: *Object, keyref: *Object) bool {
        return rt._hamtContainsImpl(hamtref, keyref, 0);
    }
    fn _hamtContainsImpl(rt: *RT, hamtref: *Object, keyref: *Object, depth: u32) bool {
        std.debug.assert(hamtref.kind == .hamt);
        var node = hamtref.value(.hamt);
        const i = (keyref.hash >> @intCast(depth * 4)) & 0b1111;
        if (node[i] == null) return false;
        if (depth < 7) {
            return switch (node[i].?.kind) {
                .cons => blk: {
                    const cons = node[i].?.value(.cons);
                    break :blk rt.eql(cons[0], keyref);
                },
                .hamt => rt._hamtContainsImpl(node[i].?, keyref, depth + 1),
                else => unreachable,
            };
        } else {
            std.debug.assert(node[i].?.kind == .cons);
            var walk = node[i].?.value(.cons);
            while (true) {
                if (rt.eql(walk[0].?.value(.cons)[0], keyref)) return true;
                if (walk[1] == null) return false;
                walk = walk[1].?.value(.cons);
            }
        }
    }

    fn hamtGet(rt: *RT, hamtref: *Object, keyref: *Object) ?*Object {
        return rt._hamtGetImpl(hamtref, keyref, 0);
    }
    fn _hamtGetImpl(rt: *RT, hamtref: *Object, keyref: *Object, depth: u32) ?*Object {
        std.debug.assert(hamtref.kind == .hamt);
        var node = hamtref.value(.hamt);
        const i = (keyref.hash >> @intCast(depth * 4)) & 0b1111;
        if (node[i] == null) return null;
        if (depth < 7) {
            return switch (node[i].?.kind) {
                .cons => blk: {
                    const cons = node[i].?.value(.cons);
                    break :blk if (rt.eql(cons[0], keyref)) cons[1] else null;
                },
                .hamt => rt._hamtGetImpl(node[i].?, keyref, depth + 1),
                else => unreachable,
            };
        } else {
            std.debug.assert(node[i].?.kind == .cons);
            var walk = node[i].?.value(.cons);
            while (true) {
                const a = walk[0].?.value(.cons);
                if (rt.eql(a[0], keyref)) return a[1];
                if (walk[1] == null) return null;
                walk = walk[1].?.value(.cons);
            }
        }
    }

    fn hamtDissoc(rt: *RT, hamtref: *Object, keyref: *Object) ?*Object {
        return rt._hamtDissocImpl(hamtref, keyref, 0);
    }
    fn _hamtDissocImpl(rt: *RT, hamtref: *Object, keyref: *Object, depth: u32) ?*Object {
        std.debug.assert(hamtref.kind == .hamt);
        var node = hamtref.value(.hamt);
        const i = (keyref.hash >> @intCast(depth * 4)) & 0b1111;
        if (node[i] == null) return hamtref;
        if (depth < 7) {
            switch (node[i].?.kind) {
                .cons => {
                    const cons = node[i].?.value(.cons);
                    if (!rt.eql(cons[0], keyref)) return hamtref;

                    // if we're deleting the last child of this node, delete the whole node
                    if (depth > 0) {
                        var n: u32 = 0;
                        for (node) |c| {
                            if (c != null) n += 1;
                        }
                        std.debug.assert(n > 0);
                        if (n == 1) return null;
                    }

                    node[i] = null;
                },
                .hamt => {
                    const result = rt._hamtDissocImpl(node[i].?, keyref, depth + 1);
                    if (rt.eql(node[i], result)) return hamtref;
                    node[i] = result;
                },
                else => unreachable,
            }
        } else {
            node[i] = rt._alistDissoc(node[i].?, keyref);
            // if we emptied the alist and this node is otherwise empty, delete the node
            if (depth > 0) {
                var n: u32 = 0;
                for (node) |c| {
                    if (c != null) n += 1;
                }
                if (n == 0) return null;
            }
        }
        return rt.create(.hamt, node);
    }
    fn _alistDissoc(rt: *RT, alistref: *Object, keyref: *Object) ?*Object {
        std.debug.assert(alistref.kind == .cons);
        // if we have found the key  return the cdr
        // else create a new cons of the car and recur
        // this adds a bit of unneeded gc churn, but hash collisions shouldn't be that common
        const cons = alistref.value(.cons);
        if (rt.eql(cons[0].?.value(.cons)[0], keyref)) return cons[1];
        if (cons[1] == null) return null;
        return rt.create(.cons, .{ cons[0], rt._alistDissoc(cons[0].?, keyref) });
    }

    fn debugPrint(rt: *RT, ref: *Object) void {
        if (ref == null) {
            std.debug.print("null", .{});
        } else {
            rt._debugPrintImpl(ref);
        }
        std.debug.print("\n", .{});
    }
    fn _debugPrintImpl(rt: *RT, ref: *Object) void {
        switch (ref.kind) {
            .real => std.debug.print("{}", .{ref.value(.real)}),
            .cons => {
                const cons = ref.value(.cons);
                // TODO special case for list printing?
                std.debug.print("(", .{});
                if (cons[0] == null)
                    std.debug.print("null", .{})
                else
                    rt._debugPrintImpl(cons[0]);
                std.debug.print(" . ", .{});
                if (cons[1] == null)
                    std.debug.print("null", .{})
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

    fn _debugPrintHamtImpl(rt: *RT, ref: *Object) void {
        std.debug.assert(ref.kind == .hamt);
        const hamt = ref.value(.hamt);

        for (hamt) |child| {
            if (child == null) {
                // std.debug.print(", ", .{});
                continue;
            }
            switch (child.kind) {
                .cons => {
                    const cons = child.value(.cons);
                    if (cons[0] == null)
                        std.debug.print("null", .{})
                    else
                        rt._debugPrintImpl(cons[0]);
                    std.debug.print(" ", .{});
                    if (cons[1] == null)
                        std.debug.print("null", .{})
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
    // try fillrate();
    // try scratch();
    try fuzz();
    try bench();
    try benchmark();
    try benchmarkReference();
}

fn fillrate() !void {
    var timer = try std.time.Timer.start();

    const T = struct {
        x: usize,
        pad: [3]usize,
    };
    const M = 128;

    var pool_a = try BlockPool.init(std.heap.page_allocator);
    defer pool_a.deinit();
    var data_a: [1024]*[M]T = undefined;

    timer.reset();
    for (0..1024) |i| {
        const a = try pool_a.create([M]T);
        for (0..M) |j| {
            a[j].x = j;
        }
        data_a[i] = a;
    }
    std.debug.print("{}\t{}\n", .{ timer.read(), timer.read() / 1024 / M });

    var pool_b = try BlockPool.init(std.heap.page_allocator);
    defer pool_b.deinit();
    var data_b: [1024]*[M]T = undefined;
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var shuffle: [M]usize = undefined;

    timer.reset();
    for (0..1024) |i| {
        for (0..M) |k| shuffle[k] = k;
        for (0..M) |k| {
            const l = rand.intRangeLessThan(usize, k, M);
            const tmp = shuffle[k];
            shuffle[k] = shuffle[l];
            shuffle[l] = tmp;
        }

        const b = try pool_b.create([M]T);
        for (0..M) |j| {
            b[shuffle[j]].x = j;
        }
        data_b[i] = b;
    }
    std.debug.print("{}\t{}\n", .{ timer.read(), timer.read() / 1024 / M });

    var acc: u64 = 0;
    for (data_a, data_b) |a, b| {
        for (0..M) |i| {
            const j = (b[i].x * 89) % M;
            acc +%= a[j].x * b[j].x;
        }
    }
    std.debug.print("{}\n", .{acc});

    for (data_a) |a| pool_a.destroy(a);
    for (data_b) |b| pool_b.destroy(b);
}

// fn gcperf() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     const alloc = gpa.allocator();
//     var gc = try _gc.GC.init(alloc);
//     defer gc.deinit();

//     for (0..1000000) |_| _ = try gc.create(.real, 0.0);
// }

fn scratch() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rt = try RT.init(alloc);
    defer rt.deinit();

    const h0 = rt.create(.hamt, [_]*Object{null} ** 16);
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
        var pool = try BlockPool.init(std.heap.page_allocator);
        defer pool.deinit();
        var rt = try RT.init(&pool);
        defer rt.deinit();

        var h = rt.create(
            .hamt,
            [_]?*Object{null} ** 16,
        );
        var s = std.AutoHashMap(u32, u32).init(alloc);
        defer s.deinit();

        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.create(.real, @floatFromInt(x));
            const b = rt.create(.real, @floatFromInt(y));

            // std.debug.print("{} {}\n", .{ rt.hamtContains(h, a), s.contains(x) });
            std.debug.assert(rt.hamtContains(h, a) == s.contains(x));
            if (rt.hamtContains(h, a)) {
                std.debug.assert(
                    @as(u32, @intFromFloat(rt.hamtGet(h, a).?.value(.real))) == s.get(x).?,
                );
                const h2 = rt.hamtDissoc(h, a);
                h = h2.?;
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

    const ns = [_]u32{ 32, 64, 128, 256, 612, 1024, 2048, 4096, 8192, 16384, 32768, 65536 };
    const m = 30000;
    std.debug.print("cap\tns\tt_gc\tt_fnd\tt_crt\tn_ass\tn_diss\n", .{});

    for (ns) |n| {
        var pool = try BlockPool.init(std.heap.page_allocator);
        defer pool.deinit();
        var rt = try RT.init(&pool);
        defer rt.deinit();

        timer.reset();
        var h = rt.create(
            .hamt,
            [_]?*Object{null} ** 16,
        );

        var n_assoc: u32 = 0;
        var n_dissoc: u32 = 0;
        var t_gc: usize = 0;
        var t_edit: usize = 0;
        var t_find: usize = 0;
        var t_create: usize = 0;
        var acc: f64 = 0;

        for (0..m) |k| {
            const x: f64 = @floatFromInt(rand.intRangeLessThan(u32, 0, n));
            const a = rt.create(.real, x);
            const b = rt.create(.real, @floatFromInt(rand.intRangeLessThan(u32, 0, n)));

            // std.debug.print("{d:.2}\t-> {}\n", .{ x, a.hash });

            if (rt.hamtContains(h, a)) {
                const h2 = rt.hamtDissoc(h, a);
                h = h2.?;
                n_dissoc += 1;
            } else {
                const h2 = rt.hamtAssoc(h, a, b);
                h = h2;
                n_assoc += 1;
            }

            // _ = k;
            if ((k + 1) % 10000 == 0) {
                const start = timer.read();
                rt.gc.traceRoot(&h);
                try rt.gc.collect();
                t_gc += timer.read() - start;
            }
        }

        t_edit = timer.lap();

        var to_search = try std.ArrayList(*Object).initCapacity(alloc, m);
        defer to_search.deinit();
        for (0..m) |_| {
            const x: f64 = @floatFromInt(rand.intRangeLessThan(u32, 0, n));
            const a = rt.create(.real, x);
            try to_search.append(a);
        }

        t_create = timer.lap();

        for (0..m) |_k| {
            const k: f64 = @floatFromInt(_k);
            const a = to_search.items[_k];
            if (rt.hamtContains(h, a)) acc += k * rt.hamtGet(h, a).?.value(.real);
        }

        t_find = timer.read();

        std.debug.print(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t\t{}\n",
            .{ n, t_edit / m, t_gc / m, t_find / m, t_create / m, n_assoc, n_dissoc, acc },
        );
    }
}

fn bench() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072 };
    const m = 10000;
    const o = 1000;

    var timer = try std.time.Timer.start();

    for (ns) |n| {
        var pool = try BlockPool.init(std.heap.page_allocator);
        defer pool.deinit();
        var rt = try RT.init(&pool);
        defer rt.deinit();

        timer.reset();
        var h = rt.create(
            .hamt,
            [_]?*Object{null} ** 16,
        );
        var tgc: u64 = 0;
        for (0..m) |i| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.create(.real, @floatFromInt(x));
            const b = rt.create(.real, @floatFromInt(y));
            if (rt.hamtContains(h, a)) {
                const h2 = rt.hamtDissoc(h, a);
                h = h2.?;
            } else {
                const h2 = rt.hamtAssoc(h, a, b);
                h = h2;
            }
            if ((i + 1) % o == 0) {
                const t = timer.read();
                rt.gc.traceRoot(&h);
                try rt.gc.collect();
                tgc += timer.read() - t;
            }
        }
        const ttot = timer.read();

        std.debug.print("{}\t{}\t{}\n", .{ n, (ttot - tgc) / m, tgc / m });
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
    const ns = [_]u32{ 32, 64, 128, 256, 612, 1024, 2048, 4096, 8192, 16384, 32768, 65536 };
    const m = 30000;
    std.debug.print("cap\tns\tt_fnd\tn_ass\tn_diss\tn_creat\n", .{});

    for (ns) |n| {
        timer.reset();
        var s = std.AutoHashMap(u64, u64).init(alloc);
        defer s.deinit();
        var n_assoc: u32 = 0;
        var n_dissoc: u32 = 0;
        var t_edit: usize = 0;
        var t_find: usize = 0;
        var acc: u64 = 0;
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

        t_edit = timer.lap();

        for (0..m) |k| {
            const a = rand.intRangeLessThan(u64, 0, n);
            if (s.contains(a)) acc +%= k *% s.get(a).?;
        }

        t_find = timer.read();

        std.debug.print(
            "{}\t{}\t{}\t{}\t{}\t\t{}\n",
            .{ n, t_edit / m, t_find / m, n_assoc, n_dissoc, acc },
        );
    }
}
