const std = @import("std");
const BlockPool = @import("blockpool.zig").BlockPool(.{});

const RT7 = @import("rt7.zig").RT;
const debugPrint7 = @import("rt7.zig").debugPrint;

const RT8 = @import("rt8.zig").RT;
const debugPrint8 = @import("rt8.zig").debugPrint;

const RT9 = @import("rt9.zig").RT;
const debugPrint9 = @import("rt9.zig").debugPrint;

pub fn main() !void {
    try scratch();
    std.debug.print("hashmap\n", .{});
    try benchHM();
    try benchLookupHM();
    std.debug.print("hamt-based\n", .{});
    try fuzz7();
    try bench7();
    try benchLookup7();
    std.debug.print("champ-based\n", .{});
    try fuzz8();
    try bench8();
    try benchLookup8();
    std.debug.print("champ-based-nomemo\n", .{});
    try fuzz9();
    try bench9();
    try benchLookup9();
}

fn scratch() !void {
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    var rt = try RT9.init(&pool);
    defer rt.deinit();

    var h = rt.newChamp();
    debugPrint9(h);
    h = rt.champAssoc(h, rt.newReal(1.0), rt.newString("hello"));
    debugPrint9(h);
    h = rt.champAssoc(h, rt.newString("world"), rt.newReal(2.0));
    debugPrint9(h);
    h = rt.champDissoc(h, rt.newReal(1.0)).?;
    debugPrint9(h);
    h = rt.champDissoc(h, rt.newString("world")).?;
    debugPrint9(h);
}

fn fuzz7() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;
    const o = 1000;

    for (ns) |n| {
        var rt = try RT7.init(&pool);
        defer rt.deinit();
        var h = rt.newHamt();

        var s = std.AutoHashMap(u32, u32).init(alloc);
        defer s.deinit();

        for (0..m) |i| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));

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

            if ((i + 1) % o == 0) {
                rt.gc.traceRoot(&h);
                try rt.gc.collect();
            }
        }
    }
}

fn fuzz8() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    const rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;
    const o = 1000;

    for (ns) |n| {
        var rt = try RT8.init(&pool);
        defer rt.deinit();
        var h = rt.newChamp();

        var s = std.AutoHashMap(u32, u32).init(alloc);
        defer s.deinit();

        for (0..m) |i| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));

            std.debug.assert(rt.champContains(h, a) == s.contains(x));
            if (rt.champContains(h, a)) {
                std.debug.assert(
                    @as(u32, @intFromFloat(rt.champGet(h, a).?.as(.real).data)) == s.get(x).?,
                );
                const h2 = rt.champDissoc(h, a);
                h = h2.?;
                _ = s.remove(x);
            } else {
                const h2 = rt.champAssoc(h, a, b);
                h = h2;
                try s.put(x, y);
            }

            const u = rand.intRangeLessThan(u32, 0, n);
            const v = rand.intRangeLessThan(u32, 0, n);
            const c = rt.newReal(@floatFromInt(u));
            const d = rt.newReal(@floatFromInt(v));
            if (rand.boolean()) {
                const h2 = rt.champDissoc(h, c);
                h = h2.?;
                _ = s.remove(u);
            } else {
                const h2 = rt.champAssoc(h, c, d);
                h = h2;
                try s.put(u, v);
            }

            if ((i + 1) % o == 0) {
                rt.gc.traceRoot(&h);
                rt.gc.shuffle();
                try rt.gc.collect();
            }
        }
    }
}

fn fuzz9() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    const rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;
    const o = 1000;

    for (ns) |n| {
        var rt = try RT9.init(&pool);
        defer rt.deinit();
        var h = rt.newChamp();

        var s = std.AutoHashMap(u32, u32).init(alloc);
        defer s.deinit();

        for (0..m) |i| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));

            std.debug.assert(rt.champContains(h, a) == s.contains(x));
            if (rt.champContains(h, a)) {
                std.debug.assert(
                    @as(u32, @intFromFloat(rt.champGet(h, a).?.as(.real).data)) == s.get(x).?,
                );
                const h2 = rt.champDissoc(h, a);
                h = h2.?;
                _ = s.remove(x);
            } else {
                const h2 = rt.champAssoc(h, a, b);
                h = h2;
                try s.put(x, y);
            }

            const u = rand.intRangeLessThan(u32, 0, n);
            const v = rand.intRangeLessThan(u32, 0, n);
            const c = rt.newReal(@floatFromInt(u));
            const d = rt.newReal(@floatFromInt(v));
            if (rand.boolean()) {
                const h2 = rt.champDissoc(h, c);
                h = h2.?;
                _ = s.remove(u);
            } else {
                const h2 = rt.champAssoc(h, c, d);
                h = h2;
                try s.put(u, v);
            }

            if ((i + 1) % o == 0) {
                rt.gc.traceRoot(&h);
                rt.gc.shuffle();
                try rt.gc.collect();
            }
        }
    }
}

fn benchHM() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;

    var timer = try std.time.Timer.start();

    for (ns) |n| {
        var h = std.AutoHashMap(u32, u32).init(alloc);
        defer h.deinit();

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            if (h.contains(x)) {
                try h.put(x, y);
            } else {
                _ = h.remove(x);
            }
            const u = rand.intRangeLessThan(u32, 0, n);
            const v = rand.intRangeLessThan(u32, 0, n);
            if (rand.boolean()) {
                try h.put(u, v);
            } else {
                _ = h.remove(v);
            }
        }
        const ttot = timer.read();

        std.debug.print("{}\t{}\n", .{ n, ttot / m });
    }
}

fn bench7() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;
    const o = 1000;

    var timer = try std.time.Timer.start();

    for (ns) |n| {
        var rt = try RT7.init(&pool);
        defer rt.deinit();

        timer.reset();
        var h = rt.newHamt();
        var tgc: u64 = 0;
        for (0..m) |i| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));
            if (rt.hamtContains(h, a)) {
                const h2 = rt.hamtDissoc(h, a);
                h = h2.?;
            } else {
                const h2 = rt.hamtAssoc(h, a, b);
                h = h2;
            }
            const u = rand.intRangeLessThan(u32, 0, n);
            const v = rand.intRangeLessThan(u32, 0, n);
            const c = rt.newReal(@floatFromInt(u));
            const d = rt.newReal(@floatFromInt(v));
            if (rand.boolean()) {
                const h2 = rt.hamtDissoc(h, c);
                h = h2.?;
            } else {
                const h2 = rt.hamtAssoc(h, c, d);
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

fn bench8() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;
    const o = 1000;

    var timer = try std.time.Timer.start();

    for (ns) |n| {
        var rt = try RT8.init(&pool);
        defer rt.deinit();

        timer.reset();
        var h = rt.newChamp();
        var tgc: u64 = 0;
        for (0..m) |i| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));
            if (rt.champContains(h, a)) {
                const h2 = rt.champDissoc(h, a);
                h = h2.?;
            } else {
                const h2 = rt.champAssoc(h, a, b);
                h = h2;
            }
            const u = rand.intRangeLessThan(u32, 0, n);
            const v = rand.intRangeLessThan(u32, 0, n);
            const c = rt.newReal(@floatFromInt(u));
            const d = rt.newReal(@floatFromInt(v));
            if (rand.boolean()) {
                const h2 = rt.champDissoc(h, c);
                h = h2.?;
            } else {
                const h2 = rt.champAssoc(h, c, d);
                h = h2;
            }
            if ((i + 1) % o == 0) {
                const t = timer.read();
                rt.gc.traceRoot(&h);
                rt.gc.shuffle();
                try rt.gc.collect();
                tgc += timer.read() - t;
            }
        }
        const ttot = timer.read();

        std.debug.print("{}\t{}\t{}\n", .{ n, (ttot - tgc) / m, tgc / m });
    }
}

fn bench9() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;
    const o = 1000;

    var timer = try std.time.Timer.start();

    for (ns) |n| {
        var rt = try RT9.init(&pool);
        defer rt.deinit();

        timer.reset();
        var h = rt.newChamp();
        var tgc: u64 = 0;
        for (0..m) |i| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));
            if (rt.champContains(h, a)) {
                const h2 = rt.champDissoc(h, a);
                h = h2.?;
            } else {
                const h2 = rt.champAssoc(h, a, b);
                h = h2;
            }
            const u = rand.intRangeLessThan(u32, 0, n);
            const v = rand.intRangeLessThan(u32, 0, n);
            const c = rt.newReal(@floatFromInt(u));
            const d = rt.newReal(@floatFromInt(v));
            if (rand.boolean()) {
                const h2 = rt.champDissoc(h, c);
                h = h2.?;
            } else {
                const h2 = rt.champAssoc(h, c, d);
                h = h2;
            }
            if ((i + 1) % o == 0) {
                const t = timer.read();
                rt.gc.traceRoot(&h);
                rt.gc.shuffle();
                try rt.gc.collect();
                tgc += timer.read() - t;
            }
        }
        const ttot = timer.read();

        std.debug.print("{}\t{}\t{}\n", .{ n, (ttot - tgc) / m, tgc / m });
    }
}

fn benchLookupHM() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;

    var timer = try std.time.Timer.start();
    var acc: usize = 0;

    for (ns) |n| {
        var h = std.AutoHashMap(u32, u32).init(alloc);

        for (0..n) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            try h.put(x, y);
        }

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            if (h.contains(x)) acc += 1;
        }
        const t1 = timer.read();

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            if (h.contains(x)) acc += 1;
        }
        const t2 = timer.read();

        std.debug.print("{}\t{}\t{}\n", .{ n, t1 / m, t2 / m });
    }
    std.debug.print("{}\n", .{acc});
}

fn benchLookup7() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;

    var timer = try std.time.Timer.start();
    var acc: usize = 0;

    for (ns) |n| {
        var rt = try RT7.init(&pool);
        defer rt.deinit();

        var h = rt.newHamt();
        var tgc: u64 = 0;
        for (0..n) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));
            const h2 = rt.hamtAssoc(h, a, b);
            h = h2;
        }

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            if (rt.hamtContains(h, a)) acc += 1;
        }
        const t1 = timer.read();

        timer.reset();
        rt.gc.traceRoot(&h);
        try rt.gc.collect();
        tgc += timer.read();

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            if (rt.hamtContains(h, a)) acc += 1;
        }
        const t2 = timer.read();

        std.debug.print("{}\t{}\t{}\t{}\n", .{ n, t1 / m, t2 / m, tgc / m });
    }
    std.debug.print("{}\n", .{acc});
}

fn benchLookup8() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;

    var timer = try std.time.Timer.start();
    var acc: usize = 0;

    for (ns) |n| {
        var rt = try RT8.init(&pool);
        defer rt.deinit();

        var h = rt.newChamp();
        var tgc: u64 = 0;
        for (0..n) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));
            const h2 = rt.champAssoc(h, a, b);
            h = h2;
        }

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            if (rt.champContains(h, a)) acc += 1;
        }
        const t1 = timer.read();

        timer.reset();
        rt.gc.traceRoot(&h);
        rt.gc.shuffle();
        try rt.gc.collect();
        tgc += timer.read();

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            if (rt.champContains(h, a)) acc += 1;
        }
        const t2 = timer.read();

        std.debug.print("{}\t{}\t{}\t{}\n", .{ n, t1 / m, t2 / m, tgc / m });
    }
    std.debug.print("{}\n", .{acc});
}

fn benchLookup9() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072, 524288 };
    const m = 100000;

    var timer = try std.time.Timer.start();
    var acc: usize = 0;

    for (ns) |n| {
        var rt = try RT9.init(&pool);
        defer rt.deinit();

        var h = rt.newChamp();
        var tgc: u64 = 0;
        for (0..n) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const y = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            const b = rt.newReal(@floatFromInt(y));
            const h2 = rt.champAssoc(h, a, b);
            h = h2;
        }

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            if (rt.champContains(h, a)) acc += 1;
        }
        const t1 = timer.read();

        timer.reset();
        rt.gc.traceRoot(&h);
        rt.gc.shuffle();
        try rt.gc.collect();
        tgc += timer.read();

        timer.reset();
        for (0..m) |_| {
            const x = rand.intRangeLessThan(u32, 0, n);
            const a = rt.newReal(@floatFromInt(x));
            if (rt.champContains(h, a)) acc += 1;
        }
        const t2 = timer.read();

        std.debug.print("{}\t{}\t{}\t{}\n", .{ n, t1 / m, t2 / m, tgc / m });
    }
    std.debug.print("{}\n", .{acc});
}
