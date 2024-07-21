const std = @import("std");
const BlockPool = @import("blockpool.zig").BlockPool(.{});

const RT7 = @import("rt7.zig").RT;
const debugPrint7 = @import("rt7.zig").debugPrint;

pub fn main() !void {
    try scratch();
    try fuzz();
    try bench();
}

fn scratch() !void {}

fn fuzz() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072 };
    const m = 10000;
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

fn bench() !void {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    var rand = rng.random();
    var pool = try BlockPool.init(std.heap.page_allocator);
    defer pool.deinit();

    const ns = [_]u32{ 32, 128, 512, 2048, 8192, 32768, 131072 };
    const m = 10000;
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
