const std = @import("std");

const BLOCK_SIZE = 4 * 1024;

const Hash = u32;
const NILHASH: Hash = 0x00b4b4b4;

pub const Kind = enum(u8) {
    real,
    cons,
    hamt,
};

pub fn KindType(comptime kind: Kind) type {
    return switch (kind) {
        .real => f64,
        .cons => [2]Ref,
        .hamt => [16]Ref,
    };
}

fn finalize(comptime kind: Kind, val: KindType(kind)) void {
    _ = val;
    switch (kind) {
        .real => {},
        .cons => {},
        .hamt => {},
    }
}

pub const Ref = extern struct {
    _location: u32 align(8),
    _property: u32,

    pub fn init(_kind: Kind, _page: u32, _slot: u8, _hash: u32) Ref {
        std.debug.assert(_page <= 0x00ffffff);
        return .{
            ._location = _page | (@as(u32, _slot) << 24),
            ._property = (_hash & 0x00ffffff) | (@as(u32, @intFromEnum(_kind)) << 24),
        };
    }

    fn page(ref: Ref) u32 {
        return ref._location & 0x00ffffff;
    }

    fn slot(ref: Ref) u8 {
        return @intCast(ref._location >> 24);
    }

    pub fn hash(ref: Ref) u32 {
        return ref._property & 0x00ffffff;
    }

    pub fn kind(ref: Ref) Kind {
        return @enumFromInt(ref._property >> 24);
    }

    pub fn isNil(ref: Ref) bool {
        return ref.page() == 0x00ffffff;
    }
};

pub const nil = Ref.init(undefined, 0x00ffffff, undefined, NILHASH);

comptime {
    std.debug.assert(@sizeOf(Ref) == 8);
    std.debug.assert(@alignOf(Ref) == 8);
}

fn SingularGCType(comptime kind: Kind) type {
    const T = KindType(kind);
    const N = @min(256, (BLOCK_SIZE - 128) / @sizeOf(T));
    const MERGE_MAX = N / 3;

    const Page = struct {
        index: u32,
        len: usize,
        data: [N]T align(64),
        marks: std.StaticBitSet(N),
        used: std.StaticBitSet(N),

        pub fn create(alloc: std.mem.Allocator) !*@This() {
            var page = try alloc.create(@This());
            page.len = 0;
            page.used = std.StaticBitSet(N).initEmpty();
            return page;
        }

        fn destroy(page: *@This(), alloc: std.mem.Allocator) void {
            var it = page.used.iterator(.{});
            while (it.next()) |i| {
                finalize(kind, page.data[i]);
            }
            alloc.destroy(page);
        }
    };

    std.debug.assert(N >= 3 and N <= 256);
    std.debug.assert(@sizeOf(Page) <= BLOCK_SIZE);

    const TableEntry = struct {
        page: usize, // ptr to page if not free, index in page_table if free
        mark: bool,
        free: bool,
    };

    return struct {
        const SGC = @This();

        alloc: std.mem.Allocator,
        rand: std.Random.Random,
        shuffle: [N]u8,
        active_page: *Page,
        inactive_pages: std.ArrayList(*Page),
        pairing_buffer: std.ArrayList(*Page),
        page_table: std.MultiArrayList(TableEntry),
        free_list: usize,

        fn init(alloc: std.mem.Allocator, rand: std.Random.Random) !SGC {
            var sgc = SGC{
                .alloc = alloc,
                .rand = rand,
                .shuffle = undefined,
                .active_page = undefined,
                .inactive_pages = std.ArrayList(*Page).init(alloc),
                .pairing_buffer = std.ArrayList(*Page).init(alloc),
                .page_table = std.MultiArrayList(TableEntry){},
                .free_list = std.math.maxInt(usize),
            };
            try sgc.newActivePage(true);
            return sgc;
        }

        fn deinit(sgc: *SGC) void {
            sgc.active_page.destroy(sgc.alloc);
            for (sgc.inactive_pages.items) |page| page.destroy(sgc.alloc);
            sgc.inactive_pages.deinit();
            sgc.pairing_buffer.deinit();
            sgc.page_table.deinit(sgc.alloc);
            sgc.* = undefined;
        }

        fn newActivePage(sgc: *SGC, comptime first_time: bool) !void {
            // NOTE order ensures an error doesn't break the structure we have
            if (!first_time) {
                try sgc.inactive_pages.ensureUnusedCapacity(1);
                try sgc.pairing_buffer.ensureTotalCapacity(sgc.inactive_pages.items.len + 1);
            }
            if (sgc.free_list == std.math.maxInt(usize)) {
                try sgc.page_table.ensureUnusedCapacity(sgc.alloc, 1);
            }
            const new_page = try Page.create(sgc.alloc);
            if (!first_time) {
                sgc.inactive_pages.appendAssumeCapacity(sgc.active_page);
            }
            sgc.active_page = new_page;

            // add new active page to page table
            var page_index: u32 = undefined;
            if (sgc.free_list == std.math.maxInt(usize)) {
                page_index = @intCast(sgc.page_table.len);
                sgc.page_table.appendAssumeCapacity(.{
                    .page = @intFromPtr(new_page),
                    .mark = true,
                    .free = false,
                });
            } else {
                page_index = @intCast(sgc.free_list);
                sgc.free_list = sgc.page_table.items(.page)[page_index];
                sgc.page_table.items(.page)[page_index] = @intFromPtr(new_page);
                sgc.page_table.items(.free)[page_index] = false;
            }
            sgc.active_page.index = page_index;

            // setup the shuffle vector
            for (0..N) |i| sgc.shuffle[i] = @intCast(i);
            for (0..N) |i| {
                const j = sgc.rand.intRangeLessThan(usize, i, N);
                const tmp = sgc.shuffle[i];
                sgc.shuffle[i] = sgc.shuffle[j];
                sgc.shuffle[j] = tmp;
            }
        }

        fn create(sgc: *SGC, val: T, hash: u32) !Ref {
            if (sgc.active_page.len == N) try sgc.newActivePage(false);

            const i = sgc.shuffle[sgc.active_page.len];
            sgc.active_page.data[i] = val;
            sgc.active_page.marks.set(i); // needed if we make this concurrent
            sgc.active_page.used.set(i);
            sgc.active_page.len += 1;

            return Ref.init(kind, sgc.active_page.index, i, hash);
        }

        fn get(sgc: *SGC, ref: Ref) T {
            std.debug.assert(!sgc.page_table.items(.free)[ref.page()]);
            const page: *Page = @ptrFromInt(sgc.page_table.items(.page)[ref.page()]);
            std.debug.assert(page.used.isSet(ref.slot()));
            return page.data[ref.slot()];
        }

        fn trace(sgc: *SGC, ref: Ref, gc: *GC) void {
            std.debug.assert(!sgc.page_table.items(.free)[ref.page()]);
            const page: *Page = @ptrFromInt(sgc.page_table.items(.page)[ref.page()]);
            sgc.page_table.items(.mark)[ref.page()] = true;
            page.marks.set(ref.slot());
            switch (kind) {
                .real => {},
                .cons => {
                    const cons = sgc.get(ref);
                    gc.trace(cons[0]);
                    gc.trace(cons[1]);
                },
                .hamt => {
                    const hamt = sgc.get(ref);
                    for (hamt) |child| gc.trace(child);
                },
            }
        }

        fn sweep(sgc: *SGC) void {
            std.debug.print("{}\t[ ", .{kind});
            var walk = sgc.free_list;
            while (walk != std.math.maxInt(usize)) {
                std.debug.print("{} ", .{walk});
                walk = sgc.page_table.items(.page)[walk];
            }
            std.debug.print("]\n", .{});

            // NOTE we only sweep the inactive pages
            // remove all values that haven't been marked
            for (sgc.inactive_pages.items) |page| {
                const unmarked = page.used.differenceWith(page.marks);
                var it = unmarked.iterator(.{});
                while (it.next()) |j| {
                    page.used.unset(j);
                    finalize(kind, page.data[j]);
                    page.len -= 1;
                }
            }
            // enable reuse of any page table entries that no references were traced through
            var i = sgc.page_table.len;
            const slices = sgc.page_table.slice();
            while (i > 0) : (i -= 1) {
                if (slices.items(.mark)[i - 1]) continue;
                if (slices.items(.free)[i - 1]) continue;
                if (slices.items(.page)[i - 1] == @intFromPtr(sgc.active_page)) continue;
                slices.items(.page)[i - 1] = sgc.free_list;
                slices.items(.free)[i - 1] = true;
                sgc.free_list = i - 1;
                // NOTE we don't need to call finalizers, since it's already done in previous step
            }

            // prepare for next round of sweeping
            for (sgc.inactive_pages.items) |page| {
                page.marks = std.StaticBitSet(N).initEmpty();
            }
            for (sgc.page_table.items(.mark)) |*mark| {
                // we don't care about marks for things in the free list
                // so branching to avoid unmarking those isn't necessary
                mark.* = false;
            }
        }

        fn canMerge(a: *Page, b: *Page) bool {
            return a.used.intersectWith(b.used).findFirstSet() == null;
        }

        fn mergeInto(src: *Page, dst: *Page) void {
            std.debug.assert(canMerge(src, dst));
            var it = src.used.iterator(.{});
            while (it.next()) |i| {
                std.debug.assert(!dst.used.isSet(i));
                dst.data[i] = src.data[i];
                dst.used.set(i);
                dst.len += 1;
            }
        }

        fn compact(sgc: *SGC) void {
            // NOTE we only try to merge the inactive pages
            sgc.pairing_buffer.clearRetainingCapacity();
            for (sgc.inactive_pages.items) |page| {
                if (page.len > MERGE_MAX) continue; // merge probability -> 0 as occupancy -> 50%
                const i = sgc.rand.uintLessThan(usize, sgc.pairing_buffer.items.len + 1);
                if (i == sgc.pairing_buffer.items.len) {
                    sgc.pairing_buffer.appendAssumeCapacity(page);
                } else {
                    sgc.pairing_buffer.appendAssumeCapacity(sgc.pairing_buffer.items[i]);
                    sgc.pairing_buffer.items[i] = page;
                }
            }
            // randomly try to merge pages
            // i wonder if using offsets from a list of primes would be better?
            for (0..60) |offset| {
                var k: usize = 0;
                while (k < sgc.pairing_buffer.items.len) : (k += 1) {
                    if (sgc.pairing_buffer.items.len == 1) return;
                    const i = k % sgc.pairing_buffer.items.len;
                    const j = (k + offset) % sgc.pairing_buffer.items.len;
                    if (!canMerge(
                        sgc.pairing_buffer.items[i],
                        sgc.pairing_buffer.items[j],
                    )) continue;
                    mergeInto(sgc.pairing_buffer.items[i], sgc.pairing_buffer.items[j]);
                    std.debug.print("MERGE!\n", .{});
                    // redirect all entries pointing to the page we just merged
                    for (sgc.page_table.items(.page), sgc.page_table.items(.free)) |*pt, free| {
                        if (free) continue;
                        if (pt.* != @intFromPtr(sgc.pairing_buffer.items[i])) continue;
                        // so long as this is atomic, this can run concurrently
                        pt.* = @intFromPtr(sgc.pairing_buffer.items[j]);
                    }
                    // destroy the page we merged away from in the list of inactive_pages
                    var ixd: usize = 0;
                    while (sgc.inactive_pages.items[ixd] != sgc.pairing_buffer.items[i]) {
                        ixd += 1;
                    }
                    sgc.alloc.destroy(sgc.inactive_pages.swapRemove(ixd));
                    // and drop it from considereation in future merges
                    _ = sgc.pairing_buffer.swapRemove(i);
                }
            }
        }

        fn occupancy(sgc: *SGC) f64 {
            var n: usize = 0;
            n += sgc.active_page.used.count();
            for (sgc.inactive_pages.items) |page| n += page.used.count();
            return @as(f64, @floatFromInt(n)) /
                @as(f64, @floatFromInt(N * (1 + sgc.inactive_pages.items.len)));
        }
    };
}

pub const GC = struct {
    const n_kinds = std.meta.fields(Kind).len;

    alloc: std.mem.Allocator,
    sgcs: [n_kinds]*anyopaque,

    rng: *std.Random.DefaultPrng,

    pub fn init(alloc: std.mem.Allocator) !GC {
        var gc = GC{
            .alloc = alloc,
            .sgcs = undefined,
            .rng = try alloc.create(std.Random.DefaultPrng),
        };
        gc.rng.* = std.Random.DefaultPrng.init(
            (@as(u64, @bitCast(std.time.microTimestamp())) | 1) *% 11400714819323198393,
        );
        errdefer alloc.destroy(gc.rng);
        inline for (0..n_kinds) |i| {
            const kind: Kind = @enumFromInt(i);
            const SGC = SingularGCType(kind);
            const sgc = try alloc.create(SGC);
            errdefer alloc.destroy(sgc);
            // NOTE how do the scopes work out with that errdefer?
            // as in, if we fail, will all the SGCs allocated so far be destroyed?
            // presumably, though inline for unrolls the loop, it maintains each iterations scope
            // hence, the errdefer will only trigger for that iteration
            // and making this completely safe is kinda tricky (and mostly a waste of time)
            // so, maybe later... TODO
            sgc.* = try SGC.init(alloc, gc.rng.random());
            gc.sgcs[i] = sgc;
        }
        return gc;
    }

    pub fn deinit(gc: *GC) void {
        gc.alloc.destroy(gc.rng);
        inline for (0..n_kinds) |i| {
            const kind: Kind = @enumFromInt(i);
            const sgc = gc.getSGC(kind);
            sgc.deinit();
            gc.alloc.destroy(sgc);
        }
        gc.* = undefined;
    }

    fn getSGC(gc: *GC, comptime kind: Kind) *SingularGCType(kind) {
        return @alignCast(@ptrCast(gc.sgcs[@intFromEnum(kind)]));
    }

    pub fn create(gc: *GC, comptime kind: Kind, val: KindType(kind)) !Ref {
        const hash = switch (kind) {
            .real => std.hash.XxHash32.hash(2590326161, std.mem.asBytes(&val)),
            .cons => blk: {
                var h: u32 = 0;
                h ^= if (val[0].isNil()) NILHASH else val[0].hash();
                h *%= 3425487983; // does this lose 1/4 of the randomness of the car?
                h ^= if (val[0].isNil()) NILHASH else val[1].hash();
                break :blk h;
            },
            .hamt => blk: {
                var h: u32 = 0;
                for (val) |child| {
                    h ^= if (child.isNil()) NILHASH else child.hash();
                    h *%= 3680932543; // same question, do we lose entropy in the early hashes?
                }
                break :blk h;
            },
        };
        return gc.getSGC(kind).create(val, hash);
    }

    pub fn get(gc: *GC, comptime kind: Kind, ref: Ref) KindType(kind) {
        std.debug.assert(!ref.isNil());
        std.debug.assert(ref.kind() == kind);
        const sgc = gc.getSGC(kind);
        return sgc.get(ref);
    }

    pub fn trace(gc: *GC, ref: Ref) void {
        if (ref.isNil()) return;
        switch (ref.kind()) {
            .real => {
                gc.getSGC(.real).trace(ref, gc);
            },
            .cons => {
                gc.getSGC(.cons).trace(ref, gc);
            },
            .hamt => {
                gc.getSGC(.hamt).trace(ref, gc);
            },
        }
    }

    pub fn sweep(gc: *GC) void {
        inline for (0..n_kinds) |i| {
            gc.getSGC(@enumFromInt(i)).sweep();
        }
    }

    pub fn compact(gc: *GC) void {
        inline for (0..n_kinds) |i| {
            gc.getSGC(@enumFromInt(i)).compact();
        }
    }
};

test "scratch" {
    var rng = std.Random.DefaultPrng.init(
        @bitCast(std.time.microTimestamp() *% 7951182790392048631),
    );
    const rand = rng.random();

    var gc = try GC.init(std.testing.allocator);
    defer gc.deinit();

    var a = std.ArrayList(Ref).init(std.testing.allocator);
    defer a.deinit();

    var w: f64 = 0.0;
    for (0..100) |_| {
        while (a.items.len < 1024) {
            if (a.items.len < 2 or rand.boolean()) {
                const x = try gc.create(.real, w);
                try a.append(x);
                w += 1.0;
            } else {
                const x = try gc.create(.cons, .{
                    a.items[rand.uintLessThan(usize, a.items.len)],
                    a.items[rand.uintLessThan(usize, a.items.len)],
                });
                try a.append(x);
            }
        }

        while (a.items.len > 512) {
            const i = rand.uintLessThan(usize, a.items.len);
            _ = a.swapRemove(i);
        }
        for (a.items) |x| {
            gc.trace(x);
        }

        gc.sweep();
        gc.compact();

        inline for (std.meta.fields(Kind)) |kind| {
            const sgc = gc.getSGC(@enumFromInt(kind.value));
            std.debug.print(
                "{}\tnum pages: {}\ttable size: {}\toccupancy: {d:.2}%\n",
                .{
                    kind,
                    sgc.inactive_pages.items.len + 1,
                    sgc.page_table.len,
                    sgc.occupancy() * 100,
                },
            );
        }
    }
}
