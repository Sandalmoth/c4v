const std = @import("std");

const Ref = extern struct {
    _r: u32,

    fn new(page_ix: u32, slot_ix: u32) Ref {
        return .{ ._r = (page_ix << 8) | slot_ix };
    }

    fn page(r: Ref) u32 {
        return r._r >> 8;
    }

    fn slot(r: Ref) u32 {
        return r._r & 0xff;
    }
};

fn PageType(comptime Kind: type, comptime Object: type) type {
    return struct {
        const Page = @This();

        len: usize,
        index: u32,
        kinds: [256]Kind,
        data: [256]Object,
        colors: [256]Color,
        used: std.StaticBitSet(256),

        pub fn create(alloc: std.mem.Allocator) !*Page {
            var page = try alloc.create(Page);
            page.len = 0;
            page.used = std.StaticBitSet(256).initEmpty();
            return page;
        }
    };
}

const Color = enum {
    black,
    grey,
    white,
};

fn GCType(comptime Kind: type, comptime Object: type) type {
    return struct {
        const GC = @This();

        const Page = PageType(Kind, Object);
        const Entry = struct { kind: Kind, data: Object };

        alloc: std.mem.Allocator,
        rng: ?*std.Random.DefaultPrng,
        rand: std.Random.Random,
        active_page: ?*Page,
        shuffle: [256]u8,
        inactive_pages: std.ArrayList(*Page),

        // in theory we could encode all these in page_table without extra memory
        page_table: std.ArrayList(*Page),
        reusable: std.ArrayList(u32),

        page_counter: u32,

        fn init(alloc: std.mem.Allocator) GC {
            var gc: GC = undefined;
            gc.alloc = alloc;
            gc.rng = null;
            gc.active_page = null;
            gc.inactive_pages = std.ArrayList(*Page).init(alloc);
            gc.page_table = std.ArrayList(*Page).init(alloc);
            gc.reusable = std.ArrayList(u32).init(alloc);
            gc.page_counter = 0;
            return gc;
        }

        fn deinit(gc: *GC) void {
            if (gc.rng) |rng| gc.alloc.destroy(rng);
            if (gc.active_page) |active_page| gc.alloc.destroy(active_page);
            for (gc.inactive_pages.items) |page| gc.alloc.destroy(page);
            gc.inactive_pages.deinit();
            gc.page_table.deinit();
            gc.reusable.deinit();
            gc.* = undefined;
        }

        fn setup(gc: *GC) !void {
            // first time setup
            // i'd just rather that init cannot fail
            // so invoke this on first use instead
            // i wonder how much perf impact this pattern really has
            // like branch predictor will predict it basically 100%
            // but it is still a comparison?
            gc.rng = try gc.alloc.create(std.Random.DefaultPrng);
            errdefer {
                gc.alloc.destroy(gc.rng.?);
                gc.rng = null;
            }
            gc.rng.?.* = std.Random.DefaultPrng.init(
                @bitCast(std.time.microTimestamp() *% 7951182790392048631),
            );
            gc.rand = gc.rng.?.random();
            try gc.newActive();
        }

        fn newActive(gc: *GC) !void {
            if (gc.reusable.items.len == 0) {
                // NOTE, we make sure we have enough space to reuse everything
                try gc.page_table.ensureTotalCapacity(gc.page_table.items.len + 1);
                try gc.reusable.ensureTotalCapacity(gc.page_counter + 1);
            }

            const new_page = try Page.create(gc.alloc);
            errdefer gc.alloc.destroy(new_page);

            if (gc.active_page) |active_page| {
                try gc.inactive_pages.ensureTotalCapacity(gc.inactive_pages.items.len + 1);
                gc.inactive_pages.appendAssumeCapacity(active_page);
                gc.active_page = null;
            }
            gc.active_page = new_page;

            if (gc.reusable.items.len > 0) {
                const page_index = gc.reusable.pop();
                gc.page_table.items[page_index] = new_page;
                gc.active_page.?.index = page_index;
            } else {
                const page_index = gc.page_counter;
                gc.page_counter += 1;
                std.debug.assert(gc.page_table.items.len == page_index);
                gc.page_table.appendAssumeCapacity(new_page);
                gc.active_page.?.index = page_index;
            }

            for (0..256) |i| gc.shuffle[i] = @intCast(i);
            for (0..256) |i| {
                const j = gc.rand.intRangeLessThan(usize, i, 256);
                const tmp = gc.shuffle[i];
                gc.shuffle[i] = gc.shuffle[j];
                gc.shuffle[j] = tmp;
            }
        }

        fn create(gc: *GC, kind: Kind, object: Object) !Ref {
            if (gc.rng == null) try gc.setup();
            if (gc.active_page.?.len == 256) try gc.newActive();

            const active_page = gc.active_page.?;
            const i = gc.shuffle[active_page.len];
            active_page.kinds[i] = kind;
            active_page.data[i] = object;
            active_page.colors[i] = .grey;
            active_page.used.set(i);
            active_page.len += 1;

            return Ref.new(active_page.index, i);
        }

        fn get(gc: *GC, ref: Ref) Entry {
            const page = gc.page_table.items[ref.page()];
            const slot = ref.slot();
            return .{
                .kind = page.kinds[slot],
                .data = page.data[slot],
            };
        }

        fn trace(gc: *GC, ref: Ref) void {
            const page = gc.page_table.items[ref.page()];
            const slot = ref.slot();
            page.colors[slot] = .black;
            page.data[slot].trace(page.kinds[slot], gc);
        }

        fn sweep(gc: *GC) void {
            for (gc.inactive_pages.items) |page| {
                var it = page.used.iterator(.{});
                // scan all entries on page and free if unused
                while (it.next()) |i| {
                    if (page.colors[i] != .white) continue;
                    page.used.unset(i);
                    page.data[i].finalize(page.kinds[i], gc);
                    page.len -= 1;
                }
            }

            {
                // is there a way to do a for (0..) but backwards?
                // i just don't like needing the extra block to scope the iteration var
                var i = gc.inactive_pages.items.len;
                while (i > 0) : (i -= 1) {
                    if (gc.inactive_pages.items[i - 1].len > 0) continue;
                    std.debug.print("DESTROYING A PAGE AND FREEING SLOTS\n", .{});
                    // destroying a page means any index in the page_table referencing it
                    // can now be safely reused
                    for (gc.page_table.items, 0..) |page, j| {
                        if (page != gc.inactive_pages.items[i - 1]) continue;
                        gc.reusable.appendAssumeCapacity(@intCast(j));
                        gc.page_table.items[j] = @ptrFromInt(
                            ~@as(usize, 7),
                        ); // ugly hack to set the pointer to an adress that definitely isn't used
                        // (i hope), probably should just allow null in rewrite
                    }
                    gc.alloc.destroy(gc.inactive_pages.swapRemove(i - 1));
                }
                // so, actually gc-ing the pages too (coloring slots in the page_table if traced)
                // is better at reusing previous slots. like this, if merging is good enough
                // we might eventually run out.
            }

            // prepare for next round of sweeping
            for (gc.inactive_pages.items) |page| {
                var it = page.used.iterator(.{});
                while (it.next()) |i| {
                    page.colors[i] = .white;
                }
            }
        }

        fn canMerge(a: *Page, b: *Page) bool {
            return a.used.intersectWith(b.used).findFirstSet() == null;
        }

        fn mergeInto(src: *Page, dst: *Page) void {
            std.debug.assert(canMerge(src, dst));
            // copy data
            var it = src.used.iterator(.{});
            while (it.next()) |i| {
                std.debug.assert(!dst.used.isSet(i));
                dst.data[i] = src.data[i];
                dst.kinds[i] = src.kinds[i];
                dst.colors[i] = src.colors[i];
                dst.used.set(i);
                dst.len += 1;
            }
        }

        fn compact(gc: *GC) void {
            var list1 = std.ArrayList(*Page).initCapacity(
                gc.alloc,
                gc.inactive_pages.items.len,
            ) catch @panic("oom"); // should go into arena or somethign
            var list2 = std.ArrayList(*Page).initCapacity(
                gc.alloc,
                gc.inactive_pages.items.len,
            ) catch @panic("oom"); // should go into arena or somethign
            defer list1.deinit();
            defer list2.deinit();
            // only try to merge pages that are fairly empty
            for (gc.inactive_pages.items) |page| {
                if (page.len > 64) continue;
                if (gc.rand.boolean()) {
                    list1.appendAssumeCapacity(page);
                } else {
                    list2.appendAssumeCapacity(page);
                }
            }
            std.debug.print("{} {}\n", .{ list1.items.len, list2.items.len });

            for (0..@min(list1.items.len, list2.items.len)) |i| {
                if (!canMerge(list1.items[i], list2.items[i])) continue;
                std.debug.print("merging {} {}\n", .{ list1.items[i].len, list2.items[i].len });
                mergeInto(list1.items[i], list2.items[i]);
                // redirect all entries pointing to the page we just merged
                for (gc.page_table.items) |*pt| {
                    if (pt.* != list1.items[i]) continue;
                    pt.* = list2.items[i];
                }
                // and destroy it in the list of inactive_pages
                var j: usize = 0;
                while (gc.inactive_pages.items[j] != list1.items[i]) : (j += 1) {}
                gc.alloc.destroy(gc.inactive_pages.swapRemove(j));
            }
        }
    };
}

const ScratchKind = enum {
    real,
    hamt,
};
const ScratchObject = union {
    real: f64,
    hamt: [8]Ref,

    fn trace(
        data: ScratchObject,
        kind: ScratchKind,
        gc: *GCType(ScratchKind, ScratchObject),
    ) void {
        switch (kind) {
            .real => {},
            .hamt => {
                for (data.hamt) |child| {
                    gc.trace(child);
                }
            },
        }
    }

    fn finalize(
        data: ScratchObject,
        kind: ScratchKind,
        gc: *GCType(ScratchKind, ScratchObject),
    ) void {
        // we could allow for special types with finalizers
        // like if we wanted to hold an array or string, allocated on the normal heap
        _ = kind;
        _ = data;
        _ = gc;
    }
};

test "scratch" {
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp() *% 3129313031303131));
    const rand = rng.random();

    var gc = GCType(ScratchKind, ScratchObject).init(std.testing.allocator);
    defer gc.deinit();

    var a = std.ArrayList(Ref).init(std.testing.allocator);
    defer a.deinit();

    std.debug.print("{}\n", .{@sizeOf(GCType(ScratchKind, ScratchObject).Page)});

    var w: f64 = 0.0;

    for (0..1000) |_| {
        while (a.items.len < 1024) {
            const x = try gc.create(.real, .{ .real = w });
            w += 1.0;
            const e = gc.get(x);
            _ = e;
            // std.debug.print("{}\t{}\t{}\n", .{ x, e.kind, e.data.real });
            try a.append(x);
        }

        while (a.items.len > 512) {
            const i = rand.uintLessThan(usize, a.items.len);
            _ = a.swapRemove(i);
        }
        for (a.items) |x| {
            gc.trace(x);
        }

        // std.debug.print("reusable before {any}\n", .{gc.reusable.items});
        gc.sweep();
        gc.compact();
        std.debug.print(
            "current page count {}\tcurrent table size {}\n",
            .{ gc.inactive_pages.items.len + 1, gc.page_counter - gc.reusable.items.len },
        );
        // std.debug.print("reusable after {any}\n", .{gc.reusable.items});
    }
}
