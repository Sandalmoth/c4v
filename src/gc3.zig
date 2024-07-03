const std = @import("std");

const Kind = enum(u8) {
    real,
    cons,
};

fn KindType(comptime kind: Kind) type {
    return switch (kind) {
        .real => f64,
        .cons => [2]Ref,
    };
}

const Ref = extern struct {
    page: u32 align(8),
    slot: u8,
    kind: Kind,
};

comptime {
    std.debug.assert(@sizeOf(Ref) == 8);
    std.debug.assert(@alignOf(Ref) == 8);
}

fn SingularGCType(comptime kind: Kind, comptime T: type) type {
    return struct {
        const SGC = @This();

        // we could/should tune the size s.t. all the SGCs can use the same pool of pages
        const Page = struct {
            index: u32, // index of this page in page table
            len: usize, // number of items in table
            data: [256]T,
            marks: std.StaticBitSet(256),
            used: std.StaticBitSet(256),

            pub fn create(alloc: std.mem.Allocator) !*Page {
                var page = try alloc.create(Page);
                page.len = 0;
                page.used = std.StaticBitSet(256).initEmpty();
                return page;
            }
        };

        alloc: std.mem.Allocator,
        rand: std.Random.Random,
        shuffle: [256]u8,
        active_page: ?*Page,
        inactive_pages: std.ArrayList(*Page),

        page_counter: u32,
        page_table: std.ArrayList(?*Page),
        page_marks: std.ArrayList(bool),
        reusable: std.ArrayList(u32),

        fn init(alloc: std.mem.Allocator, rand: std.Random.Random) SGC {
            return .{
                .alloc = alloc,
                .rand = rand,
                .shuffle = undefined,
                .active_page = null,
                .inactive_pages = std.ArrayList(*Page).init(alloc),
                .page_counter = 0,
                .page_table = std.ArrayList(?*Page).init(alloc),
                .page_marks = std.ArrayList(bool).init(alloc),
                .reusable = std.ArrayList(u32).init(alloc),
            };
            // NOTE we have to alloc in the big gc alloc, so avoiding allocs here is not needed
        }

        fn deinit(sgc: *SGC) void {
            if (sgc.active_page) |active_page| sgc.alloc.destroy(active_page);
            for (sgc.inactive_pages.items) |page| sgc.alloc.destroy(page);
            sgc.inactive_pages.deinit();
            sgc.page_table.deinit();
            sgc.page_marks.deinit();
            sgc.reusable.deinit();
            sgc.* = undefined;
        }

        fn newActivePage(sgc: *SGC) !void {
            // first create a new active page
            if (sgc.reusable.items.len == 0) {
                try sgc.page_table.ensureTotalCapacity(sgc.page_table.items.len + 1);
                try sgc.page_marks.ensureTotalCapacity(sgc.page_marks.items.len + 1);
                try sgc.reusable.ensureTotalCapacity(sgc.page_counter + 1);
            }

            const new_page = try Page.create(sgc.alloc);
            errdefer sgc.alloc.destroy(new_page);

            if (sgc.active_page) |active_page| {
                try sgc.inactive_pages.ensureTotalCapacity(sgc.inactive_pages.items.len + 1);
                sgc.inactive_pages.appendAssumeCapacity(active_page);
            }
            sgc.active_page = new_page;

            // now, add that new active page in the page table
            var page_index: u32 = undefined;
            if (sgc.reusable.items.len > 0) {
                page_index = sgc.reusable.pop();
                sgc.page_table.items[page_index] = new_page;
                sgc.page_marks.items[page_index] = true;
            } else {
                page_index = sgc.page_counter;
                sgc.page_counter += 1;
                sgc.page_table.appendAssumeCapacity(new_page);
                sgc.page_marks.appendAssumeCapacity(true);
            }
            sgc.active_page.?.index = page_index;

            for (0..256) |i| sgc.shuffle[i] = @intCast(i);
            for (0..256) |i| {
                const j = sgc.rand.intRangeLessThan(usize, i, 256);
                const tmp = sgc.shuffle[i];
                sgc.shuffle[i] = sgc.shuffle[j];
                sgc.shuffle[j] = tmp;
            }
        }

        fn create(sgc: *SGC, val: T) !Ref {
            if (sgc.active_page == null or sgc.active_page.?.len == 256) try sgc.newActivePage();

            const active_page = sgc.active_page.?;
            const i = sgc.shuffle[active_page.len];
            active_page.data[i] = val;
            active_page.marks.set(i);
            active_page.used.set(i);
            active_page.len += 1;

            return .{
                .page = active_page.index,
                .kind = kind,
                .slot = i,
            };
        }

        fn get(sgc: *SGC, ref: Ref) T {
            std.debug.assert(sgc.page_table.items[ref.page] != null);
            const page = sgc.page_table.items[ref.page].?;
            std.debug.assert(page.used.isSet(ref.slot));
            return page.data[ref.slot];
        }

        fn trace(sgc: *SGC, ref: Ref, gc: *GC) void {
            sgc.page_table.items[ref.page].?.marks.set(ref.slot);
            sgc.page_marks.items[ref.page] = true;
            switch (kind) {
                .real => {},
                .cons => {
                    const cons = sgc.get(ref);
                    gc.trace(cons[0]);
                    gc.trace(cons[1]);
                },
            }
        }

        fn sweep(sgc: *SGC) void {
            {
                var i = sgc.inactive_pages.items.len;
                // for (sgc.inactive_pages.items) |page| {
                while (i > 0) : (i -= 1) {
                    const page = sgc.inactive_pages.items[i - 1];
                    const unmarked = page.used.differenceWith(page.marks);
                    var it = unmarked.iterator(.{});
                    while (it.next()) |j| {
                        page.used.unset(j);
                        // finalizeAny(sgc, page.data[i], kind); we don't have the ref here huh...
                        page.len -= 1;
                    }

                    // if (page.len > 0) continue;
                    // std.debug.assert(!sgc.page_marks.items[page.index]);
                    // sgc.alloc.destroy(sgc.inactive_pages.swapRemove(i - 1));
                }
            }

            {
                // destroy any pages that no references were traced through
                std.debug.assert(sgc.page_table.items.len == sgc.page_marks.items.len);
                var i = sgc.page_marks.items.len;
                while (i > 0) : (i -= 1) {
                    if (sgc.page_marks.items[i - 1]) continue;
                    if (sgc.page_table.items[i - 1] == null) continue;
                    // NOTE we shouldn't need to call finalizers, since that's already been done
                    sgc.reusable.appendAssumeCapacity(@intCast(i - 1));
                    sgc.page_table.items[i - 1] = null;
                    std.debug.print("FREED A PAGE\n", .{});
                }
            }

            // prepare for next round of sweeping
            for (sgc.inactive_pages.items) |page| {
                page.marks = std.StaticBitSet(256).initEmpty();
            }
            for (sgc.page_marks.items) |*mark| {
                mark.* = false;
            }

            std.debug.print("FINISHED SWEEP\n", .{});
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
                dst.used.set(i);
                dst.len += 1;
            }
        }

        fn compact(sgc: *SGC) void {
            var list1 = std.ArrayList(*Page).initCapacity(
                sgc.alloc,
                sgc.inactive_pages.items.len,
            ) catch @panic("oom"); // should go into arena or somethign
            var list2 = std.ArrayList(*Page).initCapacity(
                sgc.alloc,
                sgc.inactive_pages.items.len,
            ) catch @panic("oom"); // should go into arena or somethign
            defer list1.deinit();
            defer list2.deinit();
            // only try to merge pages that are fairly empty
            for (sgc.inactive_pages.items) |page| {
                if (page.len > 64) continue;
                if (sgc.rand.boolean()) {
                    list1.appendAssumeCapacity(page);
                } else {
                    list2.appendAssumeCapacity(page);
                }
            }
            std.debug.print("{} {}\n", .{ list1.items.len, list2.items.len });

            for (0..100) |offset| {

                // does the @max evaluate every iteration or only once?
                // not that it really matters
                for (0..@max(list1.items.len, list2.items.len)) |k| {
                    if (list1.items.len == 0 or list2.items.len == 0) return;
                    const i = k % list1.items.len;
                    const j = (k + offset) % list2.items.len;
                    if (!canMerge(list1.items[i], list2.items[j])) continue;

                    std.debug.print("merge {} {}\n", .{ list1.items[i].len, list2.items[j].len });
                    mergeInto(list1.items[i], list2.items[j]);
                    // redirect all entries pointing to the page we just merged
                    for (sgc.page_table.items) |*pt| {
                        if (pt.* != list1.items[i]) continue;
                        pt.* = list2.items[j];
                    }
                    // destroy it in the list of inactive_pages
                    var ixd: usize = 0;
                    while (sgc.inactive_pages.items[ixd] != list1.items[i]) : (ixd += 1) {}
                    sgc.alloc.destroy(sgc.inactive_pages.swapRemove(ixd));
                    // and drop it from considereation in future merges
                    _ = list1.swapRemove(i);
                }

                // to ensure that we can merge for as long as possible
                // swap list1 and list2 to swap merge target
                const tmp = list1;
                list1 = list2;
                list2 = tmp;
            }
        }
    };
}

// probably rewrite s.t. it takes the general gc instead?
// fn traceAny(gc: *GC, ref: Ref, comptime kind: Kind) void {
//     switch (kind) {
//         .real => {},
//         .cons => |cons| {
//             const cons =
//                 gc.trace(cons[0]);
//             gc.trace(cons[1]);
//         },
//     }
// }

// fn finalizeAny(sgc: *anyopaque, ref: Ref, comptime kind: Kind) void {
//     _ = sgc;
//     _ = ref;
//     switch (kind) {
//         .real => {},
//         .cons => {},
//     }
// }

// test "sgc scratch" {
//     var rng = std.Random.DefaultPrng.init(
//         @bitCast(std.time.microTimestamp() *% 7951182790392048631),
//     );
//     const rand = rng.random();

//     const SGC = SingularGCType(.real, f64);
//     var sgc = SGC.init(std.testing.allocator, rand);
//     defer sgc.deinit();

//     var a = std.ArrayList(Ref).init(std.testing.allocator);
//     defer a.deinit();

//     var w: f64 = 0.0;
//     for (0..1000) |_| {
//         while (a.items.len < 1024) {
//             const x = try sgc.create(w);
//             w += 1.0;
//             const e = sgc.get(x);
//             _ = e;
//             try a.append(x);
//         }

//         while (a.items.len > 512) {
//             const i = rand.uintLessThan(usize, a.items.len);
//             _ = a.swapRemove(i);
//         }
//         for (a.items) |x| {
//             sgc.trace(x, @ptrFromInt(8)); // dummy, never dereferenced in this test
//         }

//         sgc.sweep();
//         sgc.compact();
//         std.debug.print(
//             "current page count {}\tcurrent table size {}\n",
//             .{ sgc.inactive_pages.items.len + 1, sgc.page_counter - sgc.reusable.items.len },
//         );
//     }
// }

const GC = struct {
    const n_kinds = std.meta.fields(Kind).len;

    alloc: std.mem.Allocator,
    sgcs: [n_kinds]*anyopaque,

    fn init(alloc: std.mem.Allocator, rand: std.Random.Random) !GC {
        var gc = GC{
            .alloc = alloc,
            .sgcs = undefined,
        };
        // these loops are a bit inelegant
        // could we iterate over the enum directly?
        // iterating over std.meta.fields(Kind) doesn't seem to work
        inline for (0..n_kinds) |i| {
            const kind: Kind = @enumFromInt(i);
            const SGC = SingularGCType(kind, KindType(kind));
            const sgc = try alloc.create(SGC);
            sgc.* = SingularGCType(kind, KindType(kind)).init(alloc, rand);
            gc.sgcs[i] = sgc;
        }
        return gc;
    }

    fn deinit(gc: *GC) void {
        inline for (0..n_kinds) |i| {
            const kind: Kind = @enumFromInt(i);
            const sgc = gc.gets(kind);
            sgc.deinit();
            gc.alloc.destroy(sgc);
        }
        gc.* = undefined;
    }

    fn gets(gc: *GC, comptime kind: Kind) *SingularGCType(kind, KindType(kind)) {
        return @alignCast(@ptrCast(gc.sgcs[@intFromEnum(kind)]));
    }

    fn create(gc: *GC, comptime kind: Kind, val: KindType(kind)) !Ref {
        return gc.gets(kind).create(val);
    }

    fn trace(gc: *GC, ref: Ref) void {
        switch (ref.kind) {
            .real => {
                gc.gets(.real).trace(ref, gc);
            },
            .cons => {
                gc.gets(.cons).trace(ref, gc);
            },
        }
    }

    fn sweep(gc: *GC) void {
        inline for (0..n_kinds) |i| {
            gc.gets(@enumFromInt(i)).sweep();
        }
    }

    fn compact(gc: *GC) void {
        inline for (0..n_kinds) |i| {
            gc.gets(@enumFromInt(i)).compact();
        }
    }
};

test "gc scratch" {
    var rng = std.Random.DefaultPrng.init(
        @bitCast(std.time.microTimestamp() *% 7951182790392048631),
    );
    const rand = rng.random();

    var gc = try GC.init(std.testing.allocator, rand);
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
    }
}
