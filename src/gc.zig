const std = @import("std");

// test a gc idea roughly based on the mesh allocator
// https://arxiv.org/pdf/1902.04738

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

const Block = struct { bytes: [32]u8 align(32) };

const Page = struct {
    id: u32,
    n: usize,
    shuffle: [256]u8 align(64),
    data: [256]Block,
    used: std.StaticBitSet(256),
    marks: std.StaticBitSet(256),

    fn init(rand: std.rand.Random, id: u32) Page {
        var page: Page = undefined;
        page.id = id;
        page.n = 0;
        for (0..256) |i| page.shuffle[i] = @intCast(i);
        for (0..256) |i| {
            // FIXME actually do fisher-yates correctly...
            const j = rand.int(u8);
            const tmp = page.shuffle[i];
            page.shuffle[i] = page.shuffle[j];
            page.shuffle[j] = tmp;
        }
        page.used = std.StaticBitSet(256).initEmpty();
        page.marks = std.StaticBitSet(256).initEmpty();
        return page;
    }

    fn create(page: *Page) u32 {
        std.debug.assert(page.n < 256);
        const slot = page.shuffle[page.n];
        page.used.set(slot);
        page.n += 1;
        return slot;
    }

    fn destroy(page: *Page, rand: std.rand.Random, slot: u8) void {
        std.debug.assert(page.used.isSet(slot));
        page.n -= 1;
        page.used.unset(slot);
        const j: u8 = @intCast(rand.intRangeLessThan(usize, page.n, 256));
        page.shuffle[page.n] = page.shuffle[j];
        page.shuffle[j] = slot;
    }

    fn mark(page: *Page, slot: u32) void {
        std.debug.assert(slot < 256);
        page.marks.set(slot);
    }

    fn sweep(page: *Page, rand: std.rand.Random) void {
        const garbage = page.used.differenceWith(page.marks);
        var it = garbage.iterator(.{});
        while (it.next()) |i| {
            page.destroy(rand, @intCast(i));
        }
        page.marks = std.StaticBitSet(256).initEmpty();
    }
};

fn canMerge(a: Page, b: Page) bool {
    return a.used.intersectWith(b.used).findFirstSet() == null;
}

fn mergeInto(src: *Page, dst: *Page, rand: std.rand.Random) void {
    std.debug.assert(canMerge(src.*, dst.*));
    // copy data
    var it = src.used.iterator(.{});
    while (it.next()) |i| {
        std.debug.assert(!dst.used.isSet(i));
        dst.data[i] = src.data[i];
        dst.used.set(i);
        dst.n += 1;
    }
    // rebuild the shuffle vector in dst
    var it2 = dst.used.iterator(.{ .kind = .unset });
    var k: usize = 255;
    while (it2.next()) |w| {
        dst.shuffle[k] = @intCast(w);
        if (k == 0) {
            std.debug.assert(it2.next() == null);
            break;
        }
        k -= 1;
    }
    for (dst.n..256) |i| {
        // FIXME actually do fisher-yates correctly...
        const j = rand.intRangeLessThan(usize, i, 256);
        const tmp = dst.shuffle[i];
        dst.shuffle[i] = dst.shuffle[j];
        dst.shuffle[j] = tmp;
    }
}

const GC = struct {
    const Indirect = struct {
        page: *Page,
        moved: ?u32,
    };

    alloc: std.mem.Allocator,
    rand: std.rand.Random,
    pages: std.ArrayList(*Page),
    page_table: std.ArrayList(Indirect), // page_table.items[Ref.page()] -> *Page
    active_page: ?*Page = null,
    next_id: u32 = 0,

    fn init(alloc: std.mem.Allocator, rand: std.rand.Random) GC {
        return .{
            .alloc = alloc,
            .rand = rand,
            .pages = std.ArrayList(*Page).init(alloc),
            .page_table = std.ArrayList(Indirect).init(alloc),
        };
    }

    fn deinit(gc: *GC) void {
        for (gc.pages.items) |page| {
            gc.alloc.destroy(page);
        }
        gc.pages.deinit();
        gc.page_table.deinit();
    }

    fn create(gc: *GC) !Ref {
        // this is kinda stupid, obviously some kind of sorting would be preferable
        // maybe we could even keep the pages in a min-max heap by occupancy?
        if (gc.active_page != null and gc.active_page.?.n == 256 and gc.pages.items.len > 0) {
            // try another random page
            gc.active_page = gc.pages.items[gc.rand.uintLessThan(usize, gc.pages.items.len)];
        }

        if (gc.active_page == null or gc.active_page.?.n == 256) {
            try gc.pages.ensureTotalCapacity(gc.pages.items.len + 1);
            try gc.page_table.ensureTotalCapacity(gc.page_table.items.len + 1);
            const new_page = try gc.alloc.create(Page);
            new_page.* = Page.init(gc.rand, gc.next_id);
            gc.next_id += 1;
            gc.active_page = new_page;
            gc.pages.appendAssumeCapacity(new_page);
            gc.page_table.appendAssumeCapacity(.{ .page = new_page, .moved = null });
        }

        const active_page = gc.active_page.?;
        const slot = active_page.create();
        return Ref.new(active_page.id, slot);
    }

    fn mark(gc: *GC, r: Ref) void {
        const page = gc.getPage(r);
        page.mark(r.slot());
    }

    fn sweep(gc: *GC) void {
        for (gc.pages.items) |page| {
            page.sweep(gc.rand);
        }
    }

    fn compact(gc: *GC) void {
        if (gc.pages.items.len < 2) return;
        const n = gc.pages.items.len / 2;
        // almost surely a worse approach than the original, but whatever
        for (0..n) |i| {
            if (n == gc.pages.items.len) break;
            const a = gc.rand.intRangeLessThan(usize, n, gc.pages.items.len);
            const b = i;
            if (a == b) continue;
            if (!canMerge(gc.pages.items[a].*, gc.pages.items[b].*)) continue;

            mergeInto(gc.pages.items[a], gc.pages.items[b], gc.rand);
            gc.page_table.items[gc.pages.items[a].id].moved = gc.pages.items[b].id;
            gc.alloc.destroy(gc.pages.items[a]);
            _ = gc.pages.swapRemove(a);
        }
        gc.active_page = null;
    }

    fn getPage(gc: *GC, r: Ref) *Page {
        const indirect = &gc.page_table.items[r.page()];
        if (indirect.moved) |move| {
            const dest = gc.page_table.items[move];
            std.debug.assert(dest.moved == null);
            indirect.page = dest.page;
            indirect.moved = null;
        }
        return indirect.page;
    }
};

test "scratch" {
    // microtimestamp has pretty unevenly distributed entropy
    // multiplying by a large prime distributes it more evenly
    var rng = std.rand.DefaultPrng.init(@bitCast(std.time.microTimestamp() *% 3129313031303131));
    const rand = rng.random();

    var gc = GC.init(std.testing.allocator, rand);
    defer gc.deinit();

    var a = std.ArrayList(Ref).init(std.testing.allocator);
    defer a.deinit();

    for (0..10) |_| {
        for (0..1024) |_| {
            const x = try gc.create();
            try a.append(x);
        }

        for (0..a.items.len) |i| {
            const j = rand.uintLessThan(usize, a.items.len);
            const tmp = a.items[i];
            a.items[i] = a.items[j];
            a.items[j] = tmp;
        }
        while (a.items.len > 64) _ = a.pop();
        for (a.items) |x| gc.mark(x);

        gc.sweep();
        gc.compact();

        std.debug.print("number of pages is {}\n", .{gc.pages.items.len});
    }
}
