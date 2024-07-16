const std = @import("std");

const BlockPool = @import("blockpool.zig").BlockPool(.{});

const NILHASH: u32 = 0x00b4b4b4;

pub const Kind = enum(u8) {
    real,
    cons,
    hamt,
    string,
};

pub fn ValueType(comptime kind: Kind) type {
    return switch (kind) {
        .real => f64,
        .cons => [2]?*Object,
        .hamt => [16]?*Object,
        .string => []const u8,
    };
}

pub const Object = extern struct {
    fwd: *Object align(16),
    hash: u32,
    kind: Kind,

    pub fn as(obj: *Object, comptime kind: Kind) *ObjectType(kind) {
        std.debug.assert(obj.kind == kind);
        return @alignCast(@ptrCast(obj));
    }

    pub fn value(obj: *Object, comptime kind: Kind) ValueType(kind) {
        std.debug.assert(obj.kind == kind);
        const ptr = obj.as(kind);
        return switch (kind) {
            .real, .cons, .hamt => ptr.data,
            .string => ptr.data[0..ptr.len],
        };
    }

    pub fn page(obj: *Object) *Page {
        const mask = ~@as(usize, BlockPool.BLOCK_SIZE - 1);
        return @ptrFromInt(@intFromPtr(obj) & mask);
    }
};

comptime {
    std.debug.assert(@alignOf(Object) == 16);
    std.debug.assert(@sizeOf(Object) == 16);
}

pub fn ObjectType(comptime kind: Kind) type {
    return switch (kind) {
        .real, .cons, .hamt => extern struct {
            head: Object,
            data: ValueType(kind),
        },
        .string => extern struct {
            head: Object,
            len: usize,
            data: [*]u8,
        },
    };
}

pub fn objectSize(comptime kind: Kind, len: usize) usize {
    if (kind != .string) std.debug.assert(len == 0);
    return @sizeOf(ObjectType(kind)) + switch (kind) {
        .real, .cons, .hamt => 0,
        .string => len,
    };
}

const Page = struct {
    next: ?*@This(),
    offset: usize,
    mark: bool,
    data: [BlockPool.BLOCK_SIZE - 32]u8 align(16),

    fn create(pool: *BlockPool) !*@This() {
        const page = try pool.create(@This());
        page.offset = 0;
        page.next = null;
        page.mark = false;
        return page;
    }

    fn alloc(page: *Page, comptime kind: Kind, val: ValueType(kind)) ?*Object {
        const addr = page.offset;
        var offset = page.offset;

        offset += switch (kind) {
            .real, .cons, .hamt => objectSize(kind, 0),
            .string => objectSize(kind, val.len),
        };
        offset = std.mem.alignForwardLog2(offset, 4);

        if (offset <= page.data.len) {
            const obj: *ObjectType(kind) = @alignCast(@ptrCast(&page.data[addr]));
            obj.head.fwd = @ptrCast(obj);
            obj.head.hash = switch (kind) {
                .real => std.hash.XxHash32.hash(2590326161, std.mem.asBytes(&val)),
                .cons => blk: {
                    var h: u32 = 0;
                    h ^= if (val[0]) |c| c.hash else NILHASH;
                    h ^= if (val[1]) |c| c.hash else NILHASH;
                    break :blk h;
                },
                .hamt => blk: {
                    var h: u32 = 0;
                    for (val) |child| {
                        h ^= if (child) |c| c.hash else NILHASH;
                    }
                    break :blk h;
                },
                .string => std.hash.XxHash32.hash(2590326161, val),
            };
            obj.head.kind = kind;
            switch (kind) {
                .real, .cons, .hamt => {
                    obj.data = val;
                },
                .string => {
                    obj.len = val.len;
                    obj.data = @ptrFromInt(@intFromPtr(&obj.data) + @sizeOf(usize));
                    @memcpy(obj.data, val);
                },
            }
            page.offset = offset;
            return @alignCast(@ptrCast(&page.data[addr]));
        } else {
            return null;
        }
    }
};

comptime {
    std.debug.assert(@sizeOf(Page) <= BlockPool.BLOCK_SIZE);
}

pub const GC = struct {
    const MAX_ROOTS = BlockPool.BLOCK_SIZE / @sizeOf(*Object);

    pool: *BlockPool,
    n_roots: usize,
    roots: *[MAX_ROOTS]*Object,

    live: ?*Page = null,
    eden: ?*Page = null,
    from: ?*Page = null,
    to: ?*Page = null,
    discard: ?*Page = null,

    pub fn init(pool: *BlockPool) !GC {
        return GC{
            .pool = pool,
            .n_roots = 0,
            .roots = try pool.create([MAX_ROOTS]*Object),
        };
    }

    pub fn deinit(gc: *GC) void {
        if (gc.live) |p| {
            std.debug.assert(p.next == null);
            gc.pool.destroy(p);
        }
        var walk: ?*Page = gc.eden;
        while (walk) |p| {
            walk = p.next;
            gc.pool.destroy(p);
        }
        walk = gc.from;
        while (walk) |p| {
            walk = p.next;
            gc.pool.destroy(p);
        }
        walk = gc.to;
        while (walk) |p| {
            walk = p.next;
            gc.pool.destroy(p);
        }
        walk = gc.discard;
        while (walk) |p| {
            walk = p.next;
            gc.pool.destroy(p);
        }

        gc.pool.destroy(@ptrCast(gc.roots));
        gc.* = undefined;
    }

    pub fn create(gc: *GC, comptime kind: Kind, val: ValueType(kind)) !*Object {
        if (gc.live == null) try gc.newPage();
        var result = gc.live.?.alloc(kind, val);
        if (result) |r| return r;
        try gc.newPage();
        result = gc.live.?.alloc(kind, val);
        if (result) |r| {
            return r;
        } else {
            std.log.err("Allocation of {}: {any} does not fit in a memory block", .{ kind, val });
            return error.OversizedAllocation;
        }
    }

    pub fn traceRoot(gc: *GC, root: **Object) void {
        // STOP THE WORLD
        // update the given roots if they point to forwarded objects
        // add root to the root dataset so we can trace it later
        root.* = root.*.fwd;
        gc.roots[gc.n_roots] = root.*;
        gc.n_roots += 1;
    }

    pub fn collect(gc: *GC) !void {
        // concurrent
        // move from-space to discard-space
        // move eden-space to from-space
        // move some of to-space to from-space (dynamic probability?)
        // trace root dataset
        // - if forwarded, update the reference
        // - if on from-space page, replicate in to-space
        // clear root dataset
        // destroy discard-space

        gc.discard = gc.from;
        gc.from = gc.eden;
        gc.eden = null;
        // collect every page for now
        var walk = gc.to;
        while (walk) |p| {
            gc.to = p.next;
            p.next = gc.from;
            gc.from = p;
            p.mark = true;
            walk = gc.to;
        }

        for (gc.roots[0..gc.n_roots]) |root| try gc._trace(root);
        gc.n_roots = 0;

        walk = gc.discard;
        while (walk) |p| {
            walk = p.next;
            gc.pool.destroy(p);
        }
    }

    fn _trace(gc: *GC, ptr: ?*Object) !void {
        const obj = ptr orelse return;
        // NOTE for concurrency, we should have a separate allocation page for this
        // and use atomics for updating the pointers?
        // first, try to update any references using the forwarding pointer
        switch (obj.kind) {
            .real, .string => {},
            .cons => {
                const cons = obj.as(.cons);
                if (cons.data[0]) |car| cons.data[0] = car.fwd;
                if (cons.data[1]) |cdr| cons.data[1] = cdr.fwd;
            },
            .hamt => {
                const hamt = obj.as(.hamt);
                for (0..hamt.data.len) |i| {
                    if (hamt.data[i]) |child| hamt.data[i] = child.fwd;
                }
            },
        }
        // now, replicate if we are on a marked page
        if (obj.page().mark) {
            const r = switch (obj.kind) {
                .real => try gc.create(.real, obj.value(.real)),
                .cons => try gc.create(.cons, obj.value(.cons)),
                .hamt => try gc.create(.hamt, obj.value(.hamt)),
                .string => try gc.create(.string, obj.value(.string)),
            };
            obj.fwd = r;
        }

        // finally, keep tracing
        switch (obj.kind) {
            .real, .string => {},
            .cons => {
                const cons = obj.as(.cons);
                try gc._trace(cons.data[0]);
                try gc._trace(cons.data[1]);
            },
            .hamt => {
                const hamt = obj.as(.hamt);
                for (0..hamt.data.len) |i| {
                    try gc._trace(hamt.data[i]);
                }
            },
        }
    }

    fn newPage(gc: *GC) !void {
        if (gc.live != null) {
            gc.live.?.mark = true;
            gc.live.?.next = gc.eden;
            gc.eden = gc.live;
            gc.live = null;
        }

        const new_page = try Page.create(gc.pool);
        new_page.next = gc.live;
        gc.live = new_page;
    }
};

test "scratch" {
    var pool = try BlockPool.init(std.testing.allocator);
    defer pool.deinit();

    var gc = try GC.init(&pool);
    defer gc.deinit();

    var timer = try std.time.Timer.start();

    for (0..10000) |_| {
        const x = try gc.create(.real, 1.0);
        const y = try gc.create(.string, "hello world!");
        _ = x;
        _ = y;
        // std.debug.print("{} {s}\n", .{ x.value(.real), y.value(.string) });
    }

    std.debug.print("{}\n", .{timer.lap() / 10000});
}
