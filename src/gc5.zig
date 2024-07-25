const std = @import("std");

const BlockPool = @import("blockpool.zig").BlockPool(.{});

pub const Kind = enum(u8) {
    real,
    cons,
    hamt,
};

pub fn ValueType(comptime kind: Kind) type {
    return switch (kind) {
        .real => f64,
        .cons => [2]?*Object,
        .hamt => [16]?*Object,
    };
}

const Object = extern struct {
    fwd: *Object,
    hash: u32,
    next: u16,
    kind: Kind,

    pub fn value(head: *Object, comptime kind: Kind) ValueType(kind) {
        std.debug.assert(head.kind == kind);
        const ptr: *ObjectType(kind) = @ptrCast(head);
        return ptr.data;
    }

    fn page(obj: *Object) *Page {
        return @ptrFromInt(@intFromPtr(obj) & ~@as(usize, BlockPool.BLOCK_SIZE - 1));
    }
};

comptime {
    std.debug.assert(@sizeOf(Object) <= 16);
}

pub fn ObjectType(comptime kind: Kind) type {
    return extern struct {
        head: Object,
        data: ValueType(kind),
    };
}

const Page = struct {
    const Header = struct {
        next: ?*Page,
        n: usize,
    };
    const L = std.math.ceilPowerOfTwo(comptime_int, BlockPool.BLOCK_SIZE / 32);
    const N = BlockPool.BLOCK_SIZE - @sizeOf(Header) - @sizeOf(u32);

    head: Header,
    marks: u32,
    data: [N]u8,

    fn mark(page: *Page, obj: *Object) void {
        const x = @intFromPtr(obj) - @intFromPtr(page);
        page.marks |= @as(u32, 1) << (x / L);
    }
};

comptime {
    std.debug.assert(@sizeOf(Page) <= BlockPool.BLOCK_SIZE);
}

pub const GC = struct {
    pool: *BlockPool,
    eden: ?*Page = null,

    pub fn init(pool: *BlockPool) GC {
        return GC{
            .pool = pool,
        };
    }

    pub fn deinit(sgc: *GC) void {
        var walk: ?*Page = sgc.eden;
        while (walk) |p| {
            walk = p.next;
            sgc.pool.destroy(p);
        }
        sgc.* = undefined;
    }

    pub fn newEden(gc: *GC) void {
        const new_eden = Page.create(gc.pool);
        new_eden.next = gc.eden;
        gc.eden = new_eden;
    }

    pub fn begin(gc: *GC) void {
        _ = gc;
        // select the pages that are to be evacuated
        // by moving them to the from-space and marking them
    }

    pub fn trace1(gc: *GC, root: *Object) void {
        _ = gc;
        _ = root;
        // traverse and evacuate any objects in the from space
        // by building a replica in to-space and forwarding usign the brooks ptr
    }

    pub fn trace2(gc: *GC, root: *Object) void {
        _ = gc;
        _ = root;
        // traverse and update any references to from-space objects
    }

    pub fn end(gc: *GC) void {
        // destroy from-space pages calling finalizers
        _ = gc;
    }
};

test "yo" {
    var pool = try BlockPool.init(std.testing.allocator);
    defer pool.deinit();

    var sg = GC.init(&pool);
    defer sg.deinit();
}
