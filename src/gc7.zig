const std = @import("std");

const BlockPool = @import("blockpool.zig").BlockPool(.{});

pub const NILHASH: u64 = 0xb4b4b4b4b4b4b4b4;

pub const Kind = enum(u8) {
    real,
    cons,
    hamt,
    string,
};

pub const Object = extern struct {
    // 54 bits of hash, 1 bit marking if unfinished, 1 unused bit, 8 bits of kind
    // if (a._property != b._property) a and b cannot be equal
    _property: u64 align(16),
    fwd: *Object,

    pub fn setHash(obj: *Object, hash: u64) void {
        // NOTE we could opt to use different bits of a 64 bit hash depending on quality?
        obj._property = (obj._property & @as(u64, 0b11_1111_1111)) | (hash << 10);
    }
    pub fn getHash(obj: *Object) u64 {
        return obj._property >> 10;
    }
    pub fn getHashAtDepth(obj: *Object, depth: usize) u64 {
        std.debug.assert(depth < 9); // TODO generate infinite bits on demand
        const h = obj.getHash();
        return (h >> @intCast(depth * 6)) & 0b11_1111;
    }

    pub fn setKind(obj: *Object, comptime kind: Kind) void {
        obj._property = (obj._property & ~@as(u64, 0b1111_1111)) | @intFromEnum(kind);
    }
    pub fn getKind(obj: *Object) Kind {
        return @enumFromInt(obj._property & @as(u64, 255));
    }

    pub fn markUnfinished(obj: *Object) void {
        obj._property |= @as(u64, 0b10_0000_0000);
    }
    pub fn markFinished(obj: *Object) void {
        obj._property &= ~@as(u64, 0b10_0000_0000);
    }
    pub fn isFinished(obj: *Object) bool {
        return obj._property & @as(u64, 0b10_0000_0000) == 0;
    }

    pub fn as(obj: *Object, comptime kind: Kind) *ObjectType(kind) {
        std.debug.assert(obj.getKind() == kind);
        return @alignCast(@ptrCast(obj));
    }

    pub fn page(obj: *Object) *Page {
        // this works because BlockPool provides memory with BLOCK_SIZE alignment
        const mask = ~@as(usize, BlockPool.BLOCK_SIZE - 1);
        return @ptrFromInt(@intFromPtr(obj) & mask);
    }
};

comptime {
    std.debug.assert(@alignOf(Object) == 16);
    std.debug.assert(@sizeOf(Object) == 16);
}

pub const ObjectReal = extern struct {
    head: Object,
    data: f64,

    pub fn hash(real: *ObjectReal) void {
        real.head.setHash(std.hash.XxHash3.hash(
            15345951513627307427, // just a large prime
            std.mem.asBytes(&real.data),
        ));
    }

    pub fn size() usize {
        return std.mem.alignForwardLog2(@sizeOf(ObjectReal), 4);
    }
};

pub const ObjectCons = extern struct {
    head: Object,
    car: ?*Object,
    cdr: ?*Object,

    pub fn hash(cons: *ObjectCons) void {
        var h: u64 = 0;
        h ^= if (cons.car) |car| car.getHash() else NILHASH;
        h ^= if (cons.cdr) |cdr| cdr.getHash() else NILHASH;
        cons.head.setHash(h);
    }

    pub fn size() usize {
        return std.mem.alignForwardLog2(@sizeOf(ObjectCons), 4);
    }
};

pub const ObjectHamt = extern struct {
    head: Object,
    mask: u64,

    pub fn hash(hamt: *ObjectHamt) void {
        var h: u64 = 0;
        const children = hamt.data();
        for (children[0..hamt.len()]) |child| {
            h ^= child.getHash();
        }
    }

    pub fn data(hamt: *ObjectHamt) [*]*Object {
        // NOTE children of hamt are always hamt or cons
        return @ptrFromInt(@intFromPtr(&hamt.mask) + 8);
    }

    pub fn len(hamt: *ObjectHamt) usize {
        return @popCount(hamt.mask);
    }

    pub fn size(_len: usize) usize {
        return std.mem.alignForwardLog2(@sizeOf(ObjectHamt) + @sizeOf(usize) * _len, 4);
    }
};

pub const ObjectString = extern struct {
    head: Object,
    len: usize,

    pub fn hash(string: *ObjectString) void {
        string.head.setHash(std.hash.XxHash3.hash(
            12916058753096001697, // just a large prime
            string.data()[0..string.len],
        ));
    }

    pub fn data(string: *ObjectString) [*]u8 {
        return @ptrFromInt(@intFromPtr(&string.len) + 8);
    }

    pub fn size(len: usize) usize {
        return std.mem.alignForwardLog2(@sizeOf(ObjectString) + @sizeOf(u8) * len, 4);
    }
};

pub fn ObjectType(comptime kind: Kind) type {
    return switch (kind) {
        .real => ObjectReal,
        .cons => ObjectCons,
        .hamt => ObjectHamt,
        .string => ObjectString,
    };
}

pub fn objectSize(comptime kind: Kind, len: usize) usize {
    if (kind != .string) std.debug.assert(len == 0);
    return @sizeOf(ObjectType(kind)) + switch (kind) {
        .real, .cons => 0,
        .hamt => len * 8,
        .string => len,
    };
}

const Page = struct {
    next: ?*@This(),
    offset: usize,
    mark: bool, // should this page be evacuated?
    data: [BlockPool.BLOCK_SIZE - 32]u8 align(16),

    fn create(pool: *BlockPool) !*@This() {
        const page = try pool.create(@This());
        page.offset = 0;
        page.next = null;
        page.mark = false;
        return page;
    }

    /// for types that aren't varsized, len should be 0
    /// for .hamt it should be the number of children (i.e. @popCount(mask))
    /// for .string it should be the number of bytes required to store it
    fn alloc(page: *Page, comptime kind: Kind, len: usize) ?*ObjectType(kind) {
        switch (kind) {
            .real, .cons => std.debug.assert(len == 0),
            .hamt => std.debug.assert(len <= 64),
            .string => {},
        }

        const addr = page.offset;
        var offset = page.offset;
        offset += switch (kind) {
            .real, .cons => ObjectType(kind).size(),
            .hamt, .string => ObjectType(kind).size(len),
        };
        std.debug.assert(std.mem.isAlignedLog2(offset, 4)); // size() is always multiple of 16

        if (offset <= page.data.len) {
            const obj: *ObjectType(kind) = @alignCast(@ptrCast(&page.data[addr]));
            obj.head.markUnfinished();
            obj.head.setKind(kind);
            obj.head.fwd = @ptrCast(obj);
            page.offset = offset;
            return obj;
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

    /// performs the allocation, but doesn't compute the hash
    /// marks the object as unfinished
    /// do not allow the alloced object to escape before calling commit
    /// but, updates are allowed before commit
    /// for types that aren't varsized, len should be 0
    /// for .hamt it should be the number of children (i.e. @popCount(mask))
    /// for .string it should be the number of bytes required to store it
    pub fn alloc(gc: *GC, comptime kind: Kind, len: usize) !*ObjectType(kind) {
        if (gc.live == null) try gc.newPage();
        var result = gc.live.?.alloc(kind, len);
        if (result) |r| return r;
        try gc.newPage(); // page couldn't fit our object, make a new one
        result = gc.live.?.alloc(kind, len);
        if (result) |r| {
            return r;
        } else {
            std.log.err("Allocation of {}, len={} does not fit in a memory block", .{ kind, len });
            return error.OversizedAllocation;
        }
    }

    /// computes the hash, and marks the object as usable
    pub fn commit(gc: *GC, comptime kind: Kind, obj: *ObjectType(kind)) *Object {
        // i guess we don't really need this to be a gc function?
        // however, seems kinda nice just for consistency?
        _ = gc;
        std.debug.assert(!obj.head.isFinished());
        std.debug.assert(obj.head.getKind() == kind);
        obj.hash();
        obj.head.markFinished();
        return @ptrCast(obj);
    }

    /// create a copy of an object
    pub fn dup(gc: *GC, comptime kind: Kind, _old: *Object) !*Object {
        std.debug.assert(_old.isFinished()); // disallow functions on unfinished allocations
        const old = _old.as(kind);
        const new = switch (kind) {
            .real, .cons => try gc.alloc(kind, 0),
            .hamt => try gc.alloc(kind, old.len()),
            .string => try gc.alloc(kind, old.len),
        };
        switch (kind) {
            .real => new.data = old.data,
            .cons => {
                new.car = old.car;
                new.cdr = old.cdr;
            },
            .hamt => {
                new.mask = old.mask;
                @memcpy(new.data(), old.data()[0..old.len()]);
            },
            .string => {
                new.len = old.len;
                @memcpy(new.data(), old.data()[0..old.len]);
            },
        }
        new.head._property = old.head._property;
        return new;
    }

    fn newPage(gc: *GC) !void {
        // full pages go into the eden space and are always compacted
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

        for (gc.roots[0..gc.n_roots]) |root| try gc.trace(root);
        gc.n_roots = 0;

        walk = gc.discard;
        while (walk) |p| {
            walk = p.next;
            gc.pool.destroy(p);
        }
    }

    fn trace(gc: *GC, ptr: ?*Object) !void {
        const obj = ptr orelse return;
        // NOTE for concurrency, we should have a separate allocation page for this
        // (really, a separate allocatioon page for each thread)
        // and use atomics for updating the pointers (?)
        // first, try to update any references using the forwarding pointer
        switch (obj.getKind()) {
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
                } // cant make this a oneliner, compiler bug?
            },
        }
        // now, replicate if we are on a marked page
        // and we haven't already been replicated
        if (obj == obj.fwd and obj.page().mark) {
            const r = switch (obj.kind) {
                .real => try gc.dup(.real, obj),
                .cons => try gc.dup(.cons, obj),
                .hamt => try gc.dup(.hamt, obj),
                .string => try gc.dup(.string, obj),
            };
            obj.fwd = r;
        }

        // finally, keep tracing
        switch (obj.kind) {
            .real, .string => {},
            .cons => {
                const cons = obj.as(.cons);
                // note, trace cdr first s.t. linked lists are sequential
                try gc.trace(cons.cdr);
                try gc.trace(cons.car);
            },
            .hamt => {
                const hamt = obj.as(.hamt);
                for (0..hamt.len()) |i| try gc.trace(hamt.data()[i]);
            },
        }
    }
};
