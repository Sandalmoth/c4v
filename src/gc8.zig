const std = @import("std");

const BlockPool = @import("blockpool.zig").BlockPool(.{});

pub const NILHASH: u64 = 0xD34D34D34D34D34;

pub const Kind = enum(u8) {
    real,
    cons,
    champ,
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

pub fn ObjectType(comptime kind: Kind) type {
    return switch (kind) {
        .real => ObjectReal,
        .cons => ObjectCons,
        .champ => ObjectChamp,
        .string => ObjectString,
    };
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

pub const ObjectChamp = extern struct {
    head: Object,
    datamask: u64,
    nodemask: u64,
    datalen: u32,
    nodelen: u32,

    pub fn hash(champ: *ObjectChamp) void {
        var h: u64 = 0;
        // NOTE entries in data have two pointers, a key and a value
        // and entries in in nodes have just one
        // however, the hash is simply the xor of all of the kv in the map
        // and since xor is commutative, we can simply xor everything together
        const children = champ.data();
        for (0..2 * champ.datalen + champ.nodelen) |i| {
            // while the nodes cannot be null, the data could be
            h ^= if (children[i]) |child| child.getHash() else NILHASH;
        }
        champ.head.setHash(h);
    }

    pub fn data(champ: *ObjectChamp) [*]?*Object {
        return @ptrFromInt(@intFromPtr(&champ.nodemask) + 16);
    }

    pub fn nodes(champ: *ObjectChamp) [*]*Object {
        return @ptrFromInt(@intFromPtr(&champ.nodemask) + 16 + 16 * champ.datalen);
    }

    pub fn size(_len: usize) usize {
        return std.mem.alignForwardLog2(
            @sizeOf(ObjectChamp) + @sizeOf(usize) * _len,
            4,
        );
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
    /// for .champ it should be the number of children (i.e. 2 * datalen + nodelen)
    /// for .string it should be the number of bytes required to store it
    fn alloc(page: *Page, comptime kind: Kind, len: usize) ?*ObjectType(kind) {
        switch (kind) {
            .real, .cons => std.debug.assert(len == 0),
            .champ => std.debug.assert(len <= 128),
            .string => {},
        }

        const addr = page.offset;
        var offset = page.offset;
        offset += switch (kind) {
            .real, .cons => ObjectType(kind).size(),
            .champ, .string => ObjectType(kind).size(len),
        };
        std.debug.assert(std.mem.isAlignedLog2(offset, 4)); // size() is always multiple of 16

        if (offset <= page.data.len) {
            const obj: *ObjectType(kind) = @alignCast(@ptrCast(&page.data[addr]));
            obj.head._property = 0; // NOTE very important to set the unused bit consistently
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

    eden: ?*Page = null,
    from: ?*Page = null,
    survivor: ?*Page = null,
    discard: ?*Page = null,

    survivor_compact_probability: f64 = 0.5,
    collect_alloc_counter: usize = 0,

    rng: std.Random.DefaultPrng,

    pub fn init(pool: *BlockPool) !GC {
        return GC{
            .pool = pool,
            .n_roots = 0,
            .roots = try pool.create([MAX_ROOTS]*Object),
            .rng = std.Random.DefaultPrng.init(
                (@as(u64, @bitCast(std.time.microTimestamp())) | 1) *% 11400714819323198393,
            ),
        };
    }

    pub fn deinit(gc: *GC) void {
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
        walk = gc.survivor;
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
    /// for .champ it should be the number of children (i.e. 2 * datalen + nodelen)
    /// for .string it should be the number of bytes required to store it
    pub fn alloc(gc: *GC, comptime kind: Kind, len: usize) !*ObjectType(kind) {
        if (gc.eden == null) try gc.newEden();
        var result = gc.eden.?.alloc(kind, len);
        if (result) |r| return r;
        try gc.newEden(); // page couldn't fit our object, make a new one
        result = gc.eden.?.alloc(kind, len);
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

    fn allocSurvivor(gc: *GC, comptime kind: Kind, len: usize) !*ObjectType(kind) {
        if (gc.survivor == null) try gc.newSurvivor();
        var result = gc.survivor.?.alloc(kind, len);
        if (result) |r| return r;
        try gc.newSurvivor(); // page couldn't fit our object, make a new one
        result = gc.survivor.?.alloc(kind, len);
        if (result) |r| {
            return r;
        } else {
            std.log.err("Allocation of {}, len={} does not fit in a memory block", .{ kind, len });
            return error.OversizedAllocation;
        }
    }

    fn dup(gc: *GC, comptime kind: Kind, _old: *Object) !*Object {
        std.debug.assert(_old.isFinished()); // disallow functions on unfinished allocations
        const old = _old.as(kind);
        const new = switch (kind) {
            .real, .cons => try gc.allocSurvivor(kind, 0),
            .champ => try gc.allocSurvivor(kind, 2 * old.datalen + old.nodelen),
            .string => try gc.allocSurvivor(kind, old.len),
        };
        switch (kind) {
            .real => new.data = old.data,
            .cons => {
                new.car = old.car;
                new.cdr = old.cdr;
            },
            .champ => {
                new.datamask = old.datamask;
                new.nodemask = old.nodemask;
                @memcpy(new.data(), old.data()[0 .. 2 * old.datalen + old.nodelen]);
            },
            .string => {
                new.len = old.len;
                @memcpy(new.data(), old.data()[0..old.len]);
            },
        }
        new.head.setKind(kind);
        new.head.setHash(old.head.getHash());
        return gc.commit(kind, new);
    }

    fn newEden(gc: *GC) !void {
        // new objects are allocated into eden space and are always compacted
        const new_page = try Page.create(gc.pool);
        new_page.next = gc.eden;
        gc.eden = new_page;
    }

    fn newSurvivor(gc: *GC) !void {
        // compaction allocates into survivor space
        // which may or may not be compacted
        const new_page = try Page.create(gc.pool);
        new_page.next = gc.survivor;
        gc.survivor = new_page;
        gc.collect_alloc_counter += 1;
    }

    // call the gc procedures in this order
    // for all roots:
    //   traceRoot(root)
    // shuffle
    // collect

    pub fn traceRoot(gc: *GC, root: **Object) void {
        // STOP THE WORLD
        // update the given roots if they point to forwarded objects
        // add root to the root dataset so we can trace it later
        root.* = root.*.fwd;
        gc.roots[gc.n_roots] = root.*;
        gc.n_roots += 1;
    }

    pub fn shuffle(gc: *GC) void {
        // STOP THE WORLD
        // NO UNREACHABLE YET NEEDED VALUE MAY EXIST WHEN THIS IS CALLED
        // (which could happen during e.g. path-copying insert operations)
        // move from-space to discard-space
        // move eden-space to from-space
        gc.discard = gc.from;
        gc.from = gc.eden;
        gc.eden = null;
    }

    pub fn collect(gc: *GC) !void {
        // CONCURRENT
        // move some of survivor-space to from-space (dynamic probability?)
        // mark all pages in from-space
        // trace root dataset
        // - if forwarded, update the reference
        // - if on from-space page, replicate in to-space
        // clear root dataset
        // destroy discard-space

        var n_survivors_compacted: usize = 0;
        var parent: *?*Page = &gc.survivor;
        var walk = gc.survivor;
        while (walk) |*p| {
            if (gc.rng.random().float(f64) > gc.survivor_compact_probability) {
                parent = @ptrCast(p);
            }
            parent.* = p.*.next;
            walk = p.*.next;
            p.*.next = gc.from;
            gc.from = p.*;
            n_survivors_compacted += 1;
        }

        var from_space_size: usize = 0;
        walk = gc.from;
        while (walk) |p| {
            p.mark = true;
            from_space_size += 1;
        }

        gc.collect_alloc_counter = 0;
        for (gc.roots[0..gc.n_roots]) |root| try gc.trace(root);
        gc.n_roots = 0;
        walk = gc.discard;
        while (walk) |p| {
            walk = p.next;
            gc.pool.destroy(p);
        }
        gc.discard = null;

        // the fewer pages we needed to allocate to fit the freed data
        // the more survivor pages we should try to compact next time
        // as the survivor space contained a lot of garbage
        // and vice versa
        std.debug.print("compact p = {}\n", .{gc.survivor_compact_probability});
        std.debug.print("n survivors = {}\n", .{n_survivors_compacted});
        std.debug.print("n from-space = {}\n", .{from_space_size});
        std.debug.print("collect allocs = {}\n", .{gc.collect_alloc_counter});
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
                if (cons.car) |car| cons.car = car.fwd;
                if (cons.cdr) |cdr| cons.cdr = cdr.fwd;
            },
            .champ => {
                const champ = obj.as(.champ);
                for (0..2 * champ.datalen + champ.nodelen) |i| {
                    const child = champ.data()[i] orelse continue;
                    champ.data()[i] = child.fwd;
                }
            },
        }
        // now, replicate if we are on a marked page
        // and we haven't already been replicated
        if (obj == obj.fwd and obj.page().mark) {
            const r = switch (obj.getKind()) {
                .real => try gc.dup(.real, obj),
                .cons => try gc.dup(.cons, obj),
                .champ => try gc.dup(.champ, obj),
                .string => try gc.dup(.string, obj),
            };
            obj.fwd = r;
        }

        // finally, keep tracing
        switch (obj.getKind()) {
            .real, .string => {},
            .cons => {
                const cons = obj.as(.cons);
                // note, trace cdr first s.t. linked lists are sequential
                try gc.trace(cons.cdr);
                try gc.trace(cons.car);
            },
            .champ => {
                const champ = obj.as(.champ);
                // TODO evaluate if tracing nodes before data (or any other order) is best
                for (0..2 * champ.datalen + champ.nodelen) |i| try gc.trace(champ.data()[i]);
            },
        }
    }
};
