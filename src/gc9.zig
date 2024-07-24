const std = @import("std");
const builtin = @import("builtin");

const BlockPool = @import("blockpool.zig").BlockPool(.{});

const SharedRng = struct {
    const DummyMutex = struct {
        fn lock(_: *DummyMutex) void {}
        fn unlock(_: *DummyMutex) void {}
    };

    rng: std.Random.DefaultPrng,
    mutex: if (builtin.single_threaded) DummyMutex else std.Thread.Mutex,

    fn init() SharedRng {
        return .{
            .rng = std.Random.DefaultPrng.init(11910820912655355079),
            .mutex = if (builtin.single_threaded) DummyMutex{} else std.Thread.Mutex{},
        };
    }

    fn float64(self: *SharedRng) f64 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.rng.random().float(f64);
    }

    fn uint64(self: *SharedRng) u64 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.rng.random().int(u64);
    }
};
var rng = SharedRng.init();

pub const Kind = enum(u8) {
    real,
    cons,
    champ,
    string,
};

pub const Object = extern struct {
    fwd: *Object align(16),
    kind: Kind,
    finished: bool, // just a nice double check to make sure we always call gc.commit
    // what should we do with all our spare bits here?
    // - external types?
    // - offset to next object in page so we could walk the pages?

    pub fn hash(obj: ?*Object, level: u64) u64 {
        // I dont' want to support hash collisions
        // so instead, we'll just support an infinite series hash-bits
        if (obj == null) {
            // this is the prime closest to 2^64/phi
            // so the weyl sequence will visit all 2^64 numbers
            // and the bits will be maximally different from the previous level
            const seed: u64 = 11400714819323198393 *% level;
            return seed *% 12542518518317951677 +% 14939819388667570391;
        }
        std.debug.assert(obj.?.finished);
        return switch (obj.?.kind) {
            .real => obj.?.as(.real).hash(level),
            .cons => obj.?.as(.cons).hash(level),
            .champ => obj.?.as(.champ).hash(level),
            .string => obj.?.as(.string).hash(level),
        };
    }

    pub fn as(obj: *Object, comptime kind: Kind) *ObjectType(kind) {
        std.debug.assert(obj.finished);
        std.debug.assert(obj.kind == kind);
        return @alignCast(@ptrCast(obj));
    }

    pub fn page(obj: *Object) *Page {
        std.debug.assert(obj.finished);
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

    pub fn hash(real: *ObjectReal, level: u64) u64 {
        const seed: u64 = 11400714819323198393 *% level;
        // make +0 and -0 hash to the same value, since they compare equal
        // i just picked these primes at random, might not be the best
        if (real.data == 0) return seed *% 15345951513627307427 +% 11490803873075654471;
        // make NaN random, since it compares unequal
        // avoids performance degradation when inserting many NaN keys (not recommended though)
        if (real.data != real.data) return rng.uint64();
        return std.hash.XxHash3.hash(seed, std.mem.asBytes(&real.data));
    }

    fn size() usize {
        return std.mem.alignForwardLog2(@sizeOf(ObjectReal), 4);
    }
};

pub const ObjectCons = extern struct {
    head: Object,
    car: ?*Object,
    cdr: ?*Object,

    pub fn hash(cons: *ObjectCons, level: u64) u64 {
        var h: u64 = 0;
        h ^= Object.hash(cons.car, level);
        h ^= Object.hash(cons.car, level);
        return h;
    }

    fn size() usize {
        return std.mem.alignForwardLog2(@sizeOf(ObjectCons), 4);
    }
};

pub const ObjectChamp = extern struct {
    head: Object,
    datamask: u64,
    nodemask: u64,
    datalen: u32,
    nodelen: u32,

    pub fn hash(champ: *ObjectChamp, level: u64) u64 {
        // entries in data have two pointers, a key and a value
        // and entries in in nodes have just one
        // however, the hash is simply the xor of all of the kv in the map
        // and since xor is commutative, we can simply xor everything together
        var h: u64 = 0;
        const children = champ.data();
        for (0..2 * champ.datalen + champ.nodelen) |i| {
            // while the nodes cannot be null, the data could be
            h ^= Object.hash(children[i], level);
        }
        return h;
    }

    pub fn data(champ: *ObjectChamp) [*]?*Object {
        return @ptrFromInt(@intFromPtr(&champ.nodemask) + 16);
    }

    pub fn nodes(champ: *ObjectChamp) [*]*Object {
        return @ptrFromInt(@intFromPtr(&champ.nodemask) + 16 + 16 * champ.datalen);
    }

    fn size(_len: usize) usize {
        return std.mem.alignForwardLog2(
            @sizeOf(ObjectChamp) + @sizeOf(usize) * _len,
            4,
        );
    }
};

pub const ObjectString = extern struct {
    head: Object,
    len: usize,

    pub fn hash(string: *ObjectString, level: u64) u64 {
        const seed: u64 = 11400714819323198393 *% level;
        return std.hash.XxHash3.hash(seed, string.data()[0..string.len]);
    }

    pub fn data(string: *ObjectString) [*]u8 {
        return @ptrFromInt(@intFromPtr(&string.len) + 8);
    }

    fn size(len: usize) usize {
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
            obj.head.fwd = @ptrCast(obj);
            obj.head.kind = kind;
            obj.head.finished = false;
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

    pub fn init(pool: *BlockPool) !GC {
        return GC{
            .pool = pool,
            .n_roots = 0,
            .roots = try pool.create([MAX_ROOTS]*Object),
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

    /// marks the object as usable
    pub fn commit(gc: *GC, comptime kind: Kind, obj: *ObjectType(kind)) *Object {
        // i guess we don't really need this to be a gc function?
        // however, seems kinda nice just for consistency?
        _ = gc;
        std.debug.assert(!obj.head.finished);
        std.debug.assert(obj.head.kind == kind);
        obj.head.finished = true;
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
        std.debug.assert(_old.finished); // disallow functions on unfinished allocations
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
                new.datalen = old.datalen;
                new.nodelen = old.nodelen;
                @memcpy(new.data(), old.data()[0 .. 2 * old.datalen + old.nodelen]);
            },
            .string => {
                new.len = old.len;
                @memcpy(new.data(), old.data()[0..old.len]);
            },
        }
        new.head.kind = kind;
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
        while (walk) |p| {
            if (rng.float64() < gc.survivor_compact_probability) {
                parent.* = p.next;
                walk = p.next;
                p.next = gc.from;
                gc.from = p;
            } else {
                parent = &p.next;
                walk = p.next;
            }
            n_survivors_compacted += 1;
        }

        var from_space_size: usize = 0;
        walk = gc.from;
        while (walk) |p| {
            p.mark = true;
            walk = p.next;
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

        // assume that the odds of surviving is the same in eden and survivor space (tunable?)
        const ratio = @as(f64, @floatFromInt(gc.collect_alloc_counter)) /
            @as(f64, @floatFromInt(from_space_size));
        gc.survivor_compact_probability = 1.0 - ratio;
        gc.survivor_compact_probability = 1.0;

        // std.debug.print("compact p = {}\n", .{gc.survivor_compact_probability});
        // std.debug.print("n survivors = {}\n", .{n_survivors_compacted});
        // std.debug.print("n from-space = {}\n", .{from_space_size});
        // std.debug.print("collect allocs = {}\n", .{gc.collect_alloc_counter});
        // std.debug.print("ratio = {}\n", .{ratio});
    }

    fn trace(gc: *GC, ptr: ?*Object) !void {
        const obj = ptr orelse return;
        // NOTE for concurrency, we should have a separate allocation page for this
        // (really, a separate allocatioon page for each thread)
        // and use atomics for updating the pointers (?)
        // first, try to update any references using the forwarding pointer
        switch (obj.kind) {
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
            const r = switch (obj.kind) {
                .real => try gc.dup(.real, obj),
                .cons => try gc.dup(.cons, obj),
                .champ => try gc.dup(.champ, obj),
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
            .champ => {
                const champ = obj.as(.champ);
                // i think this tracing-order is marginally better
                // but would need careful benchmarking, it's not a huge difference
                for (0..champ.nodelen) |i| try gc.trace(champ.nodes()[i]);
                for (0..champ.datalen) |i| try gc.trace(champ.data()[2 * i]);
                for (0..champ.datalen) |i| try gc.trace(champ.data()[2 * i + 1]);
            },
        }
    }
};
