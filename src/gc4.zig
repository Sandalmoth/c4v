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
    kind: Kind,

    pub fn value(head: *Object, comptime kind: Kind) ValueType(kind) {
        std.debug.assert(head.kind == kind);
        const ptr: *ObjectType(kind) = @ptrCast(head);
        return ptr.data;
    }
};

pub fn ObjectType(comptime kind: Kind) type {
    return extern struct {
        head: Object,
        data: ValueType(kind),
    };
}

fn SingularGCType(comptime kind: Kind) type {
    const TVAL = ValueType(kind);
    const TOBJ = ObjectType(kind);
    const N = (BlockPool.BLOCK_SIZE - 128) / @sizeOf(TOBJ);
    const Page = struct {
        next: ?*@This(),
        n: usize,
        index: usize,
        data: [N]TOBJ,

        fn create(pool: *BlockPool) *@This() {
            const page = pool.create(@This()) catch @panic("oom");
            page.n = 0;
            return page;
        }
    };
    std.debug.assert(@sizeOf(Page) <= BlockPool.BLOCK_SIZE);

    return struct {
        const SGC = @This();

        pool: *BlockPool,
        eden: ?*Page = null,
        fr: ?*Page = null,
        to: ?*Page = null,

        n_pages: usize,
        rolling_index: usize,
        compacting_index: usize,

        pub fn init(pool: *BlockPool) SGC {
            return SGC{
                .pool = pool,
            };
        }

        pub fn deinit(sgc: *SGC) void {
            var walk: ?*Page = sgc.eden;
            while (walk) |p| {
                walk = p.next;
                sgc.pool.destroy(p);
            }
            walk = sgc.fr;
            while (walk) |p| {
                walk = p.next;
                sgc.pool.destroy(p);
            }
            walk = sgc.to;
            while (walk) |p| {
                walk = p.next;
                sgc.pool.destroy(p);
            }
            sgc.* = undefined;
        }

        pub fn newEden(sgc: *SGC) void {
            const new_eden = Page.create(sgc.pool);
            new_eden.next = sgc.eden;
            new_eden.index = sgc.index;
            sgc.index += 1;
            sgc.eden = new_eden;
        }

        pub fn create(sgc: *SGC, val: TVAL) *Object {
            if (sgc.eden == null or sgc.eden.?.n == N) sgc.newEden();
            const eden = sgc.eden.?;
            const object: *TOBJ = @ptrCast(&eden.data[eden.n]);
            eden.n += 1;
            object.head = .{
                .fwd = null,
                .hash = 1337,
                .kind = kind,
            };
            object.data = val;
            return @ptrCast(object);
        }

        pub fn begin(sgc: *SGC) void {
            _ = sgc;
            // select the pages that are to be evacuated
            // by moving them to the from-space and marking them
        }

        pub fn trace(sgc: *SGC, root: *Object) void {
            _ = sgc;
            _ = root;
            // traverse and evacuate any objects in the from space
            // by building a replica in to-space and forwarding usign the brooks ptr
        }

        pub fn end(sgc: *SGC) void {
            _ = sgc;
            // iterate over all pages
            // update any references to objects in from-space
            // call finalizers for any unmoved objects in from-space
        }

        pub fn gc0(sgc: *SGC) void {

            //

            // in eden space, there are pointers into from-space
            // and we have a replica of from-space in to-space
            // with forwarding pointers in the from-space objects

            // update pointers in eden using forwarding
            // delete from-space
            // move eden to from-space
            // move to-space to from-space
            // start tracing and compacting into to-space

            // mo
            // move eden to from space
            _ = sgc;
        }
    };
}

test "scratch" {
    var pool = try BlockPool.init(std.testing.allocator);
    defer pool.deinit();

    var sgc = SingularGCType(.real).init(&pool);
    defer sgc.deinit();

    const x = sgc.create(1.0);
    std.debug.print("{}\n", .{x.value(.real)});
}
