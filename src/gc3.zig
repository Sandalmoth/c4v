const std = @import("std");

const BlockPool = @import("blockpool.zig").BlockPool(.{ .reserve = 2 });

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

pub const Ref = struct {
    _location: u32 align(8),
    _property: u32,

    pub fn init(_kind: Kind, _page: u16, _slot: u16, _hash: u32) Ref {
        return .{
            ._location = @as(u32, _page) | (@as(u32, _slot) << 16),
            ._property = (_hash & 0x00ffffff) | (@as(u32, @intFromEnum(_kind)) << 24),
        };
    }

    fn page(ref: Ref) u32 {
        return ref._location & 0x0000ffff;
    }

    fn slot(ref: Ref) u32 {
        return @intCast(ref._location >> 16);
    }

    pub fn hash(ref: Ref) u32 {
        return ref._property & 0x00ffffff;
    }

    pub fn kind(ref: Ref) Kind {
        return @enumFromInt(ref._property >> 24);
    }

    pub fn isNil(ref: Ref) bool {
        return ref._location == std.math.maxInt(u32);
    }
};

pub const nil = Ref{ ._location = std.math.maxInt(u32), ._property = undefined };

comptime {
    std.debug.assert(@sizeOf(Ref) == 8);
    std.debug.assert(@alignOf(Ref) == 8);
}

fn SingularGCType(comptime kind: Kind) type {
    const T = KindType(kind);
    const TNode = extern union { t: T, node: u32 };
    const N = @min(65536, (BlockPool.BLOCK_SIZE - 1024) / @sizeOf(TNode));
    const NO_T = std.math.maxInt(u32);
    const NO_PAGE = std.math.maxInt(u64);

    const Page = struct {
        len: usize,
        data: [N]TNode,
        used: std.StaticBitSet(N),
        marks: std.StaticBitSet(N),

        fn create(pool: *BlockPool) !*@This() {
            var page = try pool.create(@This());
            page.len = 0;
            page.used = std.StaticBitSet(N).initEmpty();
            return page;
        }
    };
    const INode = extern union { p: ?*Page, node: usize };

    const PageIndex = struct {
        pages: [BlockPool.BLOCK_SIZE / @sizeOf(usize)]INode,
    };

    std.debug.assert(@sizeOf(Page) <= BlockPool.BLOCK_SIZE);
    std.debug.assert(@sizeOf(PageIndex) <= BlockPool.BLOCK_SIZE);

    return struct {
        const SGC = @This();

        pool: *BlockPool,
        index: *PageIndex,
        n_pages: usize,

        free_t: u32,
        free_p: usize,

        fn init(pool: *BlockPool) !SGC {
            var sgc = SGC{
                .pool = pool,
                .index = try pool.create(PageIndex),
                .n_pages = 0,
                .free_t = NO_T,
                .free_p = NO_PAGE,
            };
            errdefer sgc.deinit();
            try sgc.newPage();
            return sgc;
        }

        fn deinit(sgc: *SGC) void {
            sgc.pool.destroy(sgc.index);
            sgc.* = undefined;
        }

        fn newPage(sgc: *SGC) !void {
            if (sgc.free_p == NO_PAGE) {
                sgc.index.pages[sgc.n_pages] = try Page.create(sgc.pool);
            }
        }

        fn get(sgc: *SGC, ref: Ref) T {
            std.debug.assert(!ref.isNil());
            const page = sgc.page_index.pages[ref.page()].p;
            std.debug.assert(page.isSet(ref.slot()));
            return page.data[ref.slot()].t;
        }

        fn trace(sgc: *SGC, ref: Ref, gc: *GC) void {
            _ = sgc;
            _ = ref;
            _ = gc;
        }

        fn sweep(sgc: *SGC) void {
            _ = sgc;
        }

        fn compact(sgc: *SGC) void {
            _ = sgc;
        }
    };
}

const GC = struct {};

test "yo" {
    var pool = try BlockPool.init(std.testing.allocator);
    defer pool.deinit();

    var sgc = try SingularGCType(.real).init(&pool);
    defer sgc.deinit();
}
