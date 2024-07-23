const std = @import("std");

const BlockPool = @import("blockpool.zig").BlockPool(.{ .reserve = 2 });

const Hash = u32;
const NILHASH: Hash = 0x00b4b4b4;

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

fn finalize(comptime kind: Kind, val: KindType(kind)) void {
    _ = val;
    switch (kind) {
        .real => {},
        .cons => {},
        .hamt => {},
    }
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

pub const nil = Ref.init(std.math.maxInt(u32), undefined, NILHASH);

comptime {
    std.debug.assert(@sizeOf(Ref) == 8);
    std.debug.assert(@alignOf(Ref) == 8);

    for (0..std.meta.fields(Kind).len) |i| {
        const kind: Kind = @enumFromInt(i);
        std.debug.assert(@sizeOf(KindType(kind)) <= 32);
    }
}

fn SingularGCType(comptime kind: Kind) type {
    const T = KindType(kind);
    const END = std.math.maxInt(u32);

    return struct {
        const SGC = @This();

        arena_alloc: std.mem.Allocator,
        data: std.SegmentedList(T, 0),
        meta: std.SegmentedList(i32, 0), // NOTE refcount of 0 indicates one owner
        free: u32,

        /// to free, just clear the arena allocator
        fn init(arena_alloc: std.mem.Allocator) SGC {
            return SGC{
                .arena_alloc = arena_alloc,
                .data = std.SegmentedList(T, 0){},
                .meta = std.SegmentedList(i32, 0){},
                .free = END,
            };
        }

        fn create(sgc: *SGC, val: T, hash: u32, gc: *GC) !Ref {
            if (sgc.free == END) {
                std.debug.assert(sgc.data.len == sgc.meta.len);
                const ref = Ref.init(kind, @intCast(sgc.data.len), hash);
                try sgc.data.append(sgc.arena_alloc, val);
                errdefer _ = sgc.data.pop();
                try sgc.meta.append(sgc.arena_alloc, 0);
                errdefer _ = sgc.meta.pop();
                gc._inc(kind, ref);
                return ref;
            }

            // TODO
            unreachable;
        }

        fn release(sgc: *SGC, ref: Ref) void {
            if (ref.isNil()) return;
            const meta = sgc.meta.at(ref.slot());
            meta.* -= 1;
        }

        fn get(sgc: *SGC, ref: Ref) T {
            std.debug.assert(!ref.isNil());
            // return sgc.data.uncheckedAt(ref.slot());
            return sgc.data.at(ref.slot()).*;
        }

        fn recoup(sgc: *SGC, gc: *GC) void {
            // put all items with a negative refcount on the free-list
            var it = sgc.meta.iterator();
            var i = 0;
            while (it.next()) |*meta| {
                if (meta.* < 0) {
                    gc._dec()
                    meta.* = sgc.free;
                    sgc.free = i;
                }
                i += 1;
            }
        }
    };
}

const GC = struct {
    pub fn _inc(gc: *GC, comptime kind: Kind, ref: Ref) void {
        _ = gc;
        _ = kind;
        _ = ref;
    }

    pub fn _dec(gc: *GC, comptime kind: Kind, ref: Ref) void {
        _ = gc;
        _ = kind;
        _ = ref;
    }
};

test "scratch" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var gc = GC{};
    var sgc = SingularGCType(.real).init(arena.allocator());

    const x0 = try sgc.create(0.0, 1337, &gc);
    std.debug.print("{} {}\n", .{ x0, sgc.get(x0) });

    const x1 = try sgc.create(1.0, 1337, &gc);
    std.debug.print("{} {}\n", .{ x1, sgc.get(x1) });

    sgc.release(x0);
    sgc.recoup(&gc);

    const x2 = try sgc.create(2.0, 1337, &gc);
    std.debug.print("{} {}\n", .{ x2, sgc.get(x2) });
}
