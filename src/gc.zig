const std = @import("std");
const builtin = @import("builtin");

pub const Config = struct {
    capacity: usize = 16 * 1024 * 1024,
    unit: usize = 16,
};

inline fn roundup(x: usize, m: usize) usize {
    // certified GMO lol
    // https://stackoverflow.com/a/9194117
    return (m + x - 1) & ~(m - 1);
}

test "roundUp" {
    try std.testing.expectEqual(0, roundup(0, 16));
    try std.testing.expectEqual(16, roundup(8, 16));
    try std.testing.expectEqual(16, roundup(16, 16));
    try std.testing.expectEqual(32, roundup(24, 16));
}

pub fn GarbageCollector(comptime config: Config) type {
    const Page = struct {
        data: [config.capacity]u8,
        // could be a bitset, but this is simpler
        // and right now i want to get to working
        info: [config.capacity / config.unit]u8,
    };

    return struct {
        const GC = @This();

        alloc: std.mem.Allocator,

        pages: []Page,
        from: *align(config.unit) Page,
        to: *align(config.unit) Page,

        free: [*]u8,

        pub fn init(alloc: std.mem.Allocator) !GC {
            var gc = GC{
                .alloc = alloc,
                .pages = undefined,
                .from = undefined,
                .to = undefined,
                .free = undefined,
            };
            gc.pages = try alloc.alignedAlloc(Page, config.unit, 2);
            errdefer alloc.free(gc.pages);
            // i believe these aligncasts are guaranteed to succeed
            gc.from = @alignCast(&gc.pages[0]);
            gc.to = @alignCast(&gc.pages[1]);
            reset(gc.to);
            gc.free = &gc.to.data;
            return gc;
        }

        pub fn create(gc: *GC, comptime T: type) *T {
            if (@alignOf(T) <= config.unit) {
                const p = gc.free;
                gc.free += roundup(@sizeOf(T), config.unit);
                return @alignCast(@ptrCast(p));
            } else {
                @panic("unimplemented");
            }
        }

        /// switch pages allowing us to start transfering to the new half-space
        pub fn begin(gc: *GC) void {
            std.mem.swap(*align(config.unit) Page, &gc.from, &gc.to);
            reset(gc.to);
            gc.free = &gc.to.data;
        }

        /// call this on all roots and all external references to GC'd memory
        /// returns the new address
        pub fn trace(gc: *GC, comptime T: type, root: *T) *T {
            var new: *T = undefined;
            const root_info = gc.info(root);

            switch (root_info.*) {
                0 => {
                    // move into new half-space
                    new = gc.create(T);
                    new.* = root.*;
                    @as(**T, @alignCast(@ptrCast(root))).* = new;
                    root_info.* = 1;

                    // follow references
                    new.trace(gc);
                },
                1 => {
                    // we've already traced this object, so just update this reference
                    new = @as(**T, @alignCast(@ptrCast(root))).*;
                },
                else => unreachable,
            }

            return new;
        }

        pub fn end(gc: *GC) void {
            _ = gc;
        }

        pub fn deinit(gc: *GC) void {
            gc.alloc.free(gc.pages);

            gc.* = undefined;
        }

        fn info(gc: GC, ptr: anytype) *u8 {
            const offset = @intFromPtr(ptr) - @intFromPtr(&gc.from.data);
            std.debug.assert(offset % config.unit == 0);
            return &gc.from.info[offset / config.unit];
        }

        fn reset(page: *align(config.unit) Page) void {
            // clear all the info bits indicating that space is free
            for (0..config.capacity / config.unit) |i| {
                page.info[i] = 0;
            }
            if (builtin.mode == .Debug) {
                for (0..config.unit) |i| {
                    page.data[i] = 0xAA;
                }
            }
        }
    };
}

const _Int = struct {
    int: i32,

    pub inline fn trace(x: _Int, gc: anytype) void {
        _ = x;
        _ = gc;
    }
};

const _IntRef = struct {
    ptr: ?*_Int,

    pub inline fn trace(x: *_IntRef, gc: anytype) void {
        if (x.ptr) |p| {
            x.ptr = gc.trace(_Int, p);
        }
    }
};

test "scratch" {
    var gc = try GarbageCollector(.{}).init(std.testing.allocator);
    defer gc.deinit();

    std.debug.print("\n", .{});
    std.debug.print("{} {}\n", .{
        @sizeOf(@TypeOf(gc.from.data)),
        @sizeOf(@TypeOf(gc.from.info)),
    });

    var x = gc.create(_Int);
    x.int = 1234;
    var y = gc.create(_IntRef);
    y.ptr = x;
    var z = gc.create(_IntRef);
    z.ptr = x;

    std.debug.print("{*} {}\n", .{ x, x.int });
    std.debug.print("{*} {*} {}\n", .{ y, y.ptr.?, y.ptr.?.int });
    std.debug.print("{*} {*} {}\n", .{ z, z.ptr.?, z.ptr.?.int });

    gc.begin();
    y = gc.trace(_IntRef, y);
    x = gc.trace(_Int, x);
    z = gc.trace(_IntRef, z);
    gc.end();

    std.debug.print("{*} {}\n", .{ x, x.int });
    std.debug.print("{*} {*} {}\n", .{ y, y.ptr.?, y.ptr.?.int });
    std.debug.print("{*} {*} {}\n", .{ z, z.ptr.?, z.ptr.?.int });
}
