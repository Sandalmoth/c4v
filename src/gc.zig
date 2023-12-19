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

pub fn GarbageCollector(comptime Spec: type, comptime config: Config) type {
    const TracedType = std.meta.FieldEnum(Spec);
    const n_types = std.meta.fields(TracedType).len;

    const Page = struct {
        data: [config.capacity]u8,
        // could be a bitset, but this is simpler
        // and right now i want to get to working
        info: [config.capacity / config.unit]u8,
    };

    _ = n_types;

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

        pub fn create(gc: *GC, comptime t: TracedType) *TTT(t) {
            if (@alignOf(TTT(t)) <= config.unit) {
                const p = gc.free;
                gc.free += roundup(@sizeOf(TTT(t)), config.unit);
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
        pub fn trace(gc: *GC, comptime t: TracedType, root: ?*TTT(t)) ?*TTT(t) {
            if (root == null) {
                return null;
            }

            // if we haven't moved this, move and overwrite original with new address
            // if we have already moved the item, ???

            var new: ?*TTT(t) = null;
            const root_info = gc.info(root);

            switch (root_info.*) {
                0 => {
                    std.debug.print("moving\n", .{});
                    // move
                    new = gc.create(t);
                    new.?.* = root.?.*;
                    @as(*?*TTT(t), @alignCast(@ptrCast(root.?))).* = new;
                    root_info.* = 1;

                    const type_info = @typeInfo(TTT(t));
                    std.debug.print("{}\n", .{type_info});

                    switch (type_info) {
                        .Optional => |optional| {
                            const optinfo = @typeInfo(optional.child);
                            if (optinfo == .Pointer) {
                                if (comptime iTTT(optinfo.Pointer.child)) |child| {
                                    std.debug.print("YO\n", .{});
                                    new.?.* = gc.trace(child, root.?.*);
                                }
                            }
                        },
                        .Struct => |_struct| {
                            inline for (_struct.fields) |field| {
                                const fieldinfo = @typeInfo(field.type);
                                if (fieldinfo == .Optional) {
                                    const optinfo = @typeInfo(fieldinfo.Optional.child);
                                    if (optinfo == .Pointer) {
                                        if (comptime iTTT(optinfo.Pointer.child)) |child| {
                                            std.debug.print("DAWG\n", .{});
                                            @field(new, field.name) = gc.trace(
                                                child,
                                                @field(root.?, field.name),
                                            );
                                        }
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                },
                1 => {
                    std.debug.print("found already moved\n", .{});
                    new = @as(*?*TTT(t), @alignCast(@ptrCast(root.?))).*;
                },
                else => unreachable,
            }

            return new;
        }

        /// updates references???
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

        fn TTT(comptime t: TracedType) type {
            return std.meta.fields(Spec)[@intFromEnum(t)].type;
        }

        fn iTTT(comptime U: type) ?TracedType {
            inline for (std.meta.fields(Spec), 0..) |field, i| {
                if (field.type == U) {
                    return @enumFromInt(i);
                }
            }
            return null;
        }
    };
}

const _S = struct {
    int: i32,
    ptr: ?*i32,
};

test "scratch" {
    var gc = try GarbageCollector(_S, .{}).init(std.testing.allocator);
    defer gc.deinit();

    std.debug.print("\n", .{});
    std.debug.print("{} {}\n", .{
        @sizeOf(@TypeOf(gc.from.data)),
        @sizeOf(@TypeOf(gc.from.info)),
    });

    var x = gc.create(.int);
    x.* = 1234;
    var y = gc.create(.ptr);
    y.* = x;
    var z = gc.create(.ptr);
    z.* = x;

    std.debug.print("{*} {}\n", .{ x, x.* });
    std.debug.print("{*} {*} {}\n", .{ y, y.*, y.*.?.* });
    std.debug.print("{*} {*} {}\n", .{ z, z.*, z.*.?.* });

    gc.begin();
    y = gc.trace(.ptr, y).?;
    x = gc.trace(.int, x).?;
    z = gc.trace(.ptr, z).?;
    gc.end();

    std.debug.print("{*} {}\n", .{ x, x.* });
    std.debug.print("{*} {*} {}\n", .{ y, y.*, y.*.?.* });
    std.debug.print("{*} {*} {}\n", .{ z, z.*, z.*.?.* });
}
