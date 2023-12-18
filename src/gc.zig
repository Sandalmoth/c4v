const std = @import("std");

pub const Header = struct {
    const Status = enum {
        free,
        live,
        mark,
    };

    size: u32, // a fair limitation
    final: bool, // this is the last block on page
    status: Status,
};

const MIN_ALIGN = @max(@alignOf(Header), @alignOf(*opaque {}));
const MIN_SIZE = @sizeOf(*opaque {}); // s.t. we can always fit the size in the free list

comptime {
    std.debug.assert(@sizeOf(Header) == @sizeOf(usize));
    std.debug.assert(@alignOf(Header) <= @alignOf(usize));
    std.debug.assert(@alignOf(usize) <= @sizeOf(usize)); // are there systems where this is false?
    std.debug.assert(@alignOf(usize) == @alignOf(*usize)); // i think this is true by definition?
}

pub const Config = struct {
    page_size: usize = std.mem.page_size,
    backing_allocator: std.mem.Allocator = std.heap.page_allocator,
};

pub fn GC(comptime config: Config) type {
    const Page = struct {
        const capacity = (config.page_size - @sizeOf(?*@This()));

        data: [capacity]u8 = undefined,
        next: ?*@This() = null,
    };

    std.debug.assert(@sizeOf(Page) == config.page_size);
    std.debug.assert(@alignOf(Page) >= MIN_ALIGN);

    return struct {
        const Self = @This();

        const backing_allocator: std.mem.Allocator = std.heap.page_allocator;
        const page_size: usize = config.page_size;

        first_page: ?*Page = null,

        // the free list points to a header
        // which always stores the size of a block
        // with the pointer to the next free block folling at the start of the data right after
        free: ?*Header = null,

        pub fn create(gc: *Self, comptime T: type) *T {
            if (gc.first_page == null) {
                gc.newPage();
            }

            // find the first empty slot
            std.debug.assert(gc.free != null);
            var free = gc.free;
            while (free.?.size)
                return undefined;

            // var loc = std.mem.alignBackward(
            //     usize,
            //     gc.first_page.?.free -| @sizeOf(T),
            //     @max(@alignOf(usize), @alignOf(T)),
            // );

            // if (loc < @intFromPtr(&gc.first_page.?.bytes) + @sizeOf(usize)) {
            //     if (gc.first_page.?.free == @intFromPtr(&gc.first_page.?.bytes) + Page.capacity) {
            //         @panic("GC.create: " ++ @typeName(T) ++ " cannot fit in a GC.Page");
            //     }

            //     // no room on page, so make an new one and find a new alignd address
            //     gc.newPage();
            //     return @call(.always_tail, create, .{ gc, T });
            // }

            // std.debug.print("{} {}\n", .{ @alignOf(T), @sizeOf(T) });
            // gc.first_page.?.free = loc - @sizeOf(usize);

            // std.debug.print("{}\n", .{loc});
            // return Ref(T){ .ptr = @ptrFromInt(loc) };
        }

        fn newPage(gc: *Self) void {
            var page = backing_allocator.create(Page) catch @panic("GC.newPage: out of memory");
            page.* = Page{};
            @as(*Header, @alignCast(@ptrCast(&page.data[0]))).* = .{
                .size = Page.capacity,
                .final = true,
                .status = .free,
            };
            @as(*?*Header, @alignCast(@ptrCast(&page.data[@sizeOf(Header)]))).* = gc.free;
            gc.free = @alignCast(@ptrCast(&page.data[0]));
            page.next = gc.first_page;
            gc.first_page = page;
        }
    };
}

test "scratch" {
    std.debug.print("\n", .{});
    var gc = GC(.{ .backing_allocator = std.testing.allocator }){};
    for (0..1) |_| {
        _ = gc.create(i32);
        _ = gc.create(@Vector(4, f64));
        _ = gc.create(i32);
        _ = gc.create(u8);
        _ = gc.create(@Vector(37, f64));
    }
}
