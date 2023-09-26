const std = @import("std");

// the std memorypool has alignment issues for some types
// this implementation lacks some features, but doesn't have that problem

pub fn Pool(comptime T: type) type {
    return struct {
        const Self = @This();

        const Node = union {
            item: T,
            next: ?*Node,
        };

        arena: std.heap.ArenaAllocator,
        free_list: ?*Node,

        pub fn init(alloc: std.mem.Allocator) Self {
            return .{
                .arena = std.heap.ArenaAllocator.init(alloc),
                .free_list = null,
            };
        }

        pub fn deinit(pool: *Self) void {
            pool.arena.deinit();
        }

        pub fn create(pool: *Self) !*T {
            const node = if (pool.free_list) |item| blk: {
                pool.free_list = item.next;
                break :blk item;
            } else try pool.arena.allocator().create(Node);

            node.* = Node{ .item = undefined };
            return @alignCast(@ptrCast(node));
        }

        pub fn destroy(pool: *Self, item: *T) void {
            const node: *Node = @alignCast(@ptrCast(item));
            node.* = Node{
                .next = pool.free_list,
            };
            pool.free_list = node;
        }
    };
}

test "memory pool: basic" {
    var pool = Pool(u32).init(std.testing.allocator);
    defer pool.deinit();

    const p1 = try pool.create();
    const p2 = try pool.create();
    const p3 = try pool.create();

    // Assert uniqueness
    try std.testing.expect(p1 != p2);
    try std.testing.expect(p1 != p3);
    try std.testing.expect(p2 != p3);

    pool.destroy(p2);
    const p4 = try pool.create();

    // Assert memory reuse
    try std.testing.expect(p2 == p4);
}
