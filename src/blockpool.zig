const std = @import("std");
const builtin = @import("builtin");

pub const Config = struct {
    block_size: comptime_int = std.mem.page_size,
    reserve: comptime_int = 1024,
    preheat: comptime_int = 1024,
    thread_safe: bool = !builtin.single_threaded,
};

// neat design from std.GeneralPurposeAllocator
const DummyMutex = struct {
    fn lock(_: *DummyMutex) void {}
    fn unlock(_: *DummyMutex) void {}
};

pub fn BlockPool(comptime config: Config) type {
    std.debug.assert(config.preheat <= config.preheat);

    const Block = struct {
        bytes: [config.block_size]u8 align(config.block_size),
    };

    return struct {
        const Pool = @This();
        pub const BLOCK_SIZE = config.block_size;

        alloc: std.mem.Allocator,
        mutex: if (config.thread_safe) std.Thread.Mutex else DummyMutex,
        free: []*Block,
        n_free: usize align(64),

        // stats
        n_total: usize,
        n_allocs: usize, // if this grows much bigger than n_total, try increasing reserve

        /// not threadsafe
        pub fn init(alloc: std.mem.Allocator) !Pool {
            var pool = Pool{
                .alloc = alloc,
                .mutex = if (config.thread_safe) std.Thread.Mutex{} else DummyMutex{},
                .free = try alloc.alloc(*Block, config.reserve),
                .n_free = 0,
                .n_total = 0,
                .n_allocs = 0,
            };
            while (pool.n_free < config.preheat) {
                pool.free[pool.n_free] = try alloc.create(Block);
                errdefer pool.deinit();
                pool.n_free += 1;
                pool.n_total += 1;
                pool.n_allocs += 1;
            }
            return pool;
        }

        /// not threadsafe
        pub fn deinit(pool: *Pool) void {
            for (0..pool.n_free) |i| pool.alloc.destroy(pool.free[i]);
            pool.alloc.free(pool.free);
            pool.* = undefined;
        }

        // TODO?
        // pub fn allocator(pool: *Pool) std.mem.Allocator {
        //     return .{ .ptr = pool, .vtable = &.{
        //         .alloc = alloc,
        //         .resize = resize,
        //         .free = free,
        //     } };
        // }

        // fn alloc(ctx: *anyopaque, len: usize, log2_align: u8, ret: usize) ?[*]u8 {
        //     _ = ret;
        //     std.debug.assert(len <= config.block_size);
        //     std.debug.assert(@as(usize, 1) << @as(
        //         std.mem.Allocator.Log2Align,
        //         @intCast(log2_align),
        //     ) <= config.block_size);

        //     const pool: *BlockPool = @alignCast(@ptrCast(ctx));
        //     return @ptrCast(@alignCast(pool.createAny() orelse null));
        // }

        // fn resize(ctx: *anyopaque, buf: []u8, log2_align: u8, new_len: usize, ret: usize) bool {
        //     if (new_len)
        // }

        // fn free(ctx: *anyopaque, buf: []u8, log2_align: u8, ret: usize) void {}

        pub fn createAny(pool: *Pool) !*anyopaque {
            pool.mutex.lock();
            defer pool.mutex.unlock();
            if (pool.n_free == 0) {
                // we have no free blocks, just allocate a new one
                pool.n_total += 1;
                pool.n_allocs += 1;
                return try pool.alloc.create(Block);
            } else {
                pool.n_free -= 1;
                return pool.free[pool.n_free];
            }
        }

        pub fn create(pool: *Pool, comptime T: type) !*T {
            std.debug.assert(@sizeOf(T) <= config.block_size);
            std.debug.assert(@alignOf(T) <= config.block_size);
            return @alignCast(@ptrCast(try pool.createAny()));
        }

        pub fn destroy(pool: *Pool, ptr: *anyopaque) void {
            pool.mutex.lock();
            defer pool.mutex.unlock();
            const block: *Block = @alignCast(@ptrCast(ptr));
            if (pool.n_free == config.reserve) {
                // we have too many free blocks, destroy this one
                pool.n_total -= 1;
                pool.alloc.destroy(block);
            } else {
                pool.free[pool.n_free] = block;
                pool.n_free += 1;
            }
        }
    };
}

comptime {
    const BP = BlockPool(.{});
    std.debug.assert(@alignOf(BP) == 64);
    std.debug.assert(@sizeOf(BP) <= 64);
}
