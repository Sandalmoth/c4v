const std = @import("std");

const GC = @import("gc.zig").GarbageCollector(.{});

fn djb2(x: anytype) u32 {
    var h: u32 = 5381;

    if (@TypeOf(x) == u64) {
        for (0..8) |i| {
            const c: u32 = @truncate((x >> @intCast(i)) & 0xff);
            h = (h << 5) +% h +% c;
        }
        return h;
    }

    if (@TypeOf(x) == usize) { // FIXME danger danger for 32 bit systems?
        for (0..8) |i| {
            const c: u32 = @truncate((x >> @intCast(i)) & 0xff);
            h = (h << 5) +% h +% c;
        }
        return h;
    }

    @compileError("trying to call djb2 for unimplemented type " ++ @typeName(@TypeOf(x)));
}

pub const ValType = enum {
    nat,
    cons,
    map,
    primitive,

    true,
    false,
    nil,
    // err,
};

pub const Primitive = *fn () void;

// should be extern for optimizaiton
pub const V16 = union {
    // pub const V16 = extern union {
    nat: u64,
    cons: [2]*Val,
    map: Map,
    primitive: Primitive,
};

const Map = struct {
    len: usize,
    root: ?*Block,
};

pub const Val = struct {
    val: V16,
    tag: ValType,
    hash: u32 = 0,

    pub fn car(v: Val) *Val {
        std.debug.assert(v.tag == .cons);
        return v.val.cons[0];
    }

    pub fn cdr(v: Val) *Val {
        std.debug.assert(v.tag == .cons);
        return v.val.cons[1];
    }

    pub fn get(v: Val) *Val {
        std.debug.assert(v.tag == .map);
    }

    pub fn assoc(m: Val, gc: *GC, k: *Val, v: *Val) *Val {
        std.debug.assert(m.tag == .map);

        const n = make_map(gc);
        n.* = m;
        n.val.map.len += 1;

        if (m.val.map.len == 0) {
            // if map is empty, create a first leaf node
            std.debug.assert(m.val.map.root == null);
            n.val.map.root = Block.make_map_leaf(gc);
        } else {
            // otherwise, copy the root, preserving the old map
            n.val.map.root = gc.create(Block);
            n.val.map.root.?.* = m.val.map.root.?.*;
        }

        // then insert into that new/copied root
        const kv = make_cons(gc, k, v);
        n.val.map.root = n.val.map.root.?.assoc(gc, kv, 0);
        n._hash();
        return n;
    }

    pub fn truthy(v: Val) bool {
        return switch (v.tag) {
            .false, .nil => false,
            else => true,
        };
    }

    pub fn make_nat(gc: *GC, n: u64) *Val {
        const v = gc.create(Val);
        v.* = .{ .tag = .nat, .val = .{ .nat = n } };
        v._hash();
        return v;
    }

    pub fn make_true(gc: *GC) *Val {
        const v = gc.create(Val);
        v.* = .{ .tag = .true, .val = undefined };
        v._hash();
        return v;
    }

    pub fn make_false(gc: *GC) *Val {
        const v = gc.create(Val);
        v.* = .{ .tag = .false, .val = undefined };
        v._hash();
        return v;
    }

    pub fn make_nil(gc: *GC) *Val {
        const v = gc.create(Val);
        v.* = .{ .tag = .nil, .val = undefined };
        v._hash();
        return v;
    }

    pub fn make_cons(gc: *GC, _car: *Val, _cdr: *Val) *Val {
        const v = gc.create(Val);
        v.* = .{ .tag = .cons, .val = .{ .cons = undefined } };
        v.val.cons[0] = _car;
        v.val.cons[1] = _cdr;
        v._hash();
        return v;
    }

    pub fn make_map(gc: *GC) *Val {
        const v = gc.create(Val);
        v.* = .{ .tag = .map, .val = .{ .map = .{ .len = 0, .root = null } } };
        v._hash();
        return v;
    }

    fn _hash(v: *Val) void {
        v.hash = switch (v.tag) {
            .nat => djb2(v.val.nat),
            .cons => 0x49249249 ^ v.car().hash ^ v.cdr().hash,
            .map => blk: {
                var h: u32 = 0x92492492;
                if (v.val.map.root) |root| {
                    h = h ^ root.hash;
                }
                break :blk h;
            },
            .primitive => djb2(@intFromPtr(v.val.primitive)),
            .true => 0xdb6db6db,
            .false => 0xb6db6db6,
            .nil => 0x00000000,
        };
    }

    pub inline fn trace(v: Val, gc: *GC) void {
        switch (v.tag) {
            .cons => {
                gc.trace(Val, v.car());
                gc.trace(Val, v.cdr());
            },
            .map => {
                if (v.val.root) |root| {
                    gc.trace(root);
                }
            },
            else => {},
        }
    }
};

pub fn eql(a: Val, b: Val) bool {
    if (a.tag != b.tag) {
        return false;
    }
    if (a.hash != b.hash) {
        return false;
    }

    return switch (a) {
        .nat => a.nat == b.nat,
        .cons => eql(a.car().*, b.car().*) and eql(a.car().*, b.car().*),
        .map => @panic("not implemented"),
        .primitive => a.primitive == b.primitive,
        .true, .false, .nil => true, // payload free Vals are equal if the tag is equal
    };
}

pub const BlockType = enum {
    map_node,
    map_leaf,
};

// should be extern for optimizaiton
pub const V64 = union {
    // pub const V64 = extern union {
    map_node: [8]?*Block,
    map_leaf: [8]?*Val,
};

pub const Block = struct {
    block: V64,
    tag: BlockType,
    hash: u32 = 0,

    pub fn make_map_node(gc: *GC) *Block {
        const b = gc.create(Block);
        b.* = .{ .tag = .map_node, .block = .{ .map_leaf = [_]?*Val{null} ** 8 } };
        b._hash();
        return b;
    }

    pub fn make_map_leaf(gc: *GC) *Block {
        const b = gc.create(Block);
        b.* = .{ .tag = .map_leaf, .block = .{ .map_leaf = [_]?*Val{null} ** 8 } };
        b._hash();
        return b;
    }

    fn assoc(walk: *Block, gc: *GC, kv: *Val, depth: u5) *Block {
        // in the case that we're inserting into a full leaf
        // that is not at bottom level
        // return a new node in the tree
        // otherwise, return walk
        const loc = (kv.hash >> (3 * depth)) & 0x7;
        std.debug.print("in block assoc, loc={} depth={}\n", .{ loc, depth });
        switch (walk.tag) {
            .map_node => {
                std.debug.print("  map_node ", .{});
                if (walk.block.map_node[loc] == null) {
                    std.debug.print("null\n", .{});
                    // create a new empty leaf
                    walk.block.map_node[loc] = Block.make_map_leaf(gc);
                } else {
                    std.debug.print("copy\n", .{});
                    // copy existing block (node or leaf)
                    const new_leaf = gc.create(Block);
                    new_leaf.* = walk.block.map_node[loc].?.*;
                    walk.block.map_node[loc] = new_leaf;
                }
                // then insert into the new leaf or copied node/leaf
                var new = walk.block.map_node[loc].?.assoc(gc, kv, depth + 1);
                new._hash();
                return new;
            },
            .map_leaf => {
                std.debug.print("  map_leaf ", .{});
                if (walk.block.map_leaf[loc] == null) {
                    std.debug.print("null\n", .{});
                    // slot is empty, just insert
                    walk.block.map_leaf[loc] = kv;
                    walk._hash();
                    return walk;
                } else {
                    std.debug.print("transfer\n", .{});
                    // create a new node and transfer existing kv
                    var new_node = Block.make_map_node(gc);
                    for (walk.block.map_leaf) |_kv| {
                        if (_kv != null) {
                            _ = new_node.assoc(gc, _kv.?, depth + 1);
                        }
                    }
                    // finally, insert our kv into that
                    new_node = new_node.assoc(gc, kv, depth + 1);
                    new_node._hash();
                    return new_node;
                }
            },
        }
    }

    fn _hash(b: *Block) void {
        b.hash = switch (b.tag) {
            .map_node => blk: {
                var h: u32 = 0;
                for (b.block.map_node) |n| {
                    if (n != null) {
                        h = h ^ n.?.hash;
                    }
                }
                break :blk h;
            },
            .map_leaf => blk: {
                var h: u32 = 0;
                for (b.block.map_leaf) |l| {
                    if (l != null) {
                        h = h ^ l.?.hash;
                    }
                }
                break :blk h;
            },
        };
    }

    // pub inline fn trace(v: Val, gc: *GC) void {
    //     switch (v.tag) {
    //         .cons => {
    //             gc.trace(Val, v.car());
    //             gc.trace(Val, v.cdr());
    //         },
    //         .map => {
    //             if (v.val.root) |root| {
    //                 gc.trace(root);
    //             }
    //         },
    //         else => {},
    //     }
    // }
};

comptime {
    // switch to extern union in release mode
    // and enable size asserts in release mode
    // std.debug.assert(@sizeOf(Val) == 24);
    // std.debug.assert(@sizeOf(Block) == 70);
}

test "hash" {
    std.debug.print("\n", .{});
    std.debug.print("{} {}\n", .{ @sizeOf(Val), @alignOf(Val) });
    std.debug.print("{} {}\n", .{ @sizeOf(Block), @alignOf(Block) });

    var gc = try GC.init(std.testing.allocator);
    defer gc.deinit();

    // const v_nat = Val.make_nat(&gc, 2701);
    const v_true = Val.make_true(&gc);
    const v_false = Val.make_false(&gc);
    const v_nil = Val.make_nil(&gc);
    // const v_cons = Val.make_cons(&gc, v_nat, v_nil);
    // try std.testing.expectEqual(@as(u32, 716329095), v_nat.hash);
    try std.testing.expectEqual(@as(u32, 0xdb6db6db), v_true.hash);
    try std.testing.expectEqual(@as(u32, 0xb6db6db6), v_false.hash);
    try std.testing.expectEqual(@as(u32, 0x00000000), v_nil.hash);
    // try std.testing.expectEqual(@as(u32, 4087041917), v_cons.hash);
}

test "map" {
    std.debug.print("\n", .{});

    var gc = try GC.init(std.testing.allocator);
    defer gc.deinit();

    const m0 = Val.make_map(&gc);
    std.debug.print("{*}\n", .{m0.val.map.root});
    std.debug.print("{} {}\n", .{ m0.hash, m0.val.map.len });
    const m1 = m0.assoc(&gc, Val.make_nat(&gc, 123), Val.make_nat(&gc, 321));
    std.debug.print("{*}\n", .{m1.val.map.root});
    std.debug.print("{}\n", .{m1.val.map.root.?.*});
    std.debug.print("{} {}\n", .{ m1.hash, m1.val.map.len });
    const m2 = m1.assoc(&gc, Val.make_nat(&gc, 234), Val.make_nat(&gc, 432));
    std.debug.print("{*}\n", .{m2.val.map.root});
    std.debug.print("{}\n", .{m2.val.map.root.?.*});
    std.debug.print("{} {}\n", .{ m2.hash, m2.val.map.len });
}
