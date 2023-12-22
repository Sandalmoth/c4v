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

    pub fn assoc(gc: *GC, v: Val) *Val {
        _ = gc;
        std.debug.assert(v.tag == .map);
        if (v.val.map.len == 0) {
            std.debug.assert(v.val.map.root == null);
        }
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
        v.* = .{ .tag = .nil, .val = .{ .map = .{ .len = 0, .root = null } } };
        v._hash();
        return v;
    }

    fn _hash(v: *Val) void {
        v.hash = switch (v.tag) {
            .nat => djb2(v.val.nat),
            .cons => v.car().hash ^ v.cdr().hash,
            .map => @panic("not implemented"),
            .primitive => djb2(@intFromPtr(v.val.primitive)),
            // maybe crazy, but, seems reasonable to just use a random number
            .true => 0xb3ab1cc1,
            .false => 0x1ff2ab7c,
            .nil => 0xd9291ffa,
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
    map_node: [8]*?V64,
    map_leaf: [8]*?Val,
};

pub const Block = struct {
    block: V64,
    tag: BlockType,
    hash: u32 = 0,

    pub fn make_map_leaf(gc: *GC) *Block {
        const b = gc.create(Block);
        b.* = .{ .tag = .map_leaf, .block = .{ .map_leaf = null ** 8 } };
        b._hash();
        return b;
    }

    fn _hash(b: *Block) void {
        b.hash = switch (b.tag) {
            .map_node => blk: {
                var h: u32 = 0;
                for (b.block.map_leaf) |l| {
                    if (l != null) {
                        h = h & l.?.hash;
                    }
                }
                break :blk h;
            },
            else => @panic("not implemeted"),
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

    const v_nat = Val.make_nat(&gc, 2701);
    const v_true = Val.make_true(&gc);
    const v_false = Val.make_false(&gc);
    const v_nil = Val.make_nil(&gc);
    const v_cons = Val.make_cons(&gc, v_nat, v_nil);
    try std.testing.expectEqual(@as(u32, 716329095), v_nat.hash);
    try std.testing.expectEqual(@as(u32, 3014335681), v_true.hash);
    try std.testing.expectEqual(@as(u32, 535997308), v_false.hash);
    try std.testing.expectEqual(@as(u32, 3643351034), v_nil.hash);
    try std.testing.expectEqual(@as(u32, 4087041917), v_cons.hash);
}

test "map" {
    std.debug.print("\n", .{});

    var gc = try GC.init(std.testing.allocator);
    defer gc.deinit();

    const m = Val.make_map(&gc);
    std.debug.print("{}\n", .{m.hash});
}

// const q = @import("fixedpoint.zig");

// Based on the very impressive RibbitLisp
// BSD-3-Clause license, https://github.com/udem-dlteam/ribbit/tree/dev
// however, as my usecase isn't size minimalism
// this implementation is significantly different
// and makes no attempt to be compatible

// comptime {
//     // personal memory constraint for the optimization
//     std.debug.assert(@sizeOf(Value) <= 32);
// }

// const Opcode = @import("vm.zig").Opcode;

// pub const Vinstruction = struct {
//     op: Opcode,
//     data: *Value,
//     next: *Vinstruction,
// };

// pub const Vcons = struct {
//     car: *Value,
//     cdr: *Value,
// };

// pub const Vtype = enum {
//     INSTR,
//     CONS,
//     NAT,

//     TRUE,
//     FALSE,
//     NIL,
// };
// pub const Value = union(Vtype) {
//     INSTR: Vinstruction,
//     CONS: Vcons, // pointer to a length 2 array of values
//     NAT: u64, // fixed point number
//     TRUE: void,
//     FALSE: void,
//     NIL: void,

//     pub fn car(x: Value) *Value {
//         return x.CONS[0];
//         // switch (x) {
//         //     .CONS => |cons| return cons[0],
//         //     else => @panic("car only works on a cons"),
//         // }
//     }

//     pub fn cdr(x: Value) *Value {
//         return x.CONS[1];
//         // switch (x) {
//         //     .CONS => |cons| return cons[1],
//         //     else => @panic("cdr only works on a cons"),
//         // }
//     }

//     pub fn truthy(x: Value) bool {
//         return switch (x) {
//             .FALSE, .NIL => false,
//             else => true,
//         };
//     }

//     pub fn is_nil(x: Value) bool {
//         return switch (x) {
//             .NIL => true,
//             else => false,
//         };
//     }

//     pub fn print(x: Value) void {
//         switch (x) {
//             .NUMBER => |y| std.debug.print("{}", .{y}),
//             .CONS => {
//                 std.debug.print("(", .{});
//                 car(x).print();
//                 std.debug.print(" ", .{});
//                 cdr(x).print();
//                 std.debug.print(")", .{});
//             },
//             else => std.debug.print("<{}>", .{x}),
//         }
//     }
// };

// // pub const RibFieldType = enum {
// //     TYPE, // makes more sense to let enums be separate rather than encoded in numbers
// //     OP, // same for the opcode enum
// //     RIB,
// //     NAT,
// //     // REAL,
// // };

// // // TODO benchmark union vs extern union
// // // and probably use extern union in ReleaseFast/Small
// // // but union in Debug/ReleaseSafe
// // pub const RibField = union {
// //     type: ObjectType,
// //     op: Opcode,
// //     rib: *Rib,
// //     nat: u64,
// //     // real: q.Q,
// // };

// // pub const ObjectType = enum {
// //     PAIR,
// //     PROCEDURE,
// //     SYMBOL,
// //     STRING,
// //     VECTOR,
// //     SPECIAL,
// // };

// // // NOTE out of band storage of types makes the extern union based struct significantly smaller
// // // but i think alignment means there's no point to compacting them further
// // pub const Rib = struct {
// //     car: RibField,
// //     cdr: RibField,
// //     tag: RibField,
// //     car_t: RibFieldType,
// //     cdr_t: RibFieldType,
// //     tag_t: RibFieldType,

// //     // pub inline fn car_is(rib: Rib, t: RibFieldType) bool {
// //     //     return rib.car_t == t;
// //     // }
// //     // pub inline fn cdr_is(rib: Rib, t: RibFieldType) bool {
// //     //     return rib.cdr_t == t;
// //     // }
// //     // pub inline fn tag_is(rib: Rib, t: RibFieldType) bool {
// //     //     return rib.tag_t == t;
// //     // }

// //     pub fn setCar(rib: *Rib, comptime t: RibFieldType, car: anytype) *Rib {
// //         rib.car_t = t;
// //         rib.car = switch (t) {
// //             .TYPE => RibField{ .type = car },
// //             .OP => RibField{ .op = car },
// //             .RIB => RibField{ .rib = car },
// //             .NAT => RibField{ .nat = car },
// //         };
// //         return rib;
// //     }

// //     pub fn setCdr(rib: *Rib, comptime t: RibFieldType, cdr: anytype) *Rib {
// //         rib.cdr_t = t;
// //         rib.cdr = switch (t) {
// //             .TYPE => RibField{ .type = cdr },
// //             .OP => RibField{ .op = cdr },
// //             .RIB => RibField{ .rib = cdr },
// //             .NAT => RibField{ .nat = cdr },
// //         };
// //         return rib;
// //     }

// //     pub fn setTag(rib: *Rib, comptime t: RibFieldType, tag: anytype) *Rib {
// //         rib.tag_t = t;
// //         rib.tag = switch (t) {
// //             .TYPE => RibField{ .type = tag },
// //             .OP => RibField{ .op = tag },
// //             .RIB => RibField{ .rib = tag },
// //             .NAT => RibField{ .nat = tag },
// //         };
// //         return rib;
// //     }

// //     pub fn format(
// //         rib: Rib,
// //         comptime fmt: []const u8,
// //         options: std.fmt.FormatOptions,
// //         writer: anytype,
// //     ) !void {
// //         _ = options;
// //         _ = fmt;

// //         // TODO fill in more as needed
// //         switch (rib.tag_t) {
// //             .TYPE => {
// //                 switch (rib.tag.type) {
// //                     // .PAIR => {},
// //                     // .PROCEDURE => {},
// //                     // .SYMBOL => {},
// //                     // .STRING => {},
// //                     // .VECTOR => {},
// //                     .SPECIAL => {
// //                         switch (rib.car.nat) {
// //                             0 => try writer.print("nil", .{}),
// //                             1 => try writer.print("#t", .{}),
// //                             2 => try writer.print("#f", .{}),
// //                             else => @panic("invalid special rib"),
// //                         }
// //                     },
// //                     else => unreachable,
// //                 }
// //             },
// //             // .OP => {},
// //             // .RIB => {},
// //             // .NAT => {},
// //             else => unreachable,
// //             // .RIB => |r| {
// //             // try writer.print("<{} {} {}>", .{ self.car, self.cdr, r });
// //             // },
// //         }
// //     }
// // };

// // pub const ValueType = enum {
// //     CONS,
// //     NUMBER,
// //     STRING,
// //     IDENTIFIER,
// //     TRUE,
// //     FALSE,
// //     NIL,

// //     NEXT,
// // };
// // pub const Value = union(ValueType) {
// //     CONS: [*]Value, // pointer to a length 2 array of values
// //     NUMBER: q.Q, // fixed point number
// //     STRING: u32, // key into interning table?
// //     IDENTIFIER: u32, // key into interning table?
// //     TRUE: void,
// //     FALSE: void,
// //     NIL: void,

// //     NEXT: *Value, // for garbage collection

// //     pub fn car(x: Value) Value {
// //         return x.CONS[0];
// //         // switch (x) {
// //         //     .CONS => |cons| return cons[0],
// //         //     else => @panic("car only works on a cons"),
// //         // }
// //     }

// //     pub fn cdr(x: Value) Value {
// //         return x.CONS[1];
// //         // switch (x) {
// //         //     .CONS => |cons| return cons[1],
// //         //     else => @panic("cdr only works on a cons"),
// //         // }
// //     }

// //     pub fn truthy(x: Value) bool {
// //         return switch (x) {
// //             .FALSE, .NIL => false,
// //             else => true,
// //         };
// //     }

// //     pub fn is_nil(x: Value) bool {
// //         return switch (x) {
// //             .NIL => true,
// //             else => false,
// //         };
// //     }

// //     pub fn print(x: Value) void {
// //         switch (x) {
// //             .NUMBER => |y| std.debug.print("{}", .{y}),
// //             .CONS => {
// //                 std.debug.print("(", .{});
// //                 car(x).print();
// //                 std.debug.print(" ", .{});
// //                 cdr(x).print();
// //                 std.debug.print(")", .{});
// //             },
// //             else => std.debug.print("<{}>", .{x}),
// //         }
// //     }
// // };
