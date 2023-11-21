const std = @import("std");

// const q = @import("fixedpoint.zig");

// Based on the very impressive RibbitLisp
// BSD-3-Clause license, https://github.com/udem-dlteam/ribbit/tree/dev
// however, as my usecase isn't size minimalism
// this implementation is significantly different
// and makes no attempt to be compatible

const Opcode = @import("vm.zig").Opcode;

pub const RibFieldType = enum {
    TYPE, // makes more sense to let enums be separate rather than encoded in numbers
    OP, // same for the opcode enum
    RIB,
    NAT,
    // REAL,
};

// TODO benchmark union vs extern union
// and probably use extern union in ReleaseFast/Small
// but union in Debug/ReleaseSafe
pub const RibField = union {
    type: ObjectType,
    op: Opcode,
    rib: *Rib,
    nat: u64,
    // real: q.Q,
};

pub const ObjectType = enum {
    PAIR,
    PROCEDURE,
    SYMBOL,
    STRING,
    VECTOR,
    SPECIAL,
};

// NOTE out of band storage of types makes the extern union based struct significantly smaller
// but i think alignment means there's no point to compacting them further
pub const Rib = struct {
    car: RibField,
    cdr: RibField,
    tag: RibField,
    car_t: RibFieldType,
    cdr_t: RibFieldType,
    tag_t: RibFieldType,

    // pub inline fn car_is(rib: Rib, t: RibFieldType) bool {
    //     return rib.car_t == t;
    // }
    // pub inline fn cdr_is(rib: Rib, t: RibFieldType) bool {
    //     return rib.cdr_t == t;
    // }
    // pub inline fn tag_is(rib: Rib, t: RibFieldType) bool {
    //     return rib.tag_t == t;
    // }

    pub fn setCar(rib: *Rib, comptime t: RibFieldType, car: anytype) *Rib {
        rib.car_t = t;
        rib.car = switch (t) {
            .TYPE => RibField{ .type = car },
            .OP => RibField{ .op = car },
            .RIB => RibField{ .rib = car },
            .NAT => RibField{ .nat = car },
        };
        return rib;
    }

    pub fn setCdr(rib: *Rib, comptime t: RibFieldType, cdr: anytype) *Rib {
        rib.cdr_t = t;
        rib.cdr = switch (t) {
            .TYPE => RibField{ .type = cdr },
            .OP => RibField{ .op = cdr },
            .RIB => RibField{ .rib = cdr },
            .NAT => RibField{ .nat = cdr },
        };
        return rib;
    }

    pub fn setTag(rib: *Rib, comptime t: RibFieldType, tag: anytype) *Rib {
        rib.tag_t = t;
        rib.tag = switch (t) {
            .TYPE => RibField{ .type = tag },
            .OP => RibField{ .op = tag },
            .RIB => RibField{ .rib = tag },
            .NAT => RibField{ .nat = tag },
        };
        return rib;
    }

    pub fn format(
        rib: Rib,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        // TODO fill in more as needed
        switch (rib.tag_t) {
            .TYPE => {
                switch (rib.tag.type) {
                    // .PAIR => {},
                    // .PROCEDURE => {},
                    // .SYMBOL => {},
                    // .STRING => {},
                    // .VECTOR => {},
                    .SPECIAL => {
                        switch (rib.car.nat) {
                            0 => try writer.print("nil", .{}),
                            1 => try writer.print("#t", .{}),
                            2 => try writer.print("#f", .{}),
                            else => @panic("invalid special rib"),
                        }
                    },
                    else => unreachable,
                }
            },
            // .OP => {},
            // .RIB => {},
            // .NAT => {},
            else => unreachable,
            // .RIB => |r| {
            // try writer.print("<{} {} {}>", .{ self.car, self.cdr, r });
            // },
        }
    }
};

// pub const ValueType = enum {
//     CONS,
//     NUMBER,
//     STRING,
//     IDENTIFIER,
//     TRUE,
//     FALSE,
//     NIL,

//     NEXT,
// };
// pub const Value = union(ValueType) {
//     CONS: [*]Value, // pointer to a length 2 array of values
//     NUMBER: q.Q, // fixed point number
//     STRING: u32, // key into interning table?
//     IDENTIFIER: u32, // key into interning table?
//     TRUE: void,
//     FALSE: void,
//     NIL: void,

//     NEXT: *Value, // for garbage collection

//     pub fn car(x: Value) Value {
//         return x.CONS[0];
//         // switch (x) {
//         //     .CONS => |cons| return cons[0],
//         //     else => @panic("car only works on a cons"),
//         // }
//     }

//     pub fn cdr(x: Value) Value {
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
