const std = @import("std");

const q = @import("fixedpoint.zig");

pub const ValueType = enum {
    CONS,
    NUMBER,
    STRING,
    IDENTIFIER,
    TRUE,
    FALSE,
    NIL,

    NEXT,
};
pub const Value = union(ValueType) {
    CONS: [*]Value, // pointer to a length 2 array of values
    NUMBER: q.Q, // Q32.32 fixed point
    STRING: u32, // key into interning table?
    IDENTIFIER: u32, // key into interning table?
    TRUE: void,
    FALSE: void,
    NIL: void,

    NEXT: *Value, // for garbage collection

    pub fn car(x: Value) Value {
        switch (x) {
            .CONS => |cons| return cons[0],
            else => @panic("car only works on a cons"),
        }
    }

    pub fn cdr(x: Value) Value {
        switch (x) {
            .CONS => |cons| return cons[1],
            else => @panic("cdr only works on a cons"),
        }
    }

    pub fn truthy(x: Value) bool {
        return switch (x) {
            .FALSE => false,
            else => true,
        };
    }

    pub fn is_nil(x: Value) bool {
        return switch (x) {
            .NIL => true,
            else => false,
        };
    }
};
