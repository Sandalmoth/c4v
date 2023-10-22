const std = @import("std");

const Value = @import("value.zig").Value;

pub const Opcode = enum(u8) {
    // FETCH_LITERAL,
    // LOOKUP_VARIABLE,
    // BIND,
    // BRANCH_IF_FALSE,
    // CALL_PROCEDURE,
    // UNBIND,
    // BRANCH,
    // SAVE_CONTINUATION,
    // APPLY,
    HALT,
};

pub const Instruction = packed struct {
    op: Opcode,
    a: u8 = 255,
    b: u8 = 255,
    c: u8 = 255,
};

pub const Bytecode = struct {
    bc: std.ArrayList(u32),
    constants: std.ArrayList(Value),

    pub fn init(alloc: std.mem.Allocator) Bytecode {
        return .{
            .bc = std.ArrayList(u32).init(alloc),
            .constants = std.ArrayList(Value).init(alloc),
        };
    }

    pub fn deinit(bc: *Bytecode) void {
        bc.bc.deinit();
        bc.constants.deinit();
        bc.* = undefined;
    }

    pub fn push_op(bc: *Bytecode, op: Opcode) void {
        bc.append(
            @bitCast(Instruction{ .op = op }),
        ) catch @panic("Bytecode.push_op: Out of memory");
    }
};
