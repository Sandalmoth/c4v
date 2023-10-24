const std = @import("std");

const Value = @import("value.zig").Value;

pub const Opcode = enum(u8) {
    FETCH_LITERAL,
    // LOOKUP_VARIABLE,
    // BIND,
    // BRANCH_IF_FALSE,
    // CALL_PROCEDURE,
    // UNBIND,
    // BRANCH,
    // SAVE_CONTINUATION,
    // APPLY,
    PRINT,
    HALT,

    ADD,
    COPY,
    LT,
    JUMP_IF_FALSE,
};

pub const Instruction = packed struct {
    op: Opcode,
    a: u8 = 255,
    b: u8 = 255,
    c: u8 = 255,
};

pub const Instruction_opaS = packed struct {
    op: Opcode,
    a: u8 = 255,
    S: i16 = -32768,
};

pub const Chunk = struct {
    bc: std.ArrayList(u32),
    constants: std.ArrayList(Value),

    pub fn init(alloc: std.mem.Allocator) Chunk {
        return .{
            .bc = std.ArrayList(u32).init(alloc),
            .constants = std.ArrayList(Value).init(alloc),
        };
    }

    pub fn deinit(bc: *Chunk) void {
        bc.bc.deinit();
        bc.constants.deinit();
        bc.* = undefined;
    }

    pub fn push_op(bc: *Chunk, op: Opcode) void {
        bc.bc.append(@bitCast(Instruction{
            .op = op,
        })) catch @panic("Chunk.push_op: Out of memory");
    }

    pub fn push_opa(bc: *Chunk, op: Opcode, a: u8) void {
        bc.bc.append(@bitCast(Instruction{
            .op = op,
            .a = a,
        })) catch @panic("Chunk.push_opa: Out of memory");
    }

    pub fn push_opab(bc: *Chunk, op: Opcode, a: u8, b: u8) void {
        bc.bc.append(@bitCast(Instruction{
            .op = op,
            .a = a,
            .b = b,
        })) catch @panic("Chunk.push_opab: Out of memory");
    }

    pub fn push_opabc(bc: *Chunk, op: Opcode, a: u8, b: u8, c: u8) void {
        bc.bc.append(@bitCast(Instruction{
            .op = op,
            .a = a,
            .b = b,
            .c = c,
        })) catch @panic("Chunk.push_opabc: Out of memory");
    }

    pub fn push_opaS(bc: *Chunk, op: Opcode, a: u8, S: i16) void {
        bc.bc.append(@bitCast(Instruction_opaS{
            .op = op,
            .a = a,
            .S = S,
        })) catch @panic("Chunk.push_opaS: Out of memory");
    }

    pub fn add_const(bc: *Chunk, x: Value) u8 {
        const i = bc.constants.items.len;
        bc.constants.append(x) catch @panic("Chunk.add_const: Ouf of memory");
        return @intCast(i);
    }
};
