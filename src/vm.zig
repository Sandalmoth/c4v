const std = @import("std");

const q = @import("fixedpoint.zig");

const Value = @import("value.zig").Value;

const Chunk = @import("bytecode.zig").Chunk;
const Opcode = @import("bytecode.zig").Opcode;
const Instruction = @import("bytecode.zig").Instruction;

pub const VMError = error{RuntimeError};

pub fn exec(alloc: std.mem.Allocator, bc: Chunk) void {
    var vm = VM.init(alloc);
    vm.dispatch(bc.bc.items.ptr, bc.constants.items.ptr) catch |e| {
        std.debug.print("{}\n", .{e});
    };
    defer vm.deinit();
}

pub const VM = struct {
    alloc: std.mem.Allocator,
    registers: [256]Value,

    pub fn init(alloc: std.mem.Allocator) VM {
        return .{
            .alloc = alloc,
            .registers = undefined,
        };
    }

    pub fn dispatch(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        // std.debug.print("{}\n", .{x});

        switch (x.op) {
            .FETCH_LITERAL => @call(.always_tail, op_fetch_literal, .{ vm, ip, lits }),
            .PRINT => @call(.always_tail, op_print, .{ vm, ip, lits }),
            .HALT => @call(.always_tail, op_halt, .{ vm, ip, lits }),

            .ADD => @call(.always_tail, op_add, .{ vm, ip, lits }),
            .COPY => @call(.always_tail, op_copy, .{ vm, ip, lits }),
        }
    }

    pub fn op_fetch_literal(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        vm.registers[x.a] = lits[x.b];

        return @call(.always_inline, dispatch, .{ vm, ip + 1, lits });
    }

    pub fn op_print(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        vm.registers[x.a].print();
        std.debug.print("\n", .{});

        return @call(.always_inline, dispatch, .{ vm, ip + 1, lits });
    }

    pub fn op_halt(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        _ = vm;
        _ = ip;
        _ = lits;
        return;
    }

    pub fn deinit(vm: *VM) void {
        vm.* = undefined;
    }

    pub fn op_add(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        vm.registers[x.a] = Value{
            .NUMBER = q.add(vm.registers[x.b].NUMBER, vm.registers[x.c].NUMBER),
        };

        return @call(.always_inline, dispatch, .{ vm, ip + 1, lits });
    }

    pub fn op_copy(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        vm.registers[x.a] = vm.registers[x.b];

        return @call(.always_inline, dispatch, .{ vm, ip + 1, lits });
    }
};

test "playground" {
    std.debug.print("\n", .{});

    var bc = Chunk.init(std.testing.allocator);
    defer bc.deinit();

    _ = bc.add_const(Value{ .NUMBER = q.fixedFromInt(0) });
    _ = bc.add_const(Value{ .NUMBER = q.fixedFromInt(1) });
    bc.push_opab(.FETCH_LITERAL, 0, 0);
    bc.push_opab(.FETCH_LITERAL, 1, 1);

    for (0..100) |_| {
        bc.push_opabc(.ADD, 2, 0, 1); // reg[2] = reg[0] + reg[1]
        bc.push_opab(.COPY, 0, 1); //    reg[0] = reg[1]
        bc.push_opab(.COPY, 1, 2); //    reg[1] = reg[2]
        bc.push_opa(.PRINT, 2);
    }

    bc.push_op(.HALT);

    exec(std.testing.allocator, bc);
}
