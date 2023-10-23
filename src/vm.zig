const std = @import("std");

const Value = @import("value.zig").Value;

const Chunk = @import("bytecode.zig").Chunk;
const Opcode = @import("bytecode.zig").Opcode;
const Instruction = @import("bytecode.zig").Instruction;

pub const VMError = error{RuntimeError};

pub fn exec(alloc: std.mem.Allocator, bc: Chunk) void {
    var vm = VM.init(alloc);
    vm.dispatch(bc.bc.items.ptr) catch |e| {
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

    pub fn dispatch(vm: *VM, ip: [*]u32) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        switch (x.op) {
            .PRINT => @call(.always_tail, op_halt, .{ vm, ip }),
            .HALT => @call(.always_tail, op_halt, .{ vm, ip }),
        }
    }

    pub fn op_print(vm: *VM, ip: [*]u32) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        vm.registers[x.a].print();
        ip += 1;

        return @call(.always_inline, dispatch, .{ vm, ip });
    }

    pub fn op_halt(vm: *VM, ip: [*]u32) VMError!void {
        _ = vm;
        _ = ip;
        return;
    }

    pub fn deinit(vm: *VM) void {
        vm.* = undefined;
    }
};

test "playground" {
    var bc = Chunk.init(std.testing.allocator);
    defer bc.deinit();
    bc.push_op(.HALT);

    exec(std.testing.allocator, bc);
}
