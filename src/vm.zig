const std = @import("std");

const Value = @import("value.zig").Value;

const Bytecode = @import("bytecode.zig").Bytecode;
const Opcode = @import("bytecode.zig").Opcode;
const Instruction = @import("bytecode.zig").Instruction;

pub const VMError = error{RuntimeError};

pub fn exec(alloc: std.mem.Allocator, bc: Bytecode) void {
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
        return .{ .alloc = alloc };
    }

    pub fn dispatch(vm: *VM, ip: [*]u32) VMError {
        var x: Instruction = @bitCast(ip[0]);
        switch (x.op) {
            .HALT => @call(.always_tail, op_halt, .{ vm, ip }),
        }
    }

    pub fn op_halt(vm: *VM, ip: *u32) VMError {
        _ = vm;
        _ = ip;
        return;
    }

    pub fn deinit(vm: *VM) void {
        vm.* = undefined;
    }
};

test "playground" {
    var bc = Bytecode.init(std.testing.allocator);
    defer bc.deinit();
    bc.push_op(.HALT);

    exec(std.testing.allocator, bc);
}
