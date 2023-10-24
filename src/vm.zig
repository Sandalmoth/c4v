const std = @import("std");

const q = @import("fixedpoint.zig");

const Value = @import("value.zig").Value;

const Chunk = @import("bytecode.zig").Chunk;
const Opcode = @import("bytecode.zig").Opcode;
const Instruction = @import("bytecode.zig").Instruction;
const Instruction_opaS = @import("bytecode.zig").Instruction_opaS;

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
            .LT => @call(.always_tail, op_lt, .{ vm, ip, lits }),
            .JUMP_IF_FALSE => @call(.always_tail, op_jump_if_false, .{ vm, ip, lits }),
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

    pub fn op_lt(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        var x: Instruction = @bitCast(ip[0]);
        vm.registers[x.a] = if (q.lt(vm.registers[x.b].NUMBER, vm.registers[x.c].NUMBER))
            Value{ .TRUE = {} }
        else
            Value{ .FALSE = {} };

        return @call(.always_inline, dispatch, .{ vm, ip + 1, lits });
    }

    pub fn op_jump_if_false(vm: *VM, ip: [*]u32, lits: [*]Value) VMError!void {
        var x: Instruction_opaS = @bitCast(ip[0]);
        var jump: i16 = 1; // if true, just go on to the next instruction
        if (vm.registers[x.a] == .FALSE) {
            jump = x.S;
        }

        // this is really ugly, allow pointer arithmetic with signed numbers please...
        var addr: isize = @intCast(@intFromPtr(ip));
        addr += jump * 4; // note *4 since our instructions are u32, i.e. 4 bytes
        return @call(.always_inline, dispatch, .{
            vm,
            @as([*]u32, @ptrFromInt(@as(usize, @intCast(addr)))),
            lits,
        });
    }
};

test "playground" {
    std.debug.print("\n", .{});

    var bc = Chunk.init(std.testing.allocator);
    defer bc.deinit();

    _ = bc.add_const(Value{ .NUMBER = q.fixedFromInt(0) });
    _ = bc.add_const(Value{ .NUMBER = q.fixedFromInt(1) });
    _ = bc.add_const(Value{ .NUMBER = q.fixedFromInt(67) });

    bc.push_opab(.FETCH_LITERAL, 0, 0);
    bc.push_opab(.FETCH_LITERAL, 1, 1);

    bc.push_opab(.FETCH_LITERAL, 4, 2); // for comparison at loop end
    bc.push_opab(.COPY, 3, 0); // initialize loop variable
    bc.push_opab(.COPY, 5, 1); // for incrementing the loop variable

    // calculate fibonnaci
    bc.push_opabc(.ADD, 2, 0, 1); // reg[2] = reg[0] + reg[1]
    bc.push_opab(.COPY, 0, 1); //    reg[0] = reg[1]
    bc.push_opab(.COPY, 1, 2); //    reg[1] = reg[2]

    // increment loop variable
    bc.push_opabc(.ADD, 3, 3, 5); // reg[2] = reg[0] + reg[1]

    // test if we have looped enough
    bc.push_opabc(.LT, 6, 4, 3);
    bc.push_opaS(.JUMP_IF_FALSE, 6, -5);

    // for (0..100) |_| {
    //     bc.push_opabc(.ADD, 2, 0, 1); // reg[2] = reg[0] + reg[1]
    //     bc.push_opab(.COPY, 0, 1); //    reg[0] = reg[1]
    //     bc.push_opab(.COPY, 1, 2); //    reg[1] = reg[2]
    //     bc.push_opa(.PRINT, 2);
    // }

    bc.push_opa(.PRINT, 2);

    bc.push_op(.HALT);

    exec(std.testing.allocator, bc);
}
