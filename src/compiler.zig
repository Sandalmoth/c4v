const std = @import("std");

const Pool = @import("pool.zig").Pool;
const Value = @import("value.zig").Value;
const Chunk = @import("bytecode.zig").Chunk;
const VM = @import("vm.zig").VM;

// const _scanner = @import("scanner.zig");

const Rib = @import("value.zig").Rib;

pub fn compile(compiler: *Compiler, vm: *VM, src: []const u8) Chunk {
    // by having the vm present when compiling
    // we should be able to compile partial code in a historical context
    // similarly to a REPL

    _ = compiler;
    _ = vm;
    _ = src;

    // const sr = _scanner.scan(compiler.scratch_arena.allocator(), src);

}

pub const Compiler = struct {
    alloc: std.mem.Allocator,

    // temporary memory during compilation
    arena_scratch: std.heap.ArenaAllocator,

    pub fn init(alloc: std.mem.Allocator) Compiler {
        var compiler: Compiler = .{
            .alloc = alloc,
            .arena_scratch = undefined,
            .arena_progmem = undefined,
        };
        compiler.arena_scratch = std.heap.ArenaAllocator.init(alloc);
        errdefer compiler.arena_scratch.deinit();

        return compiler;
    }

    pub fn deinit(compiler: *Compiler) void {
        compiler.arena_scratch.deinit();

        compiler.* = undefined;
    }
};

test "dev" {
    std.debug.print("\n", .{});

    var compiler = Compiler.init(std.testing.allocator);
    defer compiler.deinit();

    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();

    const t = try compile(
        \\ (+ 1 2)
    );
    t.print();
}
