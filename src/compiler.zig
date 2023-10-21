const std = @import("std");

const Pool = @import("pool.zig").Pool;
const Value = @import("value.zig").Value;
const Parser = @import("parser.zig").Parser;

pub const Thunk = struct {
    pub fn print(thunk: *Thunk) void {
        _ = thunk;
    }
};

pub const Compiler = struct {
    alloc: std.mem.Allocator,
    conses: Pool([2]Value),
    interned: std.StringHashMap(u32),
    intern_data: std.ArrayList([]const u8),

    parser: Parser,

    pub fn init(alloc: std.mem.Allocator) Compiler {
        var compiler: Compiler = .{
            .alloc = alloc,
            .conses = Pool([2]Value).init(alloc),
            .interned = std.StringHashMap(u32).init(alloc),
            .intern_data = std.ArrayList([]const u8).init(alloc),
            .parser = undefined,
        };
        compiler.parser = Parser.init(&compiler);
        compiler.parser.advance();
        return compiler;
    }

    pub fn deinit(compiler: *Compiler) void {
        compiler.intern_data.deinit();
        compiler.interned.deinit();
        compiler.conses.deinit();
        compiler.* = undefined;
    }

    pub fn compile(compiler: *Compiler, src: []const u8) !Thunk {
        var parser = Parser.init(src, &compiler);
        parser.advance();
    }
};

test "dev" {
    std.debug.print("\n", .{});

    var compiler = Compiler.init(std.testing.allocator);
    defer compiler.deinit();

    const t = try compiler.compile(
        \\ (+ 1 2)
    );
    t.print();
}
