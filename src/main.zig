const std = @import("std");

const nil = std.math.maxInt(u32);
const NILHASH: u32 = 0xb4b4b4b4;

const BOX_SIZE = 32;
comptime {
    // std.debug.assert(@sizeOf(Box) <= BLOCK_SIZE);
    // std.debug.assert(@alignOf(Box) >= BLOCK_SIZE);
}

const DataKind = enum {
    real,
    cons,
    hamt,
};

const Data = union(DataKind) {
    real: f64,
    cons: struct {
        car: u32,
        cdr: u32,
    },
    hamt: struct {
        children: [4]u32,
        leaf: bool,
    },
};

const Box = struct {
    meta: u32, // either a refcount or a pointer (i.e. index)
    hash: u32,
    data: Data,

    /// called by VM.release, do not call manually
    fn _release(box: *Box, vm: *VM) void {
        switch (box.data) {
            .real => {},
            .cons => |cons| {
                std.debug.print("{} {}\n", .{ cons.car, cons.cdr });
                if (cons.car != nil) vm.release(cons.car);
                if (cons.cdr != nil) vm.release(cons.cdr);
            },
            .hamt => |hamt| {
                for (hamt.children) |child| {
                    if (child != nil) vm.release(child);
                }
            },
        }
    }

    fn _hash(box: *Box, vm: *VM) void {
        box.hash = switch (box.data) {
            .real => std.hash.XxHash32.hash(1337, std.mem.asBytes(&box.data.real)),
            .cons => |cons| blk: {
                var h: u32 = 0;
                h ^= if (cons.car == nil) NILHASH else vm.boxPtr(cons.car).hash;
                h *%= 91;
                h ^= if (cons.cdr == nil) NILHASH else vm.boxPtr(cons.cdr).hash;
                break :blk h;
            },
            .hamt => |hamt| blk: {
                var h: u32 = 0;
                for (hamt.children) |child| {
                    h ^= if (child == nil) NILHASH else vm.boxPtr(child).hash;
                    h *%= 89;
                }
                break :blk h;
            },
        };
    }

    fn _debugPrint(box: *Box, vm: *VM) void {
        switch (box.data) {
            .real => std.debug.print("{}", .{box.data.real}),
            .cons => |cons| {
                // TODO special case for list printing?
                std.debug.print("(", .{});
                if (cons.car == nil)
                    std.debug.print("nil", .{})
                else
                    vm.boxPtr(cons.car)._debugPrint(vm);
                std.debug.print(" ", .{});
                if (cons.cdr == nil)
                    std.debug.print("nil", .{})
                else
                    vm.boxPtr(cons.cdr)._debugPrint(vm);
                std.debug.print(")", .{});
            },
            .hamt => {
                std.debug.print("{{", .{});
                box._debugPrintHamtImpl(vm);
                std.debug.print("}}", .{});
            },
        }
    }

    fn _debugPrintHamtImpl(box: *Box, vm: *VM) void {
        const hamt = box.data.hamt;

        for (hamt.children) |child| {
            if (child == nil) continue;
            const x = vm.boxPtr(child);
            switch (x.data) {
                .cons => |cons| {
                    if (cons.car == nil)
                        std.debug.print("nil", .{})
                    else
                        vm.boxPtr(cons.car)._debugPrint(vm);
                    std.debug.print(" ", .{});
                    if (cons.cdr == nil)
                        std.debug.print("nil", .{})
                    else
                        vm.boxPtr(cons.cdr)._debugPrint(vm);
                },
                .hamt => {
                    x._debugPrintHamtImpl(vm);
                },
                else => unreachable,
            }
            std.debug.print(" ", .{});
        }
    }
};

const VM = struct {
    boxes: std.ArrayList(Box),
    free_list: u32 = nil,

    fn init(alloc: std.mem.Allocator) VM {
        return .{
            .boxes = std.ArrayList(Box).init(alloc),
        };
    }

    fn deinit(vm: *VM) void {
        vm.boxes.deinit();
    }

    fn new(vm: *VM) u32 {
        if (vm.free_list == nil) {
            const ref: u32 = @intCast(vm.boxes.items.len);
            vm.boxes.append(.{
                .meta = 1,
                .hash = undefined,
                .data = undefined,
            }) catch @panic("out of memory");
            return ref;
        }

        const ref = vm.free_list;
        const box = vm.boxPtr(ref);
        box._release(vm); // lazy refcounting
        box.meta = 1;
        return ref;
    }

    fn newReal(vm: *VM, value: f64) u32 {
        const ref = vm.new();
        const box = vm.boxPtr(ref);
        box.data = Data{ .real = value };
        box._hash(vm);
        return ref;
    }

    fn newCons(vm: *VM, car: u32, cdr: u32) u32 {
        const ref = vm.new();
        const box = vm.boxPtr(ref);
        vm.obtain(car);
        vm.obtain(cdr);
        box.data = Data{ .cons = .{ .car = car, .cdr = cdr } };
        box._hash(vm);
        return ref;
    }

    fn newHamt(vm: *VM, c0: u32, c1: u32, c2: u32, c3: u32) u32 {
        const ref = vm.new();
        const box = vm.boxPtr(ref);
        vm.obtain(c0);
        vm.obtain(c1);
        vm.obtain(c2);
        vm.obtain(c3);
        box.data = Data{ .hamt = .{
            .children = .{ c0, c1, c2, c3 },
            .leaf = true,
        } };
        box._hash(vm);
        return ref;
    }

    /// add a reference to an existing box
    fn obtain(vm: *VM, ref: u32) void {
        if (ref == nil) return;
        const box = vm.boxPtr(ref);
        box.meta += 1;
    }

    /// remove a reference from an existing box
    fn release(vm: *VM, ref: u32) void {
        const box = vm.boxPtr(ref);
        box.meta -= 1;
        if (box.meta == 0) {
            box.meta = vm.free_list;
            vm.free_list = ref;
        }
    }

    fn boxPtr(vm: *VM, ref: u32) *Box {
        std.debug.assert(ref < vm.boxes.items.len);
        return &vm.boxes.items[ref];
    }

    fn debugPrint(vm: *VM, ref: u32) void {
        if (ref == nil) std.debug.print("nil", .{});
        vm.boxPtr(ref)._debugPrint(vm);
        std.debug.print("\n", .{});
    }
};

pub fn main() !void {
    std.debug.print("{} {}\n", .{ @sizeOf(Data), @alignOf(Data) });
    std.debug.print("{} {}\n", .{ @sizeOf(Box), @alignOf(Box) });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var vm = VM.init(alloc);
    defer vm.deinit();

    const a = vm.newReal(1.0);
    const b = vm.newReal(2.0);

    const m = vm.newHamt(nil, vm.newCons(a, b), nil, nil);
    vm.debugPrint(m);
}
