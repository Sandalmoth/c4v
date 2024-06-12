const std = @import("std");

const nil = std.math.maxInt(u32);

const BOX_SIZE = 32;
comptime {
    // std.debug.assert(@sizeOf(Box) <= BLOCK_SIZE);
    // std.debug.assert(@alignOf(Box) >= BLOCK_SIZE);
}

const DataKind = enum {
    real,
    cons,
};

const Data = union(DataKind) {
    real: f64,
    cons: struct {
        car: u32,
        cdr: u32,
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
                vm.release(cons.car);
                vm.release(cons.cdr);
            },
        }
    }

    fn _hash(box: *Box, vm: *VM) void {
        box.hash = switch (box.data) {
            .real => std.hash.XxHash32.hash(1337, std.mem.asBytes(&box.data.real)),
            .cons => vm.boxPtr(box.data.cons.car).hash ^ vm.boxPtr(box.data.cons.cdr).hash,
        };
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
};

pub fn main() !void {
    std.debug.print("{} {}\n", .{ @sizeOf(Data), @alignOf(Data) });
    std.debug.print("{} {}\n", .{ @sizeOf(Box), @alignOf(Box) });

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var vm = VM.init(alloc);
    defer vm.deinit();

    const a = vm.newReal(1.0);
    std.debug.print("{}\n", .{a});
    std.debug.print("{}\n", .{vm.boxPtr(a)});

    const b = vm.newReal(2.0);
    std.debug.print("{}\n", .{b});
    std.debug.print("{}\n", .{vm.boxPtr(b)});

    const c = vm.newCons(a, b);
    std.debug.print("{}\n", .{c});
    std.debug.print("{}\n", .{vm.boxPtr(c)});

    std.debug.print("{}\n", .{vm.boxPtr(a)});
    std.debug.print("{}\n", .{vm.boxPtr(b)});

    vm.release(c);

    std.debug.print("{}\n", .{vm.boxPtr(a)});
    std.debug.print("{}\n", .{vm.boxPtr(b)});

    const d = vm.newReal(3.0);
    std.debug.print("{}\n", .{d});
    std.debug.print("{}\n", .{vm.boxPtr(d)});

    std.debug.print("{}\n", .{vm.boxPtr(a)});
    std.debug.print("{}\n", .{vm.boxPtr(b)});

    vm.release(d);
    vm.release(a);
    vm.release(b);
}
