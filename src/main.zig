const std = @import("std");

const OBJECT_SIZE = 32;

const Ref = u32;
const nil = std.math.maxInt(Ref);

const Hash = u32;
const NILHASH: Hash = 0xb4b4b4b4;

const Meta = extern union {
    next: Ref,
    rc: u32,
};

const ObjectKind = enum {
    real,
    cons,
    hamt,
};

const Object = extern union {
    real: f64,
    cons: extern struct {
        car: Ref,
        cdr: Ref,
    },
    hamt: extern struct {
        children: [8]Ref align(OBJECT_SIZE),
    },
};

comptime {
    std.debug.assert(@sizeOf(Object) <= OBJECT_SIZE);
    std.debug.assert(@alignOf(Object) >= OBJECT_SIZE);
}

const RT = struct {
    metadata: std.ArrayList(Meta),
    objects: std.ArrayList(Object),
    hashes: std.ArrayList(Hash),
    kinds: std.ArrayList(ObjectKind),

    free_list: Ref = nil,

    fn init(alloc: std.mem.Allocator) RT {
        return .{
            .metadata = std.ArrayList(Meta).init(alloc),
            .objects = std.ArrayList(Object).init(alloc),
            .hashes = std.ArrayList(Hash).init(alloc),
            .kinds = std.ArrayList(ObjectKind).init(alloc),
        };
    }

    fn deinit(rt: *RT) void {
        rt.metadata.deinit();
        rt.objects.deinit();
        rt.hashes.deinit();
        rt.kinds.deinit();
    }

    fn metadataPtr(rt: *RT, ref: u32) *Meta {
        std.debug.assert(ref < rt.metadata.items.len);
        return &rt.metadata.items[ref];
    }

    fn objectPtr(rt: *RT, ref: u32) *Object {
        std.debug.assert(ref < rt.objects.items.len);
        return &rt.objects.items[ref];
    }

    fn hashPtr(rt: *RT, ref: u32) *Hash {
        std.debug.assert(ref < rt.hashes.items.len);
        return &rt.hashes.items[ref];
    }

    fn kindPtr(rt: *RT, ref: u32) *ObjectKind {
        std.debug.assert(ref < rt.kinds.items.len);
        return &rt.kinds.items[ref];
    }

    /// add a reference to an existing object
    fn obtain(rt: *RT, ref: Ref) void {
        if (ref == nil) return;
        const meta = rt.metadataPtr(ref);
        meta.rc += 1;
    }

    /// remove a reference from an existing obj, putting it on the free list if it has no parents
    fn release(rt: *RT, ref: Ref) void {
        const meta = rt.metadataPtr(ref);
        meta.rc -= 1;
        if (meta.rc == 0) {
            meta.next = rt.free_list;
            rt.free_list = ref;
        }
    }

    fn newReal(rt: *RT, value: f64) Ref {
        const ref = rt._new();
        const obj = rt.objectPtr(ref);
        obj.* = Object{ .real = value };
        rt.kindPtr(ref).* = .real;
        rt._hash(ref);
        return ref;
    }

    fn newCons(rt: *RT, car: Ref, cdr: Ref) Ref {
        const ref = rt._new();
        const obj = rt.objectPtr(ref);
        rt.obtain(car);
        rt.obtain(cdr);
        obj.* = Object{ .cons = .{ .car = car, .cdr = cdr } };
        rt.kindPtr(ref).* = .cons;
        rt._hash(ref);
        return ref;
    }

    /// actually release a value in the free_list so that the memory can be reused
    /// called on value in free list when we try to allocate (i.e. lazy refcounting)
    fn _releaseImpl(rt: *RT, ref: Ref) void {
        switch (rt.kindPtr(ref).*) {
            .real => {},
            .cons => {
                const cons = rt.objectPtr(ref).cons;
                if (cons.car != nil) rt.release(cons.car);
                if (cons.cdr != nil) rt.release(cons.cdr);
            },
            .hamt => {
                const hamt = rt.objectPtr(ref).hamt;
                for (hamt.children) |child| {
                    if (child != nil) rt.release(child);
                }
            },
        }
    }

    fn _new(rt: *RT) Ref {
        if (rt.free_list == nil) {
            std.debug.assert(rt.metadata.items.len == rt.objects.items.len);
            std.debug.assert(rt.metadata.items.len == rt.hashes.items.len);
            std.debug.assert(rt.metadata.items.len == rt.kinds.items.len);
            const ref: Ref = @intCast(rt.objects.items.len);
            rt.metadata.append(.{ .rc = 1 }) catch @panic("out of memory");
            rt.objects.append(.{ .hamt = undefined }) catch @panic("out of memory");
            rt.hashes.append(undefined) catch @panic("out of memory");
            rt.kinds.append(undefined) catch @panic("out of memory");
            return ref;
        }

        const ref = rt.free_list;
        rt.free_list = rt.metadataPtr(ref).next;
        rt._releaseImpl(ref); // lazy refcounting
        rt.metadataPtr(ref).rc = 1;
        return ref;
    }

    fn _hash(rt: *RT, ref: Ref) void {
        rt.hashPtr(ref).* = switch (rt.kindPtr(ref).*) {
            .real => std.hash.XxHash32.hash(1337, std.mem.asBytes(&rt.objectPtr(ref).real)),
            .cons => blk: {
                const cons = rt.objectPtr(ref).cons;
                var h: u32 = 0;
                h ^= if (cons.car == nil) NILHASH else rt.hashPtr(cons.car).*;
                h *%= 91;
                h ^= if (cons.cdr == nil) NILHASH else rt.hashPtr(cons.cdr).*;
                break :blk h;
            },
            .hamt => blk: {
                const hamt = rt.objectPtr(ref).hamt;
                var h: u32 = 0;
                for (hamt.children) |child| {
                    h ^= if (child == nil) NILHASH else rt.hashPtr(child).*;
                    h *%= 89;
                }
                break :blk h;
            },
        };
    }

    fn debugPrint(rt: *RT, ref: Ref) void {
        if (ref == nil) std.debug.print("nil", .{});
        rt._debugPrintImpl(ref);
        std.debug.print("\n", .{});
    }

    fn _debugPrintImpl(rt: *RT, ref: Ref) void {
        switch (rt.kindPtr(ref).*) {
            .real => std.debug.print("{}", .{rt.objectPtr(ref).real}),
            .cons => {
                const cons = rt.objectPtr(ref).cons;
                // TODO special case for list printing?
                std.debug.print("(", .{});
                if (cons.car == nil)
                    std.debug.print("nil", .{})
                else
                    rt._debugPrintImpl(cons.car);
                std.debug.print(" . ", .{});
                if (cons.cdr == nil)
                    std.debug.print("nil", .{})
                else
                    rt._debugPrintImpl(cons.cdr);
                std.debug.print(")", .{});
            },
            .hamt => {
                std.debug.print("{{", .{});
                // box._debugPrintHamtImpl(vm);
                std.debug.print("}}", .{});
            },
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var rt = RT.init(alloc);
    defer rt.deinit();

    const a = rt.newReal(1.0);
    const b = rt.newReal(2.0);
    const c = rt.newCons(a, b);

    rt.debugPrint(c);

    rt.release(a);
    rt.release(c);
    rt.release(b);
}
