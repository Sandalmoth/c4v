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
        if (ref == nil) return;
        const meta = rt.metadataPtr(ref);
        std.debug.assert(meta.rc > 0);
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

    fn newHamt(rt: *RT, children: [8]Ref) Ref {
        const ref = rt._new();
        const obj = rt.objectPtr(ref);
        for (0..8) |i| {
            rt.obtain(children[i]);
        }
        obj.* = Object{ .hamt = .{
            .children = children,
        } };
        rt.kindPtr(ref).* = .hamt;
        rt._hash(ref);
        return ref;
    }

    fn hamtAssoc(rt: *RT, ref: Ref, keyref: Ref, valref: Ref) Ref {
        std.debug.print("hamtassoc {} {} {}\n", .{ ref, keyref, valref });
        // rt.debugPrint(ref);
        // rt.debugPrint(keyref);
        // rt.debugPrint(valref);
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        return rt._hamtAssocImpl(ref, keyref, valref, 0);
    }

    fn _hamtAssocImpl(rt: *RT, ref: Ref, keyref: Ref, valref: Ref, depth: u32) Ref {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);

        if (depth > 9) @panic("max depth");

        const walk = rt._dupNoObtain(ref);
        const walk_ptr = rt.objectPtr(walk);
        const i = (rt.hashPtr(keyref).* >> @intCast(depth * 3)) & 0b111;

        std.debug.assert(walk != ref);
        std.debug.assert(rt.kindPtr(walk).* == .hamt);

        // std.debug.print(" --- {}\n", .{walk});
        // std.debug.print("{} {}\n", .{ rt.hashPtr(keyref).*, i });

        for (0..8) |j| {
            // std.debug.print("{} {}\n", .{ i, j });
            if (j != i) {
                rt.obtain(walk_ptr.hamt.children[j]);
                continue;
            }

            const child = walk_ptr.hamt.children[i];
            // std.debug.print("{} {}\n", .{ j, child });
            if (child == nil) {
                walk_ptr.hamt.children[i] = rt.newCons(keyref, valref);
                std.debug.print("YOYO\n", .{});
                rt.debugPrint(walk_ptr.hamt.children[i]);
                std.debug.print("YOYO\n", .{});
            } else {
                std.debug.print("COLLISION\n", .{});
            }
        }

        rt._hash(walk);

        std.debug.print("{any}\n", .{walk_ptr.hamt.children});

        return walk;
    }

    fn debugPrint(rt: *RT, ref: Ref) void {
        if (ref == nil) {
            std.debug.print("nil", .{});
        } else {
            rt._debugPrintImpl(ref);
        }
        std.debug.print("\n", .{});
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

    /// make a duplicate of a node, but don't increment any reference counts
    fn _dupNoObtain(rt: *RT, ref: Ref) Ref {
        const r = rt._new();
        // NOTE _new already returns the correct metadata for the duplecate
        rt.objectPtr(r).* = rt.objectPtr(ref).*;
        rt.hashPtr(r).* = rt.hashPtr(ref).*;
        rt.kindPtr(r).* = rt.kindPtr(ref).*;
        return r;
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
                rt._debugPrintHamtImpl(ref);
                std.debug.print("}}", .{});
            },
        }
    }

    fn _debugPrintHamtImpl(rt: *RT, ref: Ref) void {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        const hamt = rt.objectPtr(ref).hamt;

        // std.debug.print("{any}\n", .{hamt.children});
        for (hamt.children) |child| {
            // std.debug.print("{}\n", .{child});
            if (child == nil) {
                std.debug.print(", ", .{});
                continue;
            }
            switch (rt.kindPtr(child).*) {
                .cons => {
                    const cons = rt.objectPtr(child).cons;
                    // std.debug.print("{} {} {}\n", .{ child, cons.car, cons.cdr });
                    // std.debug.print("{}\n", .{rt.kindPtr(cons.car)});
                    // std.debug.print("{}\n", .{rt.kindPtr(cons.cdr)});
                    // NOTE hashmaps can overflow (TODO replace with a list if at max depth)
                    if (cons.car == nil)
                        std.debug.print("nil", .{})
                    else
                        rt._debugPrintImpl(cons.car);
                    std.debug.print(" ", .{});
                    if (cons.cdr == nil)
                        std.debug.print("nil", .{})
                    else
                        rt._debugPrintImpl(cons.cdr);
                },
                .hamt => {
                    rt._debugPrintHamtImpl(child);
                },
                else => unreachable,
            }
            std.debug.print(" ", .{});
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
    const d = rt.newHamt(.{ nil, nil, nil, nil, nil, nil, nil, nil });
    const e = rt.hamtAssoc(d, a, b);
    const f = rt.hamtAssoc(e, b, c);
    const g = rt.hamtAssoc(f, c, a);

    rt.debugPrint(a);
    rt.debugPrint(b);
    rt.debugPrint(c);
    rt.debugPrint(d);
    rt.debugPrint(e);
    rt.debugPrint(f);
    rt.debugPrint(g);

    std.debug.print("--- object breakdown ---\n", .{});
    for (rt.objects.items, rt.kinds.items, 0..) |obj, kind, i| {
        switch (kind) {
            .real => std.debug.print("{:>3} {}\n", .{ i, obj.real }),
            .cons => std.debug.print("{:>3} {}\n", .{ i, obj.cons }),
            .hamt => std.debug.print("{:>3} {}\n", .{ i, obj.hamt }),
        }
    }

    rt.release(a);
    rt.release(b);
    rt.release(c);
    rt.release(d);
    rt.release(e);
    rt.release(f);
    rt.release(g);
}
