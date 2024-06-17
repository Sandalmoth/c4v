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

    /// checks if two objects are identical by value
    fn eql(rt: *RT, aref: Ref, bref: Ref) bool {
        // is it better to test hash first or kind first?
        // intuition says hash, since there are more options so false equality should be more rare
        if (aref == bref) return true;
        if (rt.hashPtr(aref).* != rt.hashPtr(bref).*) return false;
        if (rt.kindPtr(aref).* != rt.kindPtr(bref).*) return false;

        std.debug.assert(rt.kindPtr(aref).* == rt.kindPtr(bref).*);
        return switch (rt.kindPtr(aref).*) {
            .real => rt.objectPtr(aref).real == rt.objectPtr(bref).real,
            .cons => blk: {
                const a = rt.objectPtr(aref).cons;
                const b = rt.objectPtr(bref).cons;
                break :blk rt.eql(a.car, b.car) and rt.eql(a.cdr, b.cdr);
            },
            .hamt => blk: {
                const a = rt.objectPtr(aref).hamt;
                const b = rt.objectPtr(bref).hamt;
                for (0..8) |i| if (!rt.eql(a.children[i], b.children[i])) break :blk false;
                break :blk true;
            },
        };
    }

    fn hamtAssoc(rt: *RT, ref: Ref, keyref: Ref, valref: Ref) Ref {
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

        for (0..8) |j| {
            if (j != i) {
                rt.obtain(walk_ptr.hamt.children[j]);
                continue;
            }

            const child = walk_ptr.hamt.children[i];
            if (child == nil) {
                walk_ptr.hamt.children[i] = rt.newCons(keyref, valref);
            } else {
                switch (rt.kindPtr(child).*) {
                    .cons => {
                        const cons = rt.objectPtr(child).cons;
                        if (rt.eql(cons.car, keyref)) {
                            // special case where key is aready present
                            walk_ptr.hamt.children[i] = rt.newCons(keyref, valref);
                        } else {
                            // make a new subtree and insert
                            // - the thing we collided with
                            // - the new kv pair
                            // using immutable modifications here is pretty inefficient though...
                            const h0 = rt.newHamt(.{ nil, nil, nil, nil, nil, nil, nil, nil });
                            const h1 = rt._hamtAssocImpl(h0, cons.car, cons.cdr, depth + 1);
                            const h2 = rt._hamtAssocImpl(h1, keyref, valref, depth + 1);
                            rt.release(h0);
                            rt.release(h1);
                            walk_ptr.hamt.children[i] = h2;
                        }
                    },
                    .hamt => {
                        walk_ptr.hamt.children[i] =
                            rt._hamtAssocImpl(child, keyref, valref, depth + 1);
                    },
                    else => unreachable,
                }
            }
        }

        rt._hash(walk);
        return walk;
    }

    fn hamtContains(rt: *RT, ref: Ref, keyref: Ref) bool {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        return rt._hamtContainsImpl(ref, keyref, 0);
    }

    fn _hamtContainsImpl(rt: *RT, ref: Ref, keyref: Ref, depth: u32) bool {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        if (depth > 9) @panic("max depth");

        const hamt = rt.objectPtr(ref).hamt;
        const i = (rt.hashPtr(keyref).* >> @intCast(depth * 3)) & 0b111;
        const child = hamt.children[i];
        if (child == nil) return false;
        return switch (rt.kindPtr(child).*) {
            .cons => blk: {
                const cons = rt.objectPtr(child).cons;
                break :blk rt.eql(cons.car, keyref);
            },
            .hamt => _hamtContainsImpl(rt, child, keyref, depth + 1),
            else => unreachable,
        };
    }

    fn hamtGet(rt: *RT, ref: Ref, keyref: Ref) Ref {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        return rt._hamtGetImpl(ref, keyref, 0);
    }

    fn _hamtGetImpl(rt: *RT, ref: Ref, keyref: Ref, depth: u32) Ref {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        if (depth > 9) @panic("max depth");

        const hamt = rt.objectPtr(ref).hamt;
        const i = (rt.hashPtr(keyref).* >> @intCast(depth * 3)) & 0b111;
        const child = hamt.children[i];
        if (child == nil) return nil;
        return switch (rt.kindPtr(child).*) {
            .cons => blk: {
                const cons = rt.objectPtr(child).cons;
                break :blk if (rt.eql(cons.car, keyref))
                    cons.cdr
                else
                    nil;
            },
            .hamt => _hamtGetImpl(rt, child, keyref, depth + 1),
            else => unreachable,
        };
    }

    fn hamtDissoc(rt: *RT, ref: Ref, keyref: Ref) Ref {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        const result = rt._hamtDissocImpl(ref, keyref, 0);
        if (result == ref) rt.obtain(result); // key not found, return the same map so increase rc
        return result;
    }

    fn _hamtDissocImpl(rt: *RT, ref: Ref, keyref: Ref, depth: u32) Ref {
        std.debug.assert(rt.kindPtr(ref).* == .hamt);
        if (depth > 9) @panic("max depth");

        const hamt = rt.objectPtr(ref).hamt;
        const i = (rt.hashPtr(keyref).* >> @intCast(depth * 3)) & 0b111;
        const child = hamt.children[i];
        if (child == nil) return ref;
        switch (rt.kindPtr(child).*) {
            .cons => {
                const cons = rt.objectPtr(child).cons;
                if (!rt.eql(cons.car, keyref)) return ref;

                // for simplicity, only delete hamt nodes when completely empty
                // even though we could lower the tree height by removing if there is 1 child
                var n: u32 = 0;
                for (hamt.children) |c| {
                    if (c != nil) n += 1;
                }
                std.debug.assert(n > 0);
                if (n == 1) return nil;

                // just delete a node
                const walk = rt._dupNoObtain(ref);
                const walk_ptr = rt.objectPtr(walk);
                walk_ptr.hamt.children[i] = nil;
                for (0..8) |j| rt.obtain(walk_ptr.hamt.children[j]);
                rt._hash(walk);
                return walk;
            },
            .hamt => {
                const result = rt._hamtDissocImpl(child, keyref, depth + 1);
                if (result == child) return ref;
                const walk = rt._dupNoObtain(ref);
                const walk_ptr = rt.objectPtr(walk);
                walk_ptr.hamt.children[i] = result;
                for (0..8) |j| {
                    if (i == j) continue;
                    rt.obtain(walk_ptr.hamt.children[j]);
                }
                rt._hash(walk);
                return walk;
            },
            else => unreachable,
        }
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

        for (hamt.children) |child| {
            if (child == nil) {
                // std.debug.print(", ", .{});
                continue;
            }
            switch (rt.kindPtr(child).*) {
                .cons => {
                    const cons = rt.objectPtr(child).cons;
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
    const h = rt.hamtDissoc(g, a);
    const i = rt.hamtDissoc(h, b);
    const j = rt.hamtDissoc(i, c);

    rt.debugPrint(a);
    rt.debugPrint(b);
    rt.debugPrint(c);
    rt.debugPrint(d);
    rt.debugPrint(e);
    rt.debugPrint(f);
    rt.debugPrint(g);
    rt.debugPrint(h);
    rt.debugPrint(i);
    rt.debugPrint(j);

    std.debug.print("{}\n", .{rt.hamtContains(g, a)});
    std.debug.print("{}\n", .{rt.hamtContains(g, b)});
    std.debug.print("{}\n", .{rt.hamtContains(g, c)});
    std.debug.print("{}\n", .{rt.hamtContains(g, d)});
    std.debug.print("{}\n", .{rt.hamtContains(g, e)});
    std.debug.print("{}\n", .{rt.hamtContains(g, f)});
    std.debug.print("{}\n", .{rt.hamtContains(g, g)});

    rt.debugPrint(rt.hamtGet(g, a));
    rt.debugPrint(rt.hamtGet(g, b));
    rt.debugPrint(rt.hamtGet(g, c));
    rt.debugPrint(rt.hamtGet(g, d));
    rt.debugPrint(rt.hamtGet(g, e));
    rt.debugPrint(rt.hamtGet(g, f));
    rt.debugPrint(rt.hamtGet(g, g));

    std.debug.print("--- object breakdown ---\n", .{});
    for (rt.objects.items, rt.kinds.items, rt.metadata.items, 0..) |obj, kind, meta, z| {
        switch (kind) {
            .real => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.real }),
            .cons => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.cons }),
            .hamt => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.hamt }),
        }
    }

    while (rt.free_list != nil) {
        _ = rt.newReal(0.0);
    }
    _ = rt.newReal(0.0);

    std.debug.print("--- object breakdown ---\n", .{});
    for (rt.objects.items, rt.kinds.items, rt.metadata.items, 0..) |obj, kind, meta, z| {
        switch (kind) {
            .real => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.real }),
            .cons => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.cons }),
            .hamt => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.hamt }),
        }
    }

    rt.release(a);
    rt.release(b);
    rt.release(c);
    rt.release(d);
    rt.release(e);
    rt.release(f);
    rt.release(g);
    rt.release(h);
    rt.release(i);
    rt.release(j);

    while (rt.free_list != nil) {
        _ = rt.newReal(0.0);
    }
    _ = rt.newReal(0.0);

    std.debug.print("--- object breakdown ---\n", .{});
    for (rt.objects.items, rt.kinds.items, rt.metadata.items, 0..) |obj, kind, meta, z| {
        switch (kind) {
            .real => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.real }),
            .cons => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.cons }),
            .hamt => std.debug.print("{:>3} {:>3} {}\n", .{ z, meta.rc, obj.hamt }),
        }
    }
}
