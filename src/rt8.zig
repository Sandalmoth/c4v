const std = @import("std");
const BlockPool = @import("blockpool.zig").BlockPool(.{});

const GC = @import("gc8.zig").GC;
const Kind = @import("gc8.zig").Kind;
const Object = @import("gc8.zig").Object;
const NILHASH = @import("gc8.zig").NILHASH;

pub const RT = struct {
    gc: GC,

    pub fn init(pool: *BlockPool) !RT {
        return .{ .gc = try GC.init(pool) };
    }

    pub fn deinit(rt: *RT) void {
        rt.gc.deinit();
        rt.* = undefined;
    }

    pub fn newReal(rt: *RT, value: f64) *Object {
        const obj = rt.gc.alloc(.real, 0) catch @panic("GC allocation failure");
        obj.data = value;
        return rt.gc.commit(.real, obj);
    }

    pub fn newCons(rt: *RT, car: ?*Object, cdr: ?*Object) *Object {
        const obj = rt.gc.alloc(.cons, 0) catch @panic("GC allocation failure");
        obj.car = car;
        obj.cdr = cdr;
        return rt.gc.commit(.cons, obj);
    }

    pub fn newChamp(rt: *RT) *Object {
        const obj = rt.gc.alloc(.champ, 0) catch @panic("GC allocation failure");
        obj.datamask = 0;
        obj.nodemask = 0;
        obj.datalen = 0;
        obj.nodelen = 0;
        return rt.gc.commit(.champ, obj);
    }

    pub fn newString(rt: *RT, value: []const u8) *Object {
        const obj = rt.gc.alloc(.string, value.len) catch @panic("GC allocation failure");
        obj.len = value.len;
        @memcpy(obj.data(), value);
        return rt.gc.commit(.string, obj);
    }

    pub fn champAssoc(rt: *RT, objchamp: ?*Object, objkey: ?*Object, objval: ?*Object) *Object {
        // inserting into nil just creates a new map
        const obj = objchamp orelse rt.newChamp();
        return rt._champAssocImpl(obj, objkey, objval, 0);
    }
    fn _champAssocImpl(
        rt: *RT,
        objchamp: *Object,
        objkey: ?*Object,
        objval: ?*Object,
        depth: usize,
    ) *Object {
        const champ = objchamp.as(.champ);
        const slot = if (objkey) |k| k.getHashAtDepth(depth) else NILHASH & 0b11_1111;
        const slotmask = @as(u64, 1) << @intCast(slot);
        const is_data = champ.datamask & slotmask > 0;
        const is_node = champ.nodemask & slotmask > 0;
        std.debug.assert(!(is_data and is_node));

        if (is_node) {
            // traverse and insert further down
            const packed_index = @popCount(champ.nodemask & (slotmask - 1));
            const new = rt.gc.alloc(.champ, 2 * champ.datalen + champ.nodelen) catch
                @panic("GC allocation failure");
            new.datamask = champ.datamask;
            new.nodemask = champ.nodemask;
            new.datalen = champ.datalen;
            new.nodelen = champ.nodelen;
            @memcpy(new.data()[0..], champ.data()[0 .. 2 * champ.datalen + champ.nodelen]);
            new.nodes()[packed_index] = rt._champAssocImpl(
                champ.nodes()[packed_index],
                objkey,
                objval,
                depth + 1,
            );
            return rt.gc.commit(.champ, new);
        }

        if (!is_data) {
            // empty slot, insert here
            const packed_index = @popCount(champ.datamask & (slotmask - 1));
            const new = rt.gc.alloc(.champ, 2 * (champ.datalen + 1) + champ.nodelen) catch
                @panic("GC allocation failure");
            new.datamask = champ.datamask | slotmask;
            new.nodemask = champ.nodemask;
            new.datalen = champ.datalen + 1;
            new.nodelen = champ.nodelen;
            const newdata = new.data();
            const olddata = champ.data();
            @memcpy(newdata[0..], olddata[0 .. 2 * packed_index]);
            newdata[2 * packed_index] = objkey;
            newdata[2 * packed_index + 1] = objval;
            @memcpy(
                newdata[2 * packed_index + 2 ..],
                olddata[2 * packed_index .. 2 * champ.datalen + champ.nodelen],
            );
            return rt.gc.commit(.champ, new);
        }

        const packed_index = @popCount(champ.datamask & (slotmask - 1));
        if (rt.eql(champ.data()[2 * packed_index], objkey)) {
            // key already present, just update
            const new = rt.gc.alloc(.champ, 2 * champ.datalen + champ.nodelen) catch
                @panic("GC allocation failure");
            new.datamask = champ.datamask;
            new.nodemask = champ.nodemask;
            new.datalen = champ.datalen;
            new.nodelen = champ.nodelen;
            @memcpy(new.data()[0..], champ.data()[0 .. 2 * champ.datalen + champ.nodelen]);
            new.data()[2 * packed_index + 1] = objval;
            return rt.gc.commit(.champ, new);
        }

        // add new sublevel with displaced child
        const packed_data_index = @popCount(champ.datamask & (slotmask - 1));
        const packed_node_index = @popCount(champ.nodemask & (slotmask - 1));
        const subkey = champ.data()[2 * packed_data_index];
        const subval = champ.data()[2 * packed_data_index + 1];
        const subslot = if (subkey) |k| k.getHashAtDepth(depth + 1) else NILHASH & 0b11_1111;
        const sub = rt.gc.alloc(.champ, 2) catch @panic("GC allocation failure");
        sub.datamask = @as(u64, 1) << @intCast(subslot);
        sub.nodemask = 0;
        sub.datalen = 1;
        sub.nodelen = 0;
        const subdata = sub.data();
        subdata[0] = subkey;
        subdata[1] = subval;

        // then insert into that sublevel
        const new = rt.gc.alloc(.champ, 2 * (champ.datalen - 1) + champ.nodelen + 1) catch
            @panic("GC allocation failure");
        new.datamask = champ.datamask & ~slotmask;
        new.nodemask = champ.nodemask | slotmask;
        new.datalen = champ.datalen - 1;
        new.nodelen = champ.nodelen + 1;
        const newdata = new.data();
        const olddata = champ.data();
        @memcpy(newdata[0..], olddata[0 .. 2 * packed_data_index]);
        @memcpy(
            newdata[2 * packed_data_index ..],
            olddata[2 * packed_data_index + 2 .. 2 * champ.datalen],
        );
        const newnodes = new.nodes();
        const oldnodes = champ.nodes();
        @memcpy(newnodes[0..], oldnodes[0..packed_node_index]);
        newnodes[packed_node_index] = rt.gc.commit(.champ, sub);
        @memcpy(newnodes[packed_node_index + 1 ..], oldnodes[packed_node_index..champ.nodelen]);

        return rt.gc.commit(.champ, new);

        // const packed_index = @popCount(hamt.mask & ((@as(u64, 1) << @intCast(slot)) - 1));
        // if (hamt.mask & (@as(u64, 1) << @intCast(slot)) == 0) {
        //     // empty slot, insert here
        //     const new = rt.gc.alloc(.hamt, hamt.len() + 1) catch @panic("GC allocation failure");
        //     new.mask = hamt.mask | (@as(u64, 1) << @intCast(slot));
        //     // shuffle the entries in the old hamt to make space
        //     @memcpy(new.data(), hamt.data()[0..packed_index]);
        //     new.data()[packed_index] = rt.newCons(objkey, objval);
        //     @memcpy(new.data()[packed_index + 1 ..], hamt.data()[packed_index..hamt.len()]);
        //     return rt.gc.commit(.hamt, new);
        // }

        // // occupied slot, either insert, traverse, or create a new subtree and traverse
        // const child = hamt.data()[packed_index];
        // const new = rt.gc.alloc(.hamt, hamt.len()) catch @panic("GC allocation failure");
        // new.mask = hamt.mask;
        // @memcpy(new.data(), hamt.data()[0..hamt.len()]);
        // switch (child.getKind()) {
        //     .cons => {
        //         const cons = child.as(.cons);
        //         if (rt.eql(cons.car, objkey)) {
        //             // replace key in this slot
        //             new.data()[packed_index] = rt.newCons(objkey, objval);
        //         } else {
        //             // generate new subtree and traverse
        //             // TODO? try fastpath if cons.car and objkey doesn't collide at depth + 1
        //             const subslot = if (cons.car) |car|
        //                 car.getHashAtDepth(depth + 1)
        //             else
        //                 NILHASH & 0b11_1111;
        //             const sub = rt.gc.alloc(.hamt, 1) catch @panic("GC allocation failure");
        //             sub.mask = @as(u64, 1) << @intCast(subslot);
        //             sub.data()[0] = child;
        //             new.data()[packed_index] = rt._hamtAssocImpl(
        //                 rt.gc.commit(.hamt, sub),
        //                 objkey,
        //                 objval,
        //                 depth + 1,
        //             );
        //         }
        //     },
        //     .hamt => {
        //         new.data()[packed_index] = rt._hamtAssocImpl(child, objkey, objval, depth + 1);
        //     },
        //     else => unreachable,
        // }
        // return rt.gc.commit(.hamt, new);
    }

    pub fn hamtGet(rt: *RT, objhamt: ?*Object, objkey: ?*Object) ?*Object {
        const obj = objhamt orelse return null;
        return rt._hamtGetImpl(obj, objkey, 0);
    }
    pub fn _hamtGetImpl(rt: *RT, objhamt: *Object, objkey: ?*Object, depth: usize) ?*Object {
        // debugPrint(objhamt);
        const hamt = objhamt.as(.hamt);
        const slot = if (objkey) |k| k.getHashAtDepth(depth) else NILHASH & 0b11_1111;
        // std.debug.print("{} {} {b}\n", .{ hamt.len(), slot, hamt.mask });
        if (hamt.mask & (@as(u64, 1) << @intCast(slot)) == 0) return null;
        const packed_index = @popCount(hamt.mask & ((@as(u64, 1) << @intCast(slot)) - 1));
        const child = hamt.data()[packed_index];

        // for (0..hamt.len()) |i| {
        //     if (i == packed_index) {
        //         std.debug.print("### {} ###\t", .{i});
        //     } else {
        //         std.debug.print("--- {} ---\t", .{i});
        //     }
        //     debugPrint(hamt.data()[i]);
        // }

        return switch (child.getKind()) {
            .cons => blk: {
                const cons = child.as(.cons);
                break :blk if (rt.eql(cons.car, objkey)) cons.cdr else null;
            },
            .hamt => rt._hamtGetImpl(child, objkey, depth + 1),
            else => unreachable,
        };
    }

    pub fn hamtContains(rt: *RT, objhamt: ?*Object, objkey: ?*Object) bool {
        const obj = objhamt orelse return false;
        return rt._hamtGetImpl(obj, objkey, 0) != null;
    }

    // NOTE This implementation does not maintain a canonical representation
    // to compensate for this, eql needs an (inefficient) implementation
    pub fn hamtDissoc(rt: *RT, objhamt: ?*Object, objkey: ?*Object) ?*Object {
        const obj = objhamt orelse return null;
        const result = rt._hamtDissocImpl(obj, objkey, 0);
        if (result == null) return rt.newHamt();
        if (result.?.getKind() == .cons) {
            const cons = result.?.as(.cons);
            const slot = if (cons.car) |k| k.getHashAtDepth(0) else NILHASH & 0b11_1111;
            const new = rt.gc.alloc(.hamt, 1) catch @panic("GC allocation failure");
            new.mask = (@as(u64, 1) << @intCast(slot));
            new.data()[0] = result.?;
            return rt.gc.commit(.hamt, new);
        }
        return result.?;
    }
    pub fn _hamtDissocImpl(rt: *RT, objhamt: *Object, objkey: ?*Object, depth: usize) ?*Object {
        const hamt = objhamt.as(.hamt);
        const slot = if (objkey) |k| k.getHashAtDepth(depth) else NILHASH & 0b11_1111;
        // if child slot for key is not occupied, return with no changes
        if (hamt.mask & (@as(u64, 1) << @intCast(slot)) == 0) return objhamt;
        const packed_index = @popCount(hamt.mask & ((@as(u64, 1) << @intCast(slot)) - 1));
        const child = hamt.data()[packed_index];

        switch (child.getKind()) {
            .cons => {
                const cons = child.as(.cons);
                if (!rt.eql(cons.car, objkey)) return objhamt; // key is not present
                // key is present, actually delete something
                // handle cases where this hamt level isn't needed after the delete
                if (hamt.len() == 1) return null;
                // // FIXME
                // // the error is that, if the child we return here is a hamt
                // // then that hamt is structured at depth + 1
                // // but if replacing this node, it should be structured at depth
                // if (hamt.len() == 2) return hamt.data()[if (packed_index == 0) 1 else 0];
                // delete and preserve level
                const new = rt.gc.alloc(.hamt, hamt.len() - 1) catch
                    @panic("GC allocation failure");
                new.mask = hamt.mask & ~(@as(u64, 1) << @intCast(slot));
                @memcpy(new.data(), hamt.data()[0..packed_index]);
                @memcpy(new.data()[packed_index..], hamt.data()[packed_index + 1 .. hamt.len()]);
                return rt.gc.commit(.hamt, new);
            },
            .hamt => {
                // recur and if a deletion happened at the sublevel, update to include it
                const result = rt._hamtDissocImpl(child, objkey, depth + 1);
                if (rt.eql(child, result)) return objhamt;
                if (result == null) {
                    // if the sublevel deletion resulted in a node deletion
                    // we need to downsize (and possibly delete) ourselves as well
                    if (hamt.len() == 1) return null;
                    // // FIXME
                    // if (hamt.len() == 2) return hamt.data()[if (packed_index == 0) 1 else 0];
                    const new = rt.gc.alloc(.hamt, hamt.len()) catch
                        @panic("GC allocation failure");
                    new.mask = hamt.mask & ~(@as(u64, 1) << @intCast(slot));
                    @memcpy(new.data(), hamt.data()[0..packed_index]);
                    @memcpy(
                        new.data()[packed_index..],
                        hamt.data()[packed_index + 1 .. hamt.len()],
                    );
                    return rt.gc.commit(.hamt, new);
                }
                const new = rt.gc.alloc(.hamt, hamt.len()) catch
                    @panic("GC allocation failure");
                new.mask = hamt.mask;
                @memcpy(new.data(), hamt.data()[0..hamt.len()]);
                new.data()[packed_index] = result.?;
                return rt.gc.commit(.hamt, new);
            },
            else => unreachable,
        }
    }

    // i don't really like needing to allocate for the iteration
    // but there seems to be no easy way around it, assuming we want a stateful iterator like this
    pub const HamtIterator = struct {
        rt: *RT,
        curr: ?*Object,
        list: ?*Object,
        slot: usize,

        pub fn next(it: *HamtIterator) ?*Object {
            if (it.curr == null) return null;

            const hamt = it.curr.?.as(.hamt);
            if (it.slot == hamt.len()) {
                if (it.list == null) {
                    it.curr = null;
                    return it.next();
                }
                const cons = it.list.?.as(.cons);
                it.curr = cons.car;
                it.list = cons.cdr;
                it.slot = 0;
                return it.next();
            }

            const child = hamt.data()[it.slot];
            switch (child.getKind()) {
                .cons => {
                    it.slot += 1;
                    return child;
                },
                .hamt => {
                    it.list = it.rt.newCons(child, it.list);
                    it.slot += 1;
                    return it.next();
                },
                else => unreachable,
            }
        }
    };
    pub fn hamtIter(rt: *RT, objhamt: ?*Object) HamtIterator {
        return HamtIterator{
            .rt = rt,
            .curr = objhamt,
            .list = null,
            .slot = 0,
        };
    }

    pub fn eql(rt: *RT, obj1: ?*Object, obj2: ?*Object) bool {
        if (obj1 == obj2) return true;
        if (obj1 == null or obj2 == null) return false;
        if (obj1.?._property != obj2.?._property) return false;
        // NOTE _property equality implies both kind and hash are equal
        return switch (obj1.?.getKind()) {
            .real => obj1.?.as(.real).data == obj2.?.as(.real).data,
            .cons => blk: {
                const cons1 = obj1.?.as(.cons);
                const cons2 = obj2.?.as(.cons);
                break :blk rt.eql(cons1.car, cons2.car) and rt.eql(cons1.cdr, cons2.cdr);
            },
            .champ => blk: {
                const champ1 = obj1.?.as(.champ);
                const champ2 = obj2.?.as(.champ);
                if (champ1.datamask != champ2.datamask or
                    champ1.nodemask != champ2.nodemask) break :blk false;
                std.debug.assert(champ1.datalen == champ2.datalen);
                std.debug.assert(champ1.nodelen == champ2.nodelen);
                const data1 = champ1.data();
                const data2 = champ2.data();
                for (0..2 * champ1.datalen + champ1.nodelen) |i| {
                    if (!rt.eql(data1[i], data2[i])) break :blk false;
                }
                break :blk true;
            },
            // .hamt => blk: {
            //     var it = rt.hamtIter(obj1);
            //     while (it.next()) |objkv| {
            //         const kv = objkv.as(.cons);
            //         if (!rt.hamtContains(obj2, kv.car)) break :blk false;
            //     }
            //     it = rt.hamtIter(obj2);
            //     while (it.next()) |objkv| {
            //         const kv = objkv.as(.cons);
            //         if (!rt.hamtContains(obj1, kv.car)) break :blk false;
            //     }
            //     break :blk true;
            // const hamt1 = obj1.?.as(.hamt);
            // const hamt2 = obj2.?.as(.hamt);
            // if (hamt1.mask != hamt2.mask) break :blk false;
            // for (0..hamt1.len()) |i| {
            //     if (!eql(hamt1.data()[i], hamt2.data()[i])) break :blk false;
            // }
            // break :blk true;
            // },
            .string => blk: {
                const string1 = obj1.?.as(.string);
                const string2 = obj2.?.as(.string);
                if (string1.len != string2.len) break :blk false;
                break :blk std.mem.eql(
                    u8,
                    string1.data()[0..string1.len],
                    string2.data()[0..string2.len],
                );
            },
        };
    }
};

pub fn print(obj: ?*Object, writer: anytype) anyerror!void {
    try _printImpl(obj, writer);
    try writer.print("\n", .{});
}

pub fn debugPrint(obj: ?*Object) void {
    const stderr = std.io.getStdErr().writer();
    print(obj, stderr) catch {};
}

fn _printImpl(_obj: ?*Object, writer: anytype) anyerror!void {
    const obj = _obj orelse return writer.print("nil", .{});
    switch (obj.getKind()) {
        .real => try writer.print("{d}", .{obj.as(.real).data}),
        .cons => {
            var cons = obj.as(.cons);
            try writer.print("(", .{});
            while (true) {
                try _printImpl(cons.car, writer);
                if (cons.cdr == null) {
                    break;
                } else if (cons.cdr.?.getKind() != .cons) {
                    try writer.print(" . ", .{});
                    try _printImpl(cons.cdr, writer);
                    break;
                }
                try writer.print(" ", .{});
                cons = cons.cdr.?.as(.cons);
            }
            try writer.print(")", .{});
        },
        .champ => {
            try writer.print("{{", .{});
            try _printChamp(obj, true, writer);
            try writer.print("}}", .{});
        },
        .string => {
            const str = obj.as(.string);
            try writer.print("\"{s}\"", .{str.data()[0..str.len]});
        },
    }
}

fn _printChamp(obj: *Object, first: bool, writer: anytype) anyerror!void {
    const champ = obj.as(.champ);
    var not_first = false;
    for (0..champ.datalen) |i| {
        if (!first or i > 0) try writer.print(", ", .{});
        const key = champ.data()[2 * i];
        const val = champ.data()[2 * i + 1];
        try _printImpl(key, writer);
        try writer.print(" ", .{});
        try _printImpl(val, writer);
        not_first = true;
    }
    for (0..champ.nodelen) |i| {
        const child = champ.nodes()[i];
        try _printChamp(child, first and i == 0 and !not_first, writer);
    }
}
