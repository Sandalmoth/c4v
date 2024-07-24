const std = @import("std");
const BlockPool = @import("blockpool.zig").BlockPool(.{});

const GC = @import("gc9.zig").GC;
const Kind = @import("gc9.zig").Kind;
const Object = @import("gc9.zig").Object;

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

    const ChampKeyContext = struct {
        objkey: ?*Object,
        keyhash: u64,
        depth: usize,

        // though it wastes some bits in the hash
        // the improved performance on divide and modulo seems to be worth it
        const LEVELS_PER_HASH = 8;

        fn init(objkey: ?*Object) ChampKeyContext {
            return .{
                .objkey = objkey,
                .keyhash = Object.hash(objkey, 0),
                .depth = 0,
            };
        }

        fn initDepth(objkey: ?*Object, depth: usize) ChampKeyContext {
            return .{
                .objkey = objkey,
                .keyhash = Object.hash(objkey, depth / LEVELS_PER_HASH),
                .depth = depth,
            };
        }

        fn next(old: ChampKeyContext) ChampKeyContext {
            var new = ChampKeyContext{
                .objkey = old.objkey,
                .keyhash = old.keyhash,
                .depth = old.depth + 1,
            };
            if (old.depth / LEVELS_PER_HASH < new.depth / LEVELS_PER_HASH) {
                new.keyhash = Object.hash(new.objkey, new.depth / LEVELS_PER_HASH);
            }
            return new;
        }

        fn slot(ctx: ChampKeyContext) usize {
            return (ctx.keyhash >> @intCast((ctx.depth % LEVELS_PER_HASH) * 6)) & 0b11_1111;
        }
    };

    pub fn champAssoc(rt: *RT, objchamp: ?*Object, objkey: ?*Object, objval: ?*Object) *Object {
        // inserting into nil just creates a new map
        const obj = objchamp orelse rt.newChamp();
        return rt._champAssocImpl(obj, ChampKeyContext.init(objkey), objval);
    }
    fn _champAssocImpl(
        rt: *RT,
        objchamp: *Object,
        keyctx: ChampKeyContext,
        objval: ?*Object,
    ) *Object {
        const champ = objchamp.as(.champ);
        const slot = keyctx.slot();
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
                keyctx.next(),
                objval,
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
            newdata[2 * packed_index] = keyctx.objkey;
            newdata[2 * packed_index + 1] = objval;
            @memcpy(
                newdata[2 * packed_index + 2 ..],
                olddata[2 * packed_index .. 2 * champ.datalen + champ.nodelen],
            );
            return rt.gc.commit(.champ, new);
        }

        const packed_index = @popCount(champ.datamask & (slotmask - 1));
        if (rt.eql(champ.data()[2 * packed_index], keyctx.objkey)) {
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
        const subctx = ChampKeyContext.initDepth(subkey, keyctx.depth + 1);
        const subslot = subctx.slot();
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
        newnodes[packed_node_index] = rt._champAssocImpl(
            rt.gc.commit(.champ, sub),
            keyctx.next(),
            objval,
        );
        @memcpy(newnodes[packed_node_index + 1 ..], oldnodes[packed_node_index..champ.nodelen]);

        return rt.gc.commit(.champ, new);
    }

    pub fn champGet(rt: *RT, objchamp: ?*Object, objkey: ?*Object) ?*Object {
        const obj = objchamp orelse return null;
        return rt._champGetImpl(obj, ChampKeyContext.init(objkey));
    }
    fn _champGetImpl(rt: *RT, objchamp: *Object, keyctx: ChampKeyContext) ?*Object {
        const champ = objchamp.as(.champ);
        const slot = keyctx.slot();
        const slotmask = @as(u64, 1) << @intCast(slot);
        const is_data = champ.datamask & slotmask > 0;
        const is_node = champ.nodemask & slotmask > 0;
        std.debug.assert(!(is_data and is_node));

        if (!(is_node or is_data)) return null;
        if (is_node) {
            const packed_index = @popCount(champ.nodemask & (slotmask - 1));
            return rt._champGetImpl(champ.nodes()[packed_index], keyctx.next());
        }
        const packed_index = @popCount(champ.datamask & (slotmask - 1));
        if (rt.eql(champ.data()[2 * packed_index], keyctx.objkey)) {
            return champ.data()[2 * packed_index + 1];
        }
        return null;
    }

    pub fn champContains(rt: *RT, objchamp: ?*Object, objkey: ?*Object) bool {
        const obj = objchamp orelse return false;
        return rt._champGetImpl(obj, ChampKeyContext.init(objkey)) != null;
    }

    pub fn champDissoc(rt: *RT, objchamp: ?*Object, objkey: ?*Object) ?*Object {
        const obj = objchamp orelse return null;
        return rt._champDissocImpl(obj, ChampKeyContext.init(objkey));
    }
    fn _champDissocImpl(rt: *RT, objchamp: *Object, keyctx: ChampKeyContext) *Object {
        const champ = objchamp.as(.champ);
        const slot = keyctx.slot();
        const slotmask = @as(u64, 1) << @intCast(slot);
        const is_data = champ.datamask & slotmask > 0;
        const is_node = champ.nodemask & slotmask > 0;
        std.debug.assert(!(is_data and is_node));

        if (!(is_data or is_node)) return objchamp;

        if (is_data) {
            const packed_index = @popCount(champ.datamask & (slotmask - 1));
            if (!rt.eql(champ.data()[2 * packed_index], keyctx.objkey)) return objchamp;
            if (champ.datalen + champ.nodelen == 1) return rt.newChamp();
            const new = rt.gc.alloc(.champ, 2 * (champ.datalen - 1) + champ.nodelen) catch
                @panic("GC allocation failure");
            new.datamask = champ.datamask & ~slotmask;
            new.nodemask = champ.nodemask;
            new.datalen = champ.datalen - 1;
            new.nodelen = champ.nodelen;
            const newdata = new.data();
            const olddata = champ.data();
            @memcpy(newdata[0..], olddata[0 .. 2 * packed_index]);
            @memcpy(
                newdata[2 * packed_index ..],
                olddata[2 * packed_index + 2 .. 2 * champ.datalen + champ.nodelen],
            );
            return rt.gc.commit(.champ, new);
        }

        const packed_index = @popCount(champ.nodemask & (slotmask - 1));
        const objresult = rt._champDissocImpl(champ.nodes()[packed_index], keyctx.next());
        if (rt.eql(champ.nodes()[packed_index], objresult)) return objchamp;

        const result = objresult.as(.champ);
        if (result.nodelen == 0 and result.datalen == 1) {
            if (champ.datalen + champ.nodelen == 1) {
                // this node has only one child
                // and that child is just a kv after the deletion
                // so we can just keep that kv and get rid of this node
                return objresult;
            } else {
                // a node child of this node is now just a kv
                // so store that kv directly here instead
                // (node without subnode) with key
                const packed_data_index = @popCount(champ.datamask & (slotmask - 1));
                const packed_node_index = @popCount(champ.nodemask & (slotmask - 1));
                const new = rt.gc.alloc(.champ, 2 * (champ.datalen + 1) + champ.nodelen - 1) catch
                    @panic("GC allocation failure");
                new.datamask = champ.datamask | slotmask;
                new.nodemask = champ.nodemask & ~slotmask;
                new.datalen = champ.datalen + 1;
                new.nodelen = champ.nodelen - 1;
                const newdata = new.data();
                const olddata = champ.data();
                @memcpy(newdata[0..], olddata[0 .. 2 * packed_data_index]);
                newdata[2 * packed_data_index] = result.data()[0];
                newdata[2 * packed_data_index + 1] = result.data()[1];
                @memcpy(
                    newdata[2 * packed_data_index + 2 ..],
                    olddata[2 * packed_data_index .. 2 * champ.datalen],
                );
                const newnodes = new.nodes();
                const oldnodes = champ.nodes();
                @memcpy(newnodes[0..], oldnodes[0..packed_node_index]);
                @memcpy(
                    newnodes[packed_node_index..],
                    oldnodes[packed_node_index + 1 .. champ.nodelen],
                );
                return rt.gc.commit(.champ, new);
            }
        }
        // node updated with result
        // the node child of this node has been altered but is still a node
        // so just replace that node-child with the result
        std.debug.assert(result.datalen + result.nodelen > 0);
        const new = rt.gc.alloc(.champ, 2 * champ.datalen + champ.nodelen) catch
            @panic("GC allocation failure");
        new.datamask = champ.datamask;
        new.nodemask = champ.nodemask;
        new.datalen = champ.datalen;
        new.nodelen = champ.nodelen;
        const newdata = new.data();
        const olddata = champ.data();
        @memcpy(newdata[0..], olddata[0 .. 2 * champ.datalen + champ.nodelen]);
        new.nodes()[packed_index] = objresult;
        return rt.gc.commit(.champ, new);
    }

    pub fn eql(rt: *RT, obj1: ?*Object, obj2: ?*Object) bool {
        if (obj1 == obj2) return true;
        if (obj1 == null or obj2 == null) return false;
        if (obj1.?.kind != obj2.?.kind) return false;
        return switch (obj1.?.kind) {
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
    switch (obj.kind) {
        .real => try writer.print("{d}", .{obj.as(.real).data}),
        .cons => {
            var cons = obj.as(.cons);
            try writer.print("(", .{});
            while (true) {
                try _printImpl(cons.car, writer);
                if (cons.cdr == null) {
                    break;
                } else if (cons.cdr.?.kind != .cons) {
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
