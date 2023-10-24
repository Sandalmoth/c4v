const std = @import("std");

// implements a single variant of
// https://github.com/ziglibs/zigfp
// originally written by Felix "xq" Queißner, released under MIT license
// specifically, this implements Q48.16
// however, all operators are saturating
// it's updated for the latest zig version
// and will eventually support other math ops that I need

const scaling = 1 << 16;

pub const Q = struct {
    raw: i64,

    pub const precision = 2.0 / @as(comptime_float, scaling);

    pub fn add(a: Q, b: Q) Q {
        return .{ .raw = a.raw +| b.raw };
    }

    pub fn sub(a: Q, b: Q) Q {
        return .{ .raw = a.raw -| b.raw };
    }

    pub fn mul(a: Q, b: Q) Q {
        return .{ .raw = @as(i64, @truncate((@as(i128, a.raw) * @as(i128, b.raw)) >> 16)) };
    }

    pub fn div(a: Q, b: Q) Q {
        return .{ .raw = @as(i64, @intCast(@divTrunc(@as(i128, a.raw) << 16, @as(i128, b.raw)))) };
    }

    pub fn mod(a: Q, b: Q) Q {
        return .{ .raw = @mod(a.raw, b.raw) };
    }

    pub fn lt(a: Q, b: Q) bool {
        return a.raw < b.raw;
    }

    pub fn gt(a: Q, b: Q) bool {
        return a.raw > b.raw;
    }

    pub fn leq(a: Q, b: Q) bool {
        return a.raw <= b.raw;
    }

    pub fn geq(a: Q, b: Q) bool {
        return a.raw >= b.raw;
    }

    pub fn eq(a: Q, b: Q) bool {
        return a.raw == b.raw;
    }

    pub fn format(x: Q, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime (fmt.len == 0 or std.mem.eql(u8, fmt, "any") or std.mem.eql(u8, fmt, "d"))) {
            var copy_options = if (options.precision == null) blk: {
                var copy = options;
                copy.precision = 5; // 4 < log10(2^32) < 5
                break :blk copy;
            } else options;
            try std.fmt.formatFloatDecimal(floatFromFixed(f64, x), copy_options, writer);
        } else if (comptime std.mem.eql(u8, fmt, "x")) {
            try std.fmt.formatFloatHexadecimal(floatFromFixed(f64, x), options, writer);
        } else if (comptime std.mem.eql(u8, fmt, "e")) {
            try std.fmt.formatFloatScientific(floatFromFixed(f64, x), options, writer);
        } else {
            @compileError(std.fmt.comptimePrint("Invalid fmt for Q (fixed point): {{{s}}}", .{fmt}));
        }
    }
};

pub const add = Q.add;
pub const sub = Q.sub;
pub const mul = Q.mul;
pub const div = Q.div;
pub const mod = Q.mod;
pub const lt = Q.lt;
pub const gt = Q.gt;
pub const leq = Q.leq;
pub const geq = Q.geq;
pub const eq = Q.eq;

pub fn fixedFromFloat(x: anytype) Q {
    return .{ .raw = @as(i64, @intFromFloat(scaling * x)) };
    // NOTE we could use lossyCast
    // since otherwise, it's possible for an inaccurate floating point
    // to round to a value that cannot be represented in the int
    // this way it would always succeed
    // return .{ .raw = std.math.lossyCast(i64, scaling * x) };
}

pub fn floatFromFixed(comptime T: type, x: Q) T {
    return @as(T, @floatFromInt(x.raw)) / scaling;
}

pub fn fixedFromInt(x: anytype) Q {
    return .{ .raw = @as(i64, x) <<| 16 };
}

pub fn intFromFixed(comptime T: type, x: Q) T {
    return @intCast(x.raw >> 16);
}

test "conversion" {
    const xs = [_]f32{
        0.0,
        1.0,
        -1.0,
        std.math.e,
        std.math.pi,
        // std.math.maxInt(i32), // due to rounding these overflow
        // we could fix by using lossyCast, but mybe this is better
        std.math.maxInt(i32) - 64,
        std.math.minInt(i32),
    };

    for (xs) |x| {
        const q = fixedFromFloat(x);
        try std.testing.expectApproxEqAbs(x, floatFromFixed(f32, q), Q.precision);
    }

    const ys = [_]i32{
        0,
        1,
        -1,
        std.math.maxInt(i32),
        std.math.minInt(i32),
    };

    for (ys) |y| {
        const q = fixedFromInt(y);
        try std.testing.expectEqual(y, intFromFixed(i32, q));
    }
}
