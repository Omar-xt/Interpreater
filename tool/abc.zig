const std = @import("std");

export fn print(str: [*c]const u8) void {
    std.debug.print("from_ffi {s}\n", .{str});
}

export fn sum(a: i32, b: i32) i32 {
    return a + b;
}

export fn fsum(a: f32, b: f32) f32 {
    return a + b;
}
