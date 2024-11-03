const std = @import("std");
const Type = @import("Builtin.zig").Type;

mem: std.ArrayList(Type),
const Self = @This();

pub fn init(size: usize, alloc: std.mem.Allocator) !Self {
    var mem = std.ArrayList(Type).init(alloc);
    try mem.ensureTotalCapacity(size);

    return Self{
        .mem = mem,
    };
}

pub fn deinit(self: Self) void {
    self.mem.deinit();
}

pub fn push(self: *Self, typ: Type) !void {
    try self.mem.append(typ);
}

pub fn pop(self: *Self) Type {
    return self.mem.pop();
}

pub fn get_top(self: Self) Type {
    return self.mem.getLast();
}
