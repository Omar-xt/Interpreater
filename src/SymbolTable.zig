const std = @import("std");

table: std.StringHashMap(Frame),
const Self = @This();

const Type = union(enum) {
    Int: i32,
};

pub fn init(alloc: std.mem.Allocator) !Self {
    var table = std.StringHashMap(Frame).init(alloc);
    try table.put("Global", Frame.init("Global", alloc));

    return Self{
        .table = table,
    };
}

pub fn deinit(self: *Self) void {
    var it = self.table.valueIterator();
    while (it.next()) |frame| {
        frame.deinit();
    }
    self.table.deinit();
}

pub fn get_global(self: *Self) *Frame {
    const entry = self.table.getEntry("Global").?;
    return entry.value_ptr;
}

pub fn put(self: *Self, key: []const u8, frame: Frame) !void {
    try self.table.put(key, frame);
}

pub fn get(self: *Self, key: []const u8) ?Frame {
    return self.table.get(key);
}

pub const Frame = struct {
    name: []const u8,
    symbols: std.StringHashMap(Type),

    pub fn init(name: []const u8, alloc: std.mem.Allocator) Frame {
        return Frame{
            .name = name,
            .symbols = std.StringHashMap(Type).init(alloc),
        };
    }

    pub fn deinit(self: *Frame) void {
        self.symbols.deinit();
    }

    pub fn put(self: *Frame, key: []const u8, val: Type) !void {
        try self.symbols.put(key, val);
    }

    pub fn get(self: *Frame, key: []const u8) ?Type {
        return self.symbols.get(key);
    }
};
