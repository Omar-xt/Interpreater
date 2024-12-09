const std = @import("std");
const Type = @import("Builtin.zig").Type;

pub const SymbolFrame = struct {
    name: []const u8,
    table: TableType,

    const TableType = std.StringArrayHashMap(Type);

    pub fn init(name: []const u8, alloc: std.mem.Allocator) SymbolFrame {
        return SymbolFrame{
            .name = name,
            .table = TableType.init(alloc),
        };
    }

    pub fn deinit(self: *SymbolFrame) void {
        self.table.deinit();
    }

    pub fn put(self: *SymbolFrame, key: []const u8, value: Type) !void {
        try self.table.put(key, value);
    }

    pub fn get(self: *SymbolFrame, key: []const u8) ?Type {
        return self.table.get(key);
    }
};

stack: std.ArrayList(SymbolFrame),
alloc: std.mem.Allocator,
const Self = @This();

pub fn init(alloc: std.mem.Allocator) !Self {
    var stack = std.ArrayList(SymbolFrame).init(alloc);
    const global_frame = SymbolFrame.init("Global", alloc);
    try stack.append(global_frame);

    return Self{
        .stack = stack,
        .alloc = alloc,
    };
}

pub fn deinit(self: *Self) void {
    for (self.stack.items) |*frame| {
        frame.deinit();
    }
    self.stack.deinit();
}

pub fn get_global(self: *Self) *SymbolFrame {
    return &self.stack.items[0];
}

pub fn getLast(self: *Self) *SymbolFrame {
    return &self.stack.items[self.stack.items.len - 1];
}

pub fn get(self: *Self, ind: usize) *SymbolFrame {
    return &self.stack.items[ind];
}

pub fn len(self: *Self) usize {
    return self.stack.items.len - 1;
}

pub fn push(self: *Self, frame: SymbolFrame) !void {
    try self.stack.append(frame);
}

pub fn add(self: *Self, name: []const u8) !void {
    const frame = SymbolFrame.init(name, self.alloc);
    try self.stack.append(frame);
}

pub fn create(self: *Self, name: []const u8) !*SymbolFrame {
    const frame = SymbolFrame.init(name, self.alloc);
    try self.stack.append(frame);
    return self.getLast();
}
