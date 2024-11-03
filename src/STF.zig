const std = @import("std");

pub fn SymbolTable(comptime T: type) type {
    return struct {
        table: std.StringHashMap(Frame(T)),
        const Self = @This();

        pub fn init(alloc: std.mem.Allocator) !Self {
            var table = std.StringHashMap(Frame(T)).init(alloc);
            const frame = Frame(T).init("Global", alloc);
            try table.put("Global", frame);

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

        pub fn get_global(self: *Self) *Frame(T) {
            const entry = self.table.getEntry("Global").?;
            return entry.value_ptr;
        }

        pub fn put(self: *Self, key: []const u8, frame: Frame(T)) !void {
            try self.table.put(key, frame);
        }

        pub fn get(self: *Self, key: []const u8) ?Frame(T) {
            return self.table.get(key);
        }
    };
}

fn Frame(comptime T: type) type {
    return struct {
        name: []const u8,
        symbols: std.StringHashMap(T),

        const Self = @This();

        pub fn init(name: []const u8, alloc: std.mem.Allocator) Self {
            return Self{
                .name = name,
                .symbols = std.StringHashMap(T).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            self.symbols.deinit();
        }

        pub fn put(self: *Self, key: []const u8, val: T) !void {
            try self.symbols.put(key, val);
        }

        pub fn get(self: *Self, key: []const u8) ?T {
            return self.symbols.get(key);
        }
    };
}
