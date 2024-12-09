const std = @import("std");

/// only should be used for arraylike types
pub fn MyArrayList(comptime T: type) type {
    return struct {
        array: std.ArrayList(T),
        alloc: std.mem.Allocator,

        const Self = @This();

        pub fn init(alloc: std.mem.Allocator) Self {
            return Self{
                .array = std.ArrayList(T).init(alloc),
                .alloc = alloc,
            };
        }

        pub fn deinit(self: Self) void {
            for (self.array.items) |*item| {
                item.deinit();
            }
            self.array.deinit();
        }

        pub fn getLast(self: Self) *T {
            return &self.array.items[self.array.items.len - 1];
        }

        pub fn append(self: *Self, item: T) !void {
            try self.array.append(item);
        }

        pub fn create(self: *Self) !*T {
            try self.array.append(T.init(self.alloc));
            return self.getLast();
        }

        pub fn pop(self: *Self) T {
            return self.array.pop();
        }

        pub fn remove_last(self: *Self) void {
            _ = self.array.pop();
        }
    };
}
