const std = @import("std");
const stdOut = std.io.getStdOut();

var file: std.fs.File = stdOut;
var writer: std.fs.File.Writer = stdOut.writer();
var config: std.io.tty.Config = .escape_codes;
var buf: std.io.FixedBufferStream([]u8) = std.io.fixedBufferStream(&memory);
const Self = @This();

var memory: [1024]u8 = undefined;

inline fn log_ex(log_lavel: @TypeOf(.EnumLiteral), comptime fmt: []const u8, args: anytype) void {
    std.debug.printSourceAtAddress(std.debug.getSelfDebugInfo() catch unreachable, buf.writer(), @returnAddress() - 1, config) catch unreachable;
    defer buf.pos = 0;

    // std.debug.print("{s}\n", .{buf.getWritten()});

    var flag = false;
    var bs: usize = 0;
    var ind: usize = 0;
    while (ind < buf.pos) : (ind += 1) {
        if (memory[ind] == '/') bs = ind + 1;
        if (memory[ind] == ':') {
            if (flag == true) break else flag = true;
        }
    }

    const color = switch (log_lavel) {
        .INFO => .blue,
        .DEBUG => .magenta,
        .WARN => .bright_yellow,
        .ERROR => .red,
        else => .white,
    };

    config.setColor(writer, color) catch unreachable;
    writer.print("[{s}] ", .{@tagName(log_lavel)}) catch unreachable;
    writer.print("[{s}] ", .{memory[bs..ind]}) catch unreachable;
    writer.print(fmt, args) catch unreachable;
    config.setColor(writer, .reset) catch unreachable;
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.printSourceAtAddress(std.debug.getSelfDebugInfo() catch unreachable, buf.writer(), @returnAddress() - 1, config) catch unreachable;

    var flag = false;
    var bs: usize = 0;
    var ind: usize = 0;
    while (ind < buf.pos) : (ind += 1) {
        if (memory[ind] == '/') bs = ind + 1;
        if (memory[ind] == ':') {
            if (flag == true) break else flag = true;
        }
    }

    writer.print("[{s}] ", .{memory[bs..ind]}) catch unreachable;
    writer.print(fmt, args) catch unreachable;
}

pub fn warn(comptime fmt: []const u8, args: anytype) void {
    log_ex(.WARN, fmt, args);
}

pub fn debug(comptime fmt: []const u8, args: anytype) void {
    log_ex(.DEBUG, fmt, args);
}

pub fn err(comptime fmt: []const u8, args: anytype) void {
    log_ex(.ERROR, fmt, args);
}

pub fn info(comptime fmt: []const u8, args: anytype) void {
    log_ex(.INFO, fmt, args);
}

pub fn vlog(log_lavel: @TypeOf(.EnumLiteral), comptime fmt: []const u8, args: anytype) void {
    log_ex(log_lavel, fmt, args);
}
