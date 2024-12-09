const std = @import("std");
const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const out_file_path = "output/src1.smc";
var mem: [1024]u8 = undefined;
var buf = std.io.fixedBufferStream(&mem);
var writer = buf.writer();

fn from_str(str: []const u8) !void {
    try writer.print("1{x}", .{str.len});
    try writer.print("{s}", .{std.fmt.fmtSliceHexUpper(str)});
}

fn from_const(con: Inst.Const) !void {
    switch (con) {
        .Number => |num| try writer.print("{x:0>2}", .{@as(usize, @intCast(num))}),
        .String => |str| try from_str(str),
        .None => _ = try writer.write("00"),
        else => unreachable,
    }
}

fn get_bytecode(inst: Instruction) ![]u8 {
    try writer.print("{x:0>2}", .{@intFromEnum(inst)});

    try switch (inst) {
        .LOAD_CONST, .RETURN_CONST => |con| from_const(con),
        .STORE_NAME, .LOAD_NAME => |str| from_str(str),
        .CALL => |c| try writer.print("{x:0>2}", .{c}),
        .PUSH_NULL, .POP_TOP => {},
        else => unreachable,
    };

    return buf.getWritten();
}

pub fn dump(programe: []Instruction) !void {
    var outfile = try std.fs.cwd().createFile(out_file_path, .{});
    defer outfile.close();

    for (programe) |inst| {
        buf.pos = 0;
        const bytecode = try get_bytecode(inst);
        std.debug.print("bc: {s}\n", .{bytecode});

        const out: []u8 = undefined;
        const byt = try std.fmt.hexToBytes(out, bytecode);

        try outfile.writeAll(byt);
    }
}
