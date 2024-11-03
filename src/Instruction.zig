const std = @import("std");
const NumberType = @import("AST.zig").NumberType;

pub const BinaryOp = enum {
    ADD,
    SUB,
    MUL,
    DIV,
};

pub const CompareOp = enum {
    EQ,
};

pub const ObjectPtr = struct {
    pos: usize,
    name: []const u8,
    body: []Instruction,
};

const Const = union(enum) {
    Number: NumberType,
    String: []const u8,
    ObjectPtr: ObjectPtr,
    None,
};

pub const Instruction = union(enum) {
    PUSH,

    LOAD_NAME: []const u8,
    STORE_NAME: []const u8,
    LOAD_FAST: []const u8,

    LOAD_CONST: Const,

    BINARY_OP: BinaryOp,
    COMPARE_OP: CompareOp,

    POP_JUMP_IF_FALSE,
    JUMP: usize,

    PUSH_NULL,
    CALL,

    MAKE_FUNCTION,

    POP_TOP,
    RETURN_CONST: Const,
    RETURN_VALUE,
};

pub fn dump(insts: []Instruction) !void {
    const stdOut = std.io.getStdOut().writer();
    for (insts, 0..) |inst, ind| {
        try switch (inst) {
            .LOAD_NAME, .STORE_NAME, .LOAD_FAST => |val| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), val }),
            .LOAD_CONST, .RETURN_CONST => |con| try dump_constants(ind, inst, con, stdOut),
            .BINARY_OP => |op| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), @tagName(op) }),
            .COMPARE_OP => |cmp| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), @tagName(cmp) }),
            .JUMP => |jump| stdOut.print("{d} {s:10} {d}\n", .{ ind, @tagName(inst), jump }),
            inline else => stdOut.print("{d} {s:10}\n", .{ ind, @tagName(inst) }),
        };
    }
}

fn dump_constants(ind: usize, inst: Instruction, con: Const, stdOut: @TypeOf(std.io.getStdOut().writer())) !void {
    try switch (con) {
        .None => stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), "None" }),
        .String => |string| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), string }),
        .Number => |number| stdOut.print("{d} {s:10} {d}\n", .{ ind, @tagName(inst), number }),
        .ObjectPtr => |ptr| stdOut.print("{d} {s:10} {s} : {d}\n", .{ ind, @tagName(inst), ptr.name, ptr.pos }),
    };
}
