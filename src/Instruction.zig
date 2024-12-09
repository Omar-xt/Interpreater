const std = @import("std");
const AST = @import("AST.zig");
const IntType = AST.IntType;
const FloatType = AST.FloatType;
const Node = AST.Node;
const Type = @import("Builtin.zig").Type;

pub const BinaryOp = enum {
    ADD,
    SUB,
    MUL,
    DIV,
};

pub const CompareOp = enum {
    EQ,
    LT,
    GT,
};

pub const ObjectPtr = struct {
    pos: usize,
    name: []const u8,
    args: []Node,
    kwargs: []Node,
};

pub const StructPtr = struct {
    name: []const u8,
    pos: usize,
    attrs: *std.StringArrayHashMap(Type),
};

pub const Const = union(enum) {
    Int: IntType,
    Float: FloatType,
    String: []const u8,
    ObjectPtr: ObjectPtr,
    StructPtr: StructPtr,
    None,
};

pub const Range = struct {
    start: Const,
    end: Const,
};

const Call = struct {
    args: usize,
    kwargs: usize,
};

pub const InstSlice = struct {
    start: usize,
    end: usize,
};

pub const Instruction = union(enum(u8)) {
    VOID = 2,

    STORE_NAME: []const u8,
    STORE_FAST: []const u8,

    STORE_ATTR: []const u8,

    LOAD_NAME: []const u8,
    LOAD_FAST: []const u8,
    LOAD_GLOBAL: []const u8,

    LOAD_ATTER: []const u8,

    LOAD_CONST: Const,
    LOAD_RANGE: Range,

    GET_ITER: InstSlice,
    FOR_ITER: usize,
    END_FOR,
    JUMP_BACKWARD: usize,

    BUILD_LIST: []Node,
    BUILD_DICT: []Node,

    BINARY_SUBCR,
    BINARY_SLICE,
    STORE_SUBSCR,

    BINARY_OP: BinaryOp,
    COMPARE_OP: AST.OP,

    POP_JUMP_IF_FALSE,
    JUMP: usize,

    PUSH_NULL,
    CALL: Call,

    KW_NAMES: [][]const u8,

    MAKE_STRUCT,
    MAKE_FUNCTION,

    POP_TOP,
    RETURN_CONST: Const,
    RETURN_VALUE,
};

pub fn dump(insts: []Instruction) !void {
    const stdOut = std.io.getStdOut().writer();
    for (insts, 0..) |inst, ind| {
        try switch (inst) {
            .STORE_NAME, .STORE_FAST, .STORE_ATTR, .LOAD_NAME, .LOAD_FAST, .LOAD_GLOBAL, .LOAD_ATTER => |val| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), val }),
            .LOAD_CONST, .RETURN_CONST => |con| try dump_constants(ind, inst, con, stdOut),
            .BINARY_OP => |op| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), @tagName(op) }),
            .COMPARE_OP => |cmp| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), @tagName(cmp) }),
            .JUMP, .JUMP_BACKWARD, .FOR_ITER => |jump| stdOut.print("{d} {s:10} {d}\n", .{ ind, @tagName(inst), jump }),
            .KW_NAMES => |names| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), names }),
            // .LOAD_ATTER, .STORE_ATTR => |attr| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), attr }),
            .VOID => stdOut.writeByte('\n'),
            inline else => stdOut.print("{d} {s:10}\n", .{ ind, @tagName(inst) }),
        };
    }
}

fn dump_constants(ind: usize, inst: Instruction, con: Const, stdOut: @TypeOf(std.io.getStdOut().writer())) !void {
    try switch (con) {
        .None => stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), "None" }),
        .String => |string| stdOut.print("{d} {s:10} {s}\n", .{ ind, @tagName(inst), string }),
        .Int => |int| stdOut.print("{d} {s:10} {d}\n", .{ ind, @tagName(inst), int }),
        .Float => |float| stdOut.print("{d} {s:10} {d}\n", .{ ind, @tagName(inst), float }),
        .ObjectPtr => |ptr| stdOut.print("{d} {s:10} {s} : {d}\n", .{ ind, @tagName(inst), ptr.name, ptr.pos }),
        .StructPtr => |ptr| stdOut.print("{d} {s:10} struct : {d}\n", .{ ind, @tagName(inst), ptr.pos }),
    };
}
