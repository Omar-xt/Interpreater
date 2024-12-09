const std = @import("std");
const AST = @import("AST.zig");

const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const PrintConfig = @import("VM.zig").PrintConfig;
const SymbolFrame = @import("SymbolTable.zig").SymbolFrame;

const Object = struct {
    name: []const u8,
    body: []Instruction,
};

const Struct2 = struct {
    name: []const u8,
    ind: usize,
    // frame: *SymbolFrame,
};

const Dict = struct {
    d: *std.AutoArrayHashMap(Type, Type),
};

pub const Type = union(enum) {
    Int: AST.IntType,
    Float: AST.FloatType,
    Str: []const u8,
    Call: *const fn ([]Type, PrintConfig) anyerror!void,
    /// dynamic library load
    DLL: *const fn ([]const u8) anyerror!std.DynLib,
    DLib: std.DynLib,
    VPointer: *anyopaque,
    FFI_Struct: Struct2,
    FunctionPtr: []Instruction,
    Fnction: []Instruction,
    Undefined,
    NULL,
    VOID,
    None,
    Object: Inst.ObjectPtr,
    Struct: Inst.StructPtr,
    Struct2: Struct2,
    Range: Inst.Range,
    List: *std.ArrayList(Type),
    Dict: *HashM,
    Kw_names: [][]const u8,
};

pub const HashM = std.ArrayHashMap(Type, Type, Context, false);

const Context = struct {
    pub fn hash(_: Context, _: Type) u32 {
        return 0;
    }

    pub fn eql(_: Context, a: Type, b: Type, _: usize) bool {
        const ac = std.meta.activeTag(a);
        const bc = std.meta.activeTag(b);

        if (ac != bc) return false;

        return switch (a) {
            .Int => |num| b.Int == num,
            .Str => |str| std.mem.eql(u8, str, b.Str),
            else => unreachable,
        };
    }
};
