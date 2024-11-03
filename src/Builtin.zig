const AST = @import("AST.zig");

const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const Object = struct {
    name: []const u8,
    body: []Instruction,
};

pub const Type = union(enum) {
    Int: AST.NumberType,
    Str: []const u8,
    Call: *const fn ([]Type) anyerror!void,
    Fnction: []Instruction,
    Undefined,
    NULL,
    Object: Object,
};
