const std = @import("std");

const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const SymbolTable = @import("SymbolTable.zig");
const Frame = SymbolTable.Frame;

const AST = @import("AST.zig");
const Node = AST.Node;
const OP = AST.OP;

const InsArray = std.ArrayList(Instruction);

pc: usize,
/// store dsiassebled code
dis: InsArray,
program: InsArray,
symbol_table: SymbolTable,
alloc: std.mem.Allocator,
const Self = @This();

pub fn init(alloc: std.mem.Allocator) !Self {
    return Self{
        .pc = 0,
        .dis = InsArray.init(alloc),
        .program = InsArray.init(alloc),
        .symbol_table = try SymbolTable.init(alloc),
        .alloc = alloc,
    };
}

pub fn deinit(self: *Self) void {
    self.dis.deinit();
    self.program.deinit();
    self.symbol_table.deinit();
}

const GenInstConfig = struct {
    in_local: bool = false,
    frame: *Frame = undefined,
};

pub fn gen_inst(self: *Self, node: Node, stack: *InsArray, config: GenInstConfig) !void {
    // std.debug.print("ln: {any}\n", .{node});
    switch (node) {
        .Assign => |ass| {
            try self.gen_inst(ass.value.*, stack, config);

            const name = ass.target.Name;
            const inst = Instruction{ .STORE_NAME = name };
            try stack.append(inst);

            try config.frame.put(name, .{ .Int = -1 });
        },
        .Name => |name| {
            const inst = switch (config.in_local) {
                true => Instruction{ .LOAD_NAME = name },
                false => Instruction{ .LOAD_FAST = name },
            };

            try stack.append(inst);
        },
        .String => |string| {
            const inst = Instruction{ .LOAD_CONST = .{ .String = string } };
            try stack.append(inst);
        },
        .Number => |number| {
            const inst = Instruction{ .LOAD_CONST = .{ .Number = number } };
            try stack.append(inst);
        },
        .Call => |call| {
            try stack.append(.PUSH_NULL);
            try stack.append(.{ .LOAD_NAME = call.name.Name });
            try self.gen_insts(call.args, stack);
            try stack.append(.CALL);
            try stack.append(.POP_TOP);
        },
        .BinOp => |binop| {
            try self.gen_inst(binop.left.*, stack, config);
            try self.gen_inst(binop.right.*, stack, config);
            try gen_inst_op(binop.op, stack);
        },
        .Compare => |comp| {
            try self.gen_inst(comp.left.*, stack, config);
            try self.gen_inst(comp.right.*, stack, config);
            try self.gen_inst_cmp(comp.option, stack);
        },
        .If => |block| {
            try self.gen_inst(block.tests.*, stack, config);
            try stack.append(.POP_JUMP_IF_FALSE);
            const pc = stack.items.len;
            try stack.append(.{ .JUMP = pc });
            try self.gen_insts(block.body, stack);
            const upc = stack.items.len;
            stack.items[pc].JUMP = upc;
        },
        .FunctionDef => |def| {
            const pos = self.dis.items.len;
            var frame = Frame.init(def.name.Name, self.alloc);
            try self.gen_insts_cf(def.body, &self.dis, .{ .in_local = true, .frame = &frame });

            try self.symbol_table.put(def.name.Name, frame);

            try stack.append(.{ .LOAD_CONST = .{ .ObjectPtr = .{
                .pos = pos,
                .name = def.name.Name,
                .body = self.dis.items[pos..],
            } } });

            std.debug.print("<---------- code object {s} ----------->\n", .{def.name.Name});
            try Inst.dump(self.dis.items[pos..]);
            std.debug.print("----------------------------------------\n", .{});

            try stack.append(.MAKE_FUNCTION);
            try stack.append(.{ .STORE_NAME = def.name.Name });
        },
        .Return => |ret| {
            const inst = switch (ret.value.*) {
                .None => Instruction{ .RETURN_CONST = .None },
                .Number => |num| Instruction{ .RETURN_CONST = .{ .Number = num } },
                .Name, .String => |str| Instruction{ .RETURN_CONST = .{ .String = str } },
                inline else => |_| {
                    try self.gen_inst(ret.value.*, stack, config);
                    try stack.append(.RETURN_VALUE);
                    return;
                },
            };
            try stack.append(inst);
        },
        .Newline => {},
        else => unreachable,
    }
}

fn gen_inst_cmp(_: *Self, op: OP, stack: *InsArray) !void {
    const cmp_op = switch (op) {
        .EQ => .EQ,
        else => unreachable,
    };

    try stack.append(.{ .COMPARE_OP = cmp_op });
}

fn gen_inst_op(op: OP, stack: *InsArray) !void {
    try switch (op) {
        .ADD => stack.append(.{ .BINARY_OP = .ADD }),
        .SUB => stack.append(.{ .BINARY_OP = .SUB }),
        .MUL => stack.append(.{ .BINARY_OP = .MUL }),
        .DIV => stack.append(.{ .BINARY_OP = .DIV }),
        else => unreachable,
    };
}

pub fn gen_insts_cf(self: *Self, nodes: []Node, stack: *InsArray, config: GenInstConfig) anyerror!void {
    for (nodes) |node| {
        try self.gen_inst(node, stack, config);
    }
}

pub fn gen_insts(self: *Self, nodes: []Node, stack: *InsArray) anyerror!void {
    const global_frame = self.symbol_table.get_global();

    for (nodes) |node| {
        try self.gen_inst(node, stack, .{ .frame = global_frame });
    }
}

/// Returns dsiassembled instructions
pub fn get_dis(self: *Self) []Instruction {
    return self.dis.items;
}

pub fn get_instructions(self: *Self, nodes: []Node) ![]Instruction {
    try self.gen_insts(nodes, &self.program);
    return self.program.items;
}
