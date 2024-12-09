const std = @import("std");
const log = @import("log");

const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const SymbolTable = @import("SymbolTable.zig");
const Frame = SymbolTable.SymbolFrame;

const AST = @import("AST.zig");
const Node = AST.Node;
const OP = AST.OP;

const MyArrayList = @import("MyArrayList.zig").MyArrayList;

const InsArray = std.ArrayList(Instruction);
const DisType = MyArrayList(InsArray);

/// programe counter
pc: usize,
token_counter: usize,
token_map: *std.ArrayList(usize),
/// store dsiassebled code
dis: DisType,
program: InsArray,
symbol_table: SymbolTable,
inst_token_map: std.AutoArrayHashMap(usize, usize),
alloc: std.mem.Allocator,
const Self = @This();

pub fn init(token_map: *std.ArrayList(usize), alloc: std.mem.Allocator) !Self {
    return Self{
        .pc = 0,
        .token_counter = 0,
        .token_map = token_map,
        .dis = DisType.init(alloc),
        .program = InsArray.init(alloc),
        .symbol_table = try SymbolTable.init(alloc),
        .inst_token_map = std.AutoArrayHashMap(usize, usize).init(alloc),
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

fn get_store_inst(name: []const u8, config: GenInstConfig) Instruction {
    return switch (config.in_local) {
        true => Instruction{ .STORE_FAST = name },
        false => Instruction{ .STORE_NAME = name },
    };
}

fn get_load_inst(name: []const u8, config: GenInstConfig) Instruction {
    return switch (config.in_local) {
        false => Instruction{ .LOAD_NAME = name },
        true => Instruction{ .LOAD_FAST = name },
    };
}

pub fn gen_inst(self: *Self, node: Node, stack: *InsArray, config: GenInstConfig) !void {
    // std.debug.print("ln: {any}\n", .{node});

    switch (node) {
        .Assign => |ass| {
            try self.gen_inst(ass.value.*, stack, config);
            self.pc += 1;

            switch (ass.target.*) {
                .Subscript => |sbc| {
                    const name_inst = get_load_inst(sbc.value.Name.str, config);
                    try stack.append(name_inst);

                    try self.gen_inst(sbc.slice.*, stack, config);
                    try stack.append(.STORE_SUBSCR);
                },
                .Name => |name| {
                    const name_inst = get_store_inst(name.str, config);
                    try stack.append(name_inst);

                    try config.frame.put(name.str, .{ .Int = -1 });
                },
                .Attribute => |attr| {
                    const name_inst = get_load_inst(attr.name.Name.str, config);
                    try stack.append(name_inst);

                    try stack.append(.{ .STORE_ATTR = attr.attr.Name.str });
                },
                else => unreachable,
            }
        },
        .Name => |name| {
            const inst = get_load_inst(name.str, config);

            try self.inst_token_map.put(stack.items.len, name.ind);
            self.pc += 1;

            try stack.append(inst);
        },
        .String => |string| {
            const inst = Instruction{ .LOAD_CONST = .{ .String = string } };
            try stack.append(inst);
        },
        .Constant => |con| {
            const inst = switch (con) {
                .Int => |int| Instruction{ .LOAD_CONST = .{ .Int = int } },
                .Float => |float| Instruction{ .LOAD_CONST = .{ .Float = float } },
            };
            try stack.append(inst);
        },

        .List => |list| try stack.append(.{ .BUILD_LIST = list }),
        .Dict => |dict| {
            log.err("dlen: {d}\n", .{dict.len});
            var ind: usize = dict.len - 1;
            while (true) : (ind -= 1) {
                // const pair = switch (dict[ind]) {
                //     .Pair => |pair| pair,
                //     else => unreachable,
                // };
                const pair = dict[ind].Pair;
                try self.gen_inst(pair.right.*, stack, config);

                if (ind == 0) break;
            }

            try stack.append(.{ .BUILD_DICT = dict });
        },
        .Subscript => |sbc| {
            try self.gen_inst(sbc.value.*, stack, config);

            switch (sbc.slice.*) {
                .Range => |ran| {
                    try self.gen_inst(ran.start.*, stack, config);
                    try self.gen_inst(ran.end.*, stack, config);
                    try stack.append(.BINARY_SLICE);
                },
                else => {
                    try self.gen_inst(sbc.slice.*, stack, config);
                    try stack.append(.BINARY_SUBCR);
                },
            }
        },
        .Call => |call| {
            // self.token_counter += 1; //--fx
            // try stack.append(.{ .LOAD_NAME = call.name.Name });

            switch (call.func.*) {
                .Name => |name| {
                    try stack.append(.PUSH_NULL);
                    self.pc += 2;
                    if (std.mem.eql(u8, "print", name.str)) {
                        try stack.append(.{ .LOAD_NAME = name.str });
                    } else {
                        log.info("name: {s} sil: {d}\n", .{ name.str, stack.items.len });
                        try self.inst_token_map.put(stack.items.len, name.ind);
                        try stack.append(get_load_inst(name.str, config));
                    }
                },
                .Attribute => |_| {
                    try stack.append(.PUSH_NULL);
                    // log.info("attr sil: {d}\n", .{stack.items.len});
                    // try self.inst_token_map.put(stack.items.len, attr.attr.Name.ind);
                    try self.gen_inst(call.func.*, stack, config);
                },
                // else => unreachable,
                else => |err| {
                    std.debug.print("Unrechable: {any}\n", .{err});
                    unreachable;
                },
            }

            // try self.inst_token_map.put(self.pc - 1, self.token_map[self.token_counter]);
            // self.token_counter += 1; //--fx

            try self.gen_insts_cf(call.args, stack, config);

            if (call.kwargs.len > 0) { //-- setting insts for keyword arguments
                try self.gen_insts_cf(call.kwargs, stack, config);
                try stack.append(.{ .KW_NAMES = call.kw_names });
            }

            try stack.append(.{ .CALL = .{ .args = call.args.len, .kwargs = call.kwargs.len } });
            try stack.append(.POP_TOP);
        },
        .Keyword => |kw| {
            try self.gen_inst(kw.value.*, stack, config);
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
            try stack.append(.{ .JUMP = pc - 1 });
            try self.gen_insts_cf(block.body, stack, config);
            const upc = stack.items.len;
            stack.items[pc].JUMP = upc - 1;
        },
        .For => |block| {
            switch (block.iter.*) {
                .Name => |name| {
                    try stack.append(get_load_inst(name.str, config));
                },
                .Range => |ran| {
                    try stack.append(.{ .LOAD_RANGE = .{
                        .start = get_const_value(ran.start.*),
                        .end = get_const_value(ran.end.*),
                    } });
                },
                else => unreachable,
            }

            try stack.append(.{ .GET_ITER = .{ .start = 0, .end = 0 } });
            const pc = stack.items.len;
            try stack.append(.{ .FOR_ITER = pc });
            try stack.append(get_store_inst(block.target.Name.str, config));
            try self.gen_insts_cf(block.body, stack, config);
            try stack.append(.{ .JUMP_BACKWARD = pc - 1 });
            stack.items[pc].FOR_ITER = stack.items.len - 1;
            try stack.append(.END_FOR);
            stack.items[pc - 1].GET_ITER = .{ .start = pc, .end = stack.items.len - 1 };
        },
        .FunctionDef => |def| {
            self.token_counter += 1; //--fx

            const pos = self.dis.array.items.len;

            var frame = Frame.init(def.name.Name.str, self.alloc);
            const inst_frame = try self.dis.create();
            try self.gen_insts_cf(def.body, inst_frame, .{ .in_local = true, .frame = &frame });

            try self.symbol_table.push(frame);

            try stack.append(.{ .LOAD_CONST = .{ .ObjectPtr = .{
                .pos = pos,
                .name = def.name.Name.str,
                .args = def.args,
                .kwargs = def.kwargs,
            } } });

            log.debug("args: {d}\n", .{def.args.len});

            std.debug.print("<---------- code object {s} ----------->\n", .{def.name.Name.str});
            try Inst.dump(self.dis.array.items[pos].items);
            std.debug.print("----------------------------------------\n", .{});

            try stack.append(.MAKE_FUNCTION);
            try stack.append(get_store_inst(def.name.Name.str, config));

            try config.frame.put(def.name.Name.str, .{ .Int = @intCast(pos) });
        },
        .StructDef => |def| {
            const pos = self.dis.array.items.len;

            var frame = Frame.init(def.name, self.alloc);
            const inst_frame = try self.dis.create();
            try self.gen_insts_cf(def.body, inst_frame, .{ .in_local = true, .frame = &frame });

            try self.symbol_table.push(frame);

            try stack.append(.{ .LOAD_CONST = .{ .StructPtr = .{
                .name = def.name,
                .pos = pos,
                .attrs = &frame.table,
            } } });

            std.debug.print("<---------- code object {s} ----------->\n", .{def.name});
            try Inst.dump(self.dis.array.items[pos].items);
            std.debug.print("----------------------------------------\n", .{});

            try stack.append(.MAKE_STRUCT);
            try stack.append(get_store_inst(def.name, config));
        },
        .Module => |mod| {
            const pos = self.dis.array.items.len;

            var frame = Frame.init(mod.name.Name.str, self.alloc);
            const inst_frame = try self.dis.create();
            try self.gen_insts_cf(mod.body, inst_frame, .{ .in_local = true, .frame = &frame });

            try self.symbol_table.push(frame);

            try stack.append(.{ .LOAD_CONST = .{ .StructPtr = .{
                .name = mod.name.Name.str,
                .pos = pos,
                .attrs = &frame.table,
            } } });

            std.debug.print("<---------- code object {s} ----------->\n", .{mod.name.Name.str});
            try Inst.dump(self.dis.array.items[pos].items);
            std.debug.print("----------------------------------------\n", .{});

            try stack.append(.MAKE_STRUCT);
            try stack.append(get_store_inst(mod.name.Name.str, config));
        },
        .DynLib => |str| {
            try stack.append(.PUSH_NULL);
            try stack.append(.{ .LOAD_NAME = "@DynLib" });
            try stack.append(.{ .LOAD_CONST = .{ .String = str } });
            try stack.append(.{ .CALL = .{ .args = 1, .kwargs = 0 } });
            try stack.append(.POP_TOP);
        },
        .Attribute => |attr| {
            try self.gen_inst(attr.name.*, stack, config);
            try self.inst_token_map.put(stack.items.len, attr.attr.Name.ind);
            try stack.append(.{ .LOAD_ATTER = attr.attr.Name.str });
        },
        .Return => |ret| {
            const inst = switch (ret.value.*) {
                .None => Instruction{ .RETURN_CONST = .None },
                .Constant => |con| switch (con) {
                    .Int => |int| Instruction{ .RETURN_CONST = .{ .Int = int } },
                    .Float => |float| Instruction{ .RETURN_CONST = .{ .Float = float } },
                },
                .Name => |name| Instruction{ .RETURN_CONST = .{ .String = name.str } },
                .String => |str| Instruction{ .RETURN_CONST = .{ .String = str } },
                inline else => |_| {
                    try self.gen_inst(ret.value.*, stack, config);
                    try stack.append(.RETURN_VALUE);
                    return;
                },
            };
            try stack.append(inst);
        },
        .Newline, .Comment => {},
        else => |c| std.debug.print("Unrechable: {any}\n", .{c}),
        // else => unreachable,
    }
}

fn get_const_value(node: Node) Inst.Const {
    return switch (node) {
        .Name => |name| Inst.Const{ .String = name.str },
        .Constant => |con| switch (con) {
            .Int => |int| Inst.Const{ .Int = int },
            .Float => |float| Inst.Const{ .Float = float },
        },
        else => @panic("Cant get const value"),
    };
}

fn gen_inst_cmp(_: *Self, op: OP, stack: *InsArray) !void {
    try stack.append(.{ .COMPARE_OP = op });
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
pub fn get_dis(self: *Self) []InsArray {
    return self.dis.array.items;
}

pub fn get_instructions(self: *Self, nodes: []Node) ![]Instruction {
    try self.gen_insts(nodes, &self.program);
    return self.program.items;
}

pub fn get_inst_token_map(self: Self) std.AutoArrayHashMap(usize, usize) {
    return self.inst_token_map;
}
