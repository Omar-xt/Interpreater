const std = @import("std");
const CodeGen = @import("CodeGen.zig");
const Tokenizer = @import("Tokenizer.zig");

const AST = @import("AST.zig");
const Node = AST.Node;

const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const Type = @import("Builtin.zig").Type;
const Stack = @import("Stack.zig");
const stf = @import("STF.zig");

const default_stack_size = 10;

stack: Stack,
symbol_table: stf.SymbolTable(Type),
alloc: std.mem.Allocator,
const Self = @This();

pub fn init(alloc: std.mem.Allocator) !Self {
    var symbol_table = try stf.SymbolTable(Type).init(alloc);
    const global_frame = symbol_table.get_global();
    try global_frame.put("print", .{ .Call = fprint });
    // try symbol_table.put("print", .{ .Call = fprint });

    return Self{
        .stack = try Stack.init(default_stack_size, alloc),
        .symbol_table = symbol_table,
        .alloc = alloc,
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit();
    self.symbol_table.deinit();
}

const RunError = union(enum) {
    NameError: []const u8,
};

const Error = error{runError};

fn RaiseError(err: RunError) !void {
    const stdOut = std.io.getStdOut().writer();
    try switch (err) {
        .NameError => |name| stdOut.print("NameError: {s} is not definied.!\n", .{name}),
    };
    return error.runError;
}

pub fn run(self: *Self, program: []Instruction) anyerror!void {
    self.run_insts(program) catch |err| return switch (err) {
        error.runError => {},
        else => err,
    };
}

fn run_insts(self: *Self, program: []Instruction) !void {
    var ind: usize = 0;
    while (ind < program.len) : (ind += 1) {
        const pg = program[ind];
        // std.debug.print("pg: {any}\n", .{pg});
        switch (pg) {
            .LOAD_CONST, .RETURN_CONST => |con| {
                const val = switch (con) {
                    .Number => |num| Type{ .Int = num },
                    .String => |string| Type{ .Str = string },
                    .ObjectPtr => |ptr| Type{ .Object = .{ .name = ptr.name, .body = ptr.body } },
                    .None => return, //-- fix to return from a scope frame.
                };

                try self.stack.push(val);
            },

            .STORE_NAME => |name| {
                try self.symbol_table.get_global().put(name, self.stack.pop());
            },

            .LOAD_NAME => |name| {
                const obj = self.symbol_table.get_global().get(name);
                if (obj) |val| {
                    try self.stack.push(val);
                } else try RaiseError(.{ .NameError = name });
            },

            .BINARY_OP => |bin_op| {
                const rval = self.stack.pop();
                const lval = self.stack.pop();

                const res = switch (bin_op) {
                    .ADD => BinaryAdd(lval, rval),
                    .SUB => BinarySub(lval, rval),
                    .MUL => BinaryMul(lval, rval),
                    .DIV => BinaryDiv(lval, rval),
                };
                try self.stack.push(res);
            },

            .COMPARE_OP => |cmp| {
                const rval = self.stack.pop();
                const lval = self.stack.pop();

                const res = switch (cmp) {
                    .EQ => BinaryCmpEq(lval, rval),
                };
                try self.stack.push(res);
            },

            .POP_JUMP_IF_FALSE => {
                if (self.stack.pop().Int == 1) ind += 1;
            },

            .JUMP => |jump| ind = jump - 1,

            .CALL => {
                var args = std.ArrayList(Type).init(self.alloc);
                defer args.deinit();

                var it = self.stack.pop();
                while (it != .NULL) {
                    try args.append(it);
                    it = self.stack.pop();
                }

                const func = args.pop();
                std.mem.reverse(Type, args.items);

                try switch (func) {
                    .Call => |call| call(args.items),
                    .Object => |obj| try self.run(obj.body),
                    else => unreachable,
                };
            },

            .MAKE_FUNCTION => { //-- fix the whole system
                const obj_ptr = self.stack.pop();
                try self.stack.push(obj_ptr);
            },

            // .POP_TOP => _ = self.stack.pop(),
            .PUSH_NULL => try self.stack.push(.NULL),

            else => {},
        }
    }
}

fn BinaryAdd(lval: Type, rval: Type) Type {
    return Type{ .Int = lval.Int + rval.Int };
}

fn BinarySub(lval: Type, rval: Type) Type {
    return Type{ .Int = lval.Int - rval.Int };
}

fn BinaryMul(lval: Type, rval: Type) Type {
    return Type{ .Int = lval.Int * rval.Int };
}

fn BinaryDiv(lval: Type, rval: Type) Type {
    return Type{ .Int = @divFloor(lval.Int, rval.Int) };
}

fn BinaryCmpEq(lval: Type, rval: Type) Type {
    const cmp = std.meta.eql(lval, rval);

    // std.debug.print("Cmp: {any}\n", .{@typeInfo(lval)});

    return Type{ .Int = @intFromBool(cmp) };
}

fn fprint(args: []Type) !void {
    // std.debug.print("args: {any}\n", .{args});
    const stdOut = std.io.getStdOut().writer();
    for (args) |arg| {
        switch (arg) {
            .Int => |int| try stdOut.print("{d}", .{int}),
            .Str => |str| try stdOut.print("{s}", .{str}),
            else => unreachable,
        }

        try stdOut.writeByte(' ');
    }

    try stdOut.writeByte('\n');
}

pub fn dump_symbol(self: Self) !void {
    var it = self.symbol_table.iterator();

    while (it.next()) |entry| {
        std.debug.print("{s} : {any}\n", .{ entry.key_ptr.*, entry.value_ptr });
    }
}
