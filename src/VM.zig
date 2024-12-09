const std = @import("std");
const log = @import("log");
const CodeGen = @import("CodeGen.zig");
const Tokenizer = @import("Tokenizer.zig");

const AST = @import("AST.zig");
const Node = AST.Node;

const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const Builtin = @import("Builtin.zig");
const Type = Builtin.Type;

const SymbolTable = @import("SymbolTable.zig");
const Error = @import("Error.zig");

const MyArrayList = @import("MyArrayList.zig").MyArrayList;

const c = @cImport(@cInclude("ffi.h"));

const stdOut = std.io.getStdOut().writer();

const default_stack_size = 10;

const StackTracker = struct {
    name: []const u8,
    pos: usize,
};

const Iterable = union(enum) {
    range: Range,
    list: ListIter,

    pub fn next(self: *Iterable) ?Type {
        return switch (self.*) {
            inline else => |*it| it.next(),
        };
    }
};

const ListIter = struct {
    start: usize,
    end: usize,
    slice: []Type,

    pub fn init(slice: []Type) ListIter {
        return ListIter{ .start = 0, .end = slice.len, .slice = slice };
    }

    pub fn next(self: *ListIter) ?Type {
        if (self.start >= self.end) return null;
        const out = self.slice[self.start];
        self.start += 1;
        return out;
    }
};

const Range = struct {
    start: usize,
    end: usize,

    pub fn init(start: usize, end: usize) Range {
        return Range{ .start = start, .end = end };
    }

    pub fn next(self: *Range) ?Type {
        if (self.start >= self.end) return null;
        const out = self.start;
        self.start += 1;
        return Type{ .Int = @intCast(out) };
    }
};

const HashMapType = Builtin.HashM;
const ListTable = MyArrayList(MyArrayList(std.ArrayList(Type)));
const DictTable = MyArrayList(HashMapType);

var tokens: []Tokenizer.Token = undefined;
var iterables: std.ArrayList(Iterable) = undefined;
pub var lists: ListTable = undefined;
pub var dicts: DictTable = undefined;
var dynamic_libs: std.ArrayList(std.DynLib) = undefined;

const InsArray = std.ArrayList(Instruction);
var disassabled_code: []InsArray = undefined;

var tracer: std.ArrayList(Type) = undefined;

stack: std.ArrayList(Type),
stack_tracker: std.ArrayList(StackTracker),
symbol_table: SymbolTable,
inst_token_map: std.AutoArrayHashMap(usize, usize),
/// Error raised by virtual matchine
verror: Error,
alloc: std.mem.Allocator,
const Self = @This();

fn DynLib(path: []const u8) !std.DynLib {
    const lib = try std.DynLib.open(path);
    try dynamic_libs.append(lib);
    return lib;
}

pub fn init(inst_token_map: std.AutoArrayHashMap(usize, usize), ts: []Tokenizer.Token, dis: []InsArray, alloc: std.mem.Allocator) !Self {
    var symbol_table = try SymbolTable.init(alloc);
    const global_frame = symbol_table.get_global();
    try global_frame.put("print", .{ .Call = fprint });
    try global_frame.put("@DynLib", .{ .DLL = DynLib });

    var stack_tracker = std.ArrayList(StackTracker).init(alloc);
    try stack_tracker.append(.{ .name = "Global", .pos = 0 });

    var stack = std.ArrayList(Type).init(alloc);
    try stack.ensureTotalCapacity(default_stack_size);

    disassabled_code = dis;

    tokens = ts;

    iterables = std.ArrayList(Iterable).init(alloc);

    lists = ListTable.init(alloc);
    try lists.append(MyArrayList(std.ArrayList(Type)).init(alloc));

    dicts = DictTable.init(alloc);
    try dicts.append(HashMapType.init(alloc));

    tracer = std.ArrayList(Type).init(alloc);

    dynamic_libs = std.ArrayList(std.DynLib).init(alloc);

    return Self{
        .symbol_table = symbol_table,
        .stack = stack,
        .stack_tracker = stack_tracker,
        .inst_token_map = inst_token_map,
        .verror = Error.init(tokens, inst_token_map),
        .alloc = alloc,
    };
}

pub fn deinit(self: *Self) void {
    lists.deinit();
    dicts.deinit();
    tracer.deinit();

    for (dynamic_libs.items) |*dl| {
        dl.close();
    }
    dynamic_libs.deinit();

    self.stack.deinit();
    self.symbol_table.deinit();
    self.stack_tracker.deinit();
    iterables.deinit();
}

const RunConfig = struct {
    loop: bool = false,
    start_ind: ?usize = 0,
};
pub fn run(self: *Self, program: []Instruction, config: RunConfig) anyerror!void {
    self.run_insts(program, config) catch |err| return switch (err) {
        error.runError => try self.verror.flush(), //-- dump the errors
        else => err,
    };
}

fn run_insts(self: *Self, program: []Instruction, config: RunConfig) !void {
    // std.debug.print("pg len: {d}\n", .{program.len});
    // std.debug.print("pgs {any}\n", .{program});

    var ind: usize = 0;
    var program_len: usize = program.len;

    if (config.start_ind) |sind| {
        ind = sind;
        program_len += sind;
    }

    while (ind < program_len) : (ind += 1) {
        // std.debug.print("ind: {d}\n", .{ind});
        const pg = program[ind];
        // std.debug.print("pg: {any}\n", .{pg});
        switch (pg) {
            .LOAD_CONST, .RETURN_CONST => |con| {
                const val = switch (con) {
                    .Int => |int| Type{ .Int = int },
                    .Float => |float| Type{ .Float = float },
                    .String => |string| Type{ .Str = string },
                    .ObjectPtr => |ptr| Type{ .Object = ptr },
                    .StructPtr => |ptr| Type{ .Struct = ptr },
                    .None => Type{ .None = {} },
                };

                if (pg == .RETURN_CONST) { //-- ooganize it later //-- going out of scope and clearing unnecessary data

                    if (config.loop == true) break;

                    var frame = self.symbol_table.stack.pop();
                    frame.deinit();

                    var list = lists.pop(); //-- remving the list scope
                    list.deinit();

                    // if (self.stack_tracker.items.len > 0) {
                    //     const track = self.stack_tracker.pop();
                    //     const len = self.stack.items.len - track.pos;

                    //     for (0..len) |_| _ = self.stack.pop(); //-- removing all values from the stack for this scope
                    // }

                    try self.stack.append(val);

                    break;
                } else try self.stack.append(val);
            },

            .LOAD_GLOBAL => |name| {
                const val = self.symbol_table.get_global().get(name).?;
                try self.stack.append(val);
            },

            .LOAD_FAST => |name| {
                const val = self.symbol_table.getLast().get(name);

                //-- checking error
                try switch (val != null) {
                    true => self.stack.append(val.?),
                    false => self.verror.raise(.{ .NameError = {} }, ind),
                };
            },

            .LOAD_NAME => |name| {
                const obj = self.symbol_table.get_global().get(name);

                //-- checking error
                try switch (obj == null) {
                    true => {
                        const index = if (ind == 0) 0 else ind;
                        try self.verror.raise(.NameError, index);
                    },
                    false => self.stack.append(obj.?),
                };
            },

            .LOAD_ATTER => |name| {
                const last = self.stack.pop();
                // std.debug.print("st: {any}\n", .{st});
                switch (last) {
                    .Struct2, .FFI_Struct => |st| {
                        const index = st.ind;
                        var frame = self.symbol_table.get(index);
                        const attr = frame.get(name);

                        switch (attr == null) {
                            true => {
                                log.info("hmm ind: {d}\n", .{ind});
                                try self.verror.raise(.{ .AttrError = st.name }, ind);
                            },
                            false => {
                                try tracer.append(last);
                                try self.stack.append(attr.?);
                            },
                        }
                    },

                    .DLib => |dl| {
                        var buf: [128]u8 = undefined;
                        const nameZ = try std.fmt.bufPrintZ(&buf, "{s}", .{name});
                        const attr = std.c.dlsym(dl.inner.handle, nameZ) orelse unreachable;
                        const st = try self.symbol_table.create(name);
                        try st.put("func", .{ .VPointer = attr });
                        try st.put("ret_type", .{ .Int = -1 });
                        try self.stack.append(.{ .FFI_Struct = .{ .name = name, .ind = self.symbol_table.len() } });
                    },
                    else => unreachable,
                }
            },

            .LOAD_RANGE => |ran| {
                try self.stack.append(.{ .Range = ran });
            },

            .GET_ITER => |inst_slice| {
                switch (self.stack.pop()) {
                    .Range => |ran| {
                        const start = self.get_int_from_const(ran.start);
                        const end = self.get_int_from_const(ran.end);

                        const new_seq = Range.init(start, end);
                        try iterables.append(.{ .range = new_seq });
                    },

                    .List => |list| try iterables.append(.{ .list = ListIter.init(list.items) }),

                    else => unreachable,
                }

                try self.run(program, .{ .loop = true, .start_ind = ind + 1 });
                ind = inst_slice.end;
            },

            .FOR_ITER => |end_ind| {
                var last_seq = &iterables.items[iterables.items.len - 1];
                const next = last_seq.next();

                switch (next != null) {
                    true => try self.stack.append(next.?),
                    false => ind = end_ind,
                }
            },

            .END_FOR => {
                _ = iterables.pop();
                break;
            },

            .STORE_NAME => |name| {
                try self.symbol_table.get_global().put(name, self.stack.pop());
            },

            .STORE_FAST => |name| {
                try self.symbol_table.getLast().put(name, self.stack.pop());
            },

            .STORE_ATTR => |name| {
                const index = switch (self.stack.pop()) {
                    .Struct2, .FFI_Struct => |st| st.ind,
                    else => unreachable,
                };

                const value = self.stack.pop();

                var frame = self.symbol_table.get(index);
                try frame.put(name, value);
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
                try self.stack.append(res);
            },

            .COMPARE_OP => |cmp| {
                const rval = self.stack.pop();
                const lval = self.stack.pop();

                const res = switch (cmp) {
                    .EQ => BinaryCmpEq(lval, rval),
                    .GT => BinaryGt(lval, rval),
                    .LT => BinaryLt(lval, rval),
                    .LTE => BinaryLte(lval, rval),
                    .GTE => BinaryGte(lval, rval),
                    else => unreachable,
                };
                try self.stack.append(res);
            },

            .POP_JUMP_IF_FALSE => {
                if (self.stack.pop().Int == 1) ind += 1;
            },

            .JUMP, .JUMP_BACKWARD => |jump| ind = jump,

            .KW_NAMES => |names| {
                try self.stack.append(.{ .Kw_names = names });
            },

            .CALL => |call_args| {
                var args = std.ArrayList(Type).init(self.alloc);
                defer args.deinit();

                // var kwargs = HashMapType.init(self.alloc);
                // defer kwargs.deinit();

                var print_config = PrintConfig{};

                switch (self.stack.getLast()) { //-- checking for keyword arguments
                    .Kw_names => |names| {
                        _ = self.stack.pop(); //-- removing the Kw_names from the stack

                        try self.symbol_table.add("anonymous");
                        var frame = self.symbol_table.stack.pop();
                        var index = names.len - 1;
                        while (true) : (index -|= 1) {
                            try frame.put(names[index], self.stack.pop());
                            if (index == 0) break;
                        }

                        print_config.kwargs = &frame; //-- setting the keyword arguments to print_config for print func
                    },
                    else => {},
                }

                // var it = self.stack.pop();
                // while (it != .NULL) {
                //     try args.append(it);
                //     it = self.stack.pop();
                // }

                const arg_count = call_args.args;
                for (0..arg_count + 1) |_| {
                    try args.append(self.stack.pop());
                }

                if (self.stack.items.len > 0) {
                    if (self.stack.getLast() == .NULL) {
                        _ = self.stack.pop(); //-- removing null value
                    }
                }

                var obj_self: ?Type = null;
                var func: Type = undefined;

                const last = args.pop();
                switch (last) {
                    .Call, .Struct2, .DLL, .FFI_Struct => func = last,
                    .Object => {
                        func = last;

                        if (tracer.items.len > 0)
                            obj_self = tracer.pop();
                    },
                    // .DLL => func = last,
                    else => |err| {
                        log.err("Unrechable: {s}\n", .{@tagName(err)});
                        unreachable;
                    },
                }

                //-- freeing the tracker as it only tracke for call in attribute access
                defer tracer.clearRetainingCapacity();

                std.mem.reverse(Type, args.items);

                try switch (func) {
                    .Call => |call| call(args.items, print_config),
                    .DLL => |dll| {
                        std.debug.assert(arg_count == 1);
                        const lib = try dll(args.getLast().Str);
                        try self.stack.append(.{ .DLib = lib });
                    },
                    .FFI_Struct => |fs| {
                        var cif: c.ffi_cif = undefined;
                        var ffi_args = std.ArrayList([*c]c.ffi_type).init(self.alloc);
                        defer ffi_args.deinit();

                        var values = std.ArrayList(*anyopaque).init(self.alloc);
                        defer values.deinit();

                        const st_frame = self.symbol_table.get(fs.ind);
                        const func_ptr = st_frame.get("func") orelse unreachable;

                        var buf: [1024]u8 = undefined;
                        for (args.items) |*arg| {
                            switch (arg.*) {
                                .Int => |*int| {
                                    try ffi_args.append(&c.ffi_type_sint32);
                                    try values.append(@ptrCast(int));
                                },
                                .Str => |str| {
                                    var strZ = try std.fmt.bufPrintZ(&buf, "{s}", .{str});
                                    try ffi_args.append(&c.ffi_type_pointer);
                                    try values.append(@ptrCast(&strZ));
                                },
                                .Float => |*float| {
                                    try ffi_args.append(&c.ffi_type_float);
                                    try values.append(@ptrCast(float));
                                },
                                else => unreachable,
                            }
                        }

                        const rt = st_frame.get("ret_type") orelse unreachable;
                        const r_type = switch (rt) {
                            .Int => &c.ffi_type_sint32,
                            .Float => &c.ffi_type_float,
                            .Str => &c.ffi_type_pointer,
                            else => unreachable,
                        };

                        switch (rt) {
                            .Int, .Float => {
                                var ptr: i32 = 0;
                                const nargs: c_uint = @intCast(args.items.len);
                                _ = c.ffi_prep_cif(@ptrCast(&cif), c.FFI_DEFAULT_ABI, nargs, r_type, @ptrCast(ffi_args.items));
                                c.ffi_call(@ptrCast(&cif), @ptrCast(func_ptr.VPointer), @ptrCast(&ptr), @ptrCast(values.items));

                                const out: Type = switch (rt) {
                                    .Int => .{ .Int = ptr },
                                    .Float => .{ .Float = @as(*f32, @ptrCast(&ptr)).* },
                                    else => unreachable,
                                };
                                try self.stack.append(out);
                            },
                            .Str => {
                                var ptr: *u8 = undefined;
                                const nargs: c_uint = @intCast(args.items.len);
                                _ = c.ffi_prep_cif(@ptrCast(&cif), c.FFI_DEFAULT_ABI, nargs, r_type, @ptrCast(ffi_args.items));
                                c.ffi_call(@ptrCast(&cif), @ptrCast(func_ptr.VPointer), @ptrCast(&ptr), @ptrCast(values.items));

                                var i: usize = 0;
                                while (true) : (i += 1) {
                                    const p = @intFromPtr(ptr) + i;
                                    const cc: *u8 = @ptrFromInt(p);
                                    if (cc.* == '\x00') break;
                                }

                                const str: [*c]const u8 = @ptrCast(ptr);
                                const out: Type = switch (rt) {
                                    .Str => .{ .Str = str[0..i] },
                                    else => unreachable,
                                };
                                try self.stack.append(out);
                            },
                            else => unreachable,
                        }
                    },
                    .Struct2 => |st| {
                        const st_frame = self.symbol_table.get(st.ind);
                        const frame = try st_frame.table.clone();
                        try self.symbol_table.push(.{ .name = st.name, .table = frame });
                        try self.stack.append(.{ .Struct2 = .{ .name = st.name, .ind = self.symbol_table.len() } });
                    },
                    .Object => |obj| {
                        //-- error checking
                        const given_arg_len = if (obj_self != null) args.items.len + 1 else args.items.len;

                        // const given_arg_len = args.items.len;
                        // const obj_arg_len = if (obj_self != null) obj.args.len - 1 else obj.args.len;
                        const obj_arg_len = obj.args.len;
                        if (given_arg_len > (obj_arg_len + obj.kwargs.len) or given_arg_len < obj_arg_len) {
                            try self.verror.raise(.{ .TypeError = .{ .func_args = .{
                                .name = obj.name,
                                .takes = obj_arg_len,
                                .given = given_arg_len,
                            } } }, ind - (args.items.len + 1));
                        }

                        //-- creating scope
                        const name = obj.name;
                        try self.symbol_table.add(name);

                        //-- making a scope for lists also
                        try lists.append(MyArrayList(std.ArrayList(Type)).init(self.alloc));

                        //-- setting the args to the new scope
                        const frame = self.symbol_table.getLast();
                        //--------------------------------------------------------------

                        if (obj_self) |obj_s| { //-- adding self if obj is a method of an object
                            try frame.put(obj.args[0].Name.str, obj_s);
                        }

                        const start: usize = if (obj_self != null) 1 else 0;
                        const len = obj.args.len;
                        for (start..len) |index| { //-- adding args
                            const arg_node = obj.args[index];
                            const index2: usize = if (obj_self != null) index - 1 else index;
                            const arg = args.items[index2];
                            try frame.put(arg_node.Name.str, arg);
                        }

                        for (len..given_arg_len) |index| { //-- adding kwargs
                            const arg = args.items[index - 1];
                            const kw = obj.kwargs[index - len].Keyword;

                            try frame.put(kw.arg.Name.str, arg);
                        }

                        const kw_len = given_arg_len - obj.args.len;
                        for (kw_len..obj.kwargs.len) |index| { //-- adding kwargs
                            const kw = obj.kwargs[index].Keyword;

                            const val = switch (kw.value.*) {
                                .String => |str| Type{ .Str = str },
                                .Constant => |con| switch (con) {
                                    .Int => |int| Type{ .Int = int },
                                    .Float => |float| Type{ .Float = float },
                                },
                                else => unreachable,
                            };

                            try frame.put(kw.arg.Name.str, val);
                        }

                        if (print_config.kwargs) |kw_frame| { //-- adding kwargs
                            var it = kw_frame.table.iterator();
                            while (it.next()) |entry| {
                                try frame.put(entry.key_ptr.*, entry.value_ptr.*);
                            }
                            kw_frame.deinit();
                        }

                        //-- track the stack for scope managemrnt
                        try self.stack_tracker.append(.{ .name = obj.name, .pos = self.stack.items.len });

                        const body = disassabled_code[obj.pos];
                        try self.run(body.items, .{});
                    },
                    else => unreachable,
                };
            },

            .MAKE_FUNCTION => { //-- fix the whole system
                const obj_ptr = self.stack.pop();

                try self.stack.append(obj_ptr);
            },

            .MAKE_STRUCT => {
                const st = self.stack.pop();
                try self.symbol_table.add(st.Struct.name);

                const body = disassabled_code[st.Struct.pos];
                try self.run(body.items, .{});

                const val = .{ .Struct2 = .{ .name = st.Struct.name, .ind = self.symbol_table.len() } };
                try self.stack.append(val);
            },

            .BUILD_LIST => |list| {
                var new_list = std.ArrayList(Type).init(self.alloc);

                for (list) |item| {
                    const val = switch (item) {
                        .String => |str| Type{ .Str = str },
                        .Constant => |con| switch (con) {
                            .Int => |int| Type{ .Int = int },
                            .Float => |float| Type{ .Float = float },
                        },
                        else => unreachable,
                    };
                    try new_list.append(val);
                }

                const last_list = lists.getLast();
                try last_list.append(new_list);

                try self.stack.append(.{ .List = last_list.getLast() });
            },
            .BUILD_DICT => |dict| {
                var new_dict = HashMapType.init(self.alloc);
                for (dict) |pair| {
                    const pair_node = switch (pair) {
                        .Pair => |node| node,
                        .Newline => continue,
                        else => unreachable,
                    };
                    const value = switch (pair_node.left.*) {
                        .String => |str| Type{ .Str = str },
                        .Constant => |con| switch (con) {
                            .Int => |int| Type{ .Int = int },
                            .Float => |float| Type{ .Float = float },
                        },
                        else => unreachable,
                    };
                    try new_dict.put(value, self.stack.pop());
                }

                // try new_dict.put(Type{ .Str = "hi" }, Type{ .Int = 20 });
                try dicts.append(new_dict);

                try self.stack.append(.{ .Dict = dicts.getLast() });
            },

            .BINARY_SUBCR => {
                const index = self.stack.pop();
                const obj = self.stack.pop();
                switch (obj) {
                    .List => |list| {
                        const uind: usize = @intCast(index.Int);
                        try self.stack.append(list.items[uind]);
                    },
                    .Dict => |dict| {
                        try self.stack.append(dict.get(index).?);
                    },
                    else => unreachable,
                }
            },

            .BINARY_SLICE => {
                const upper: usize = @intCast(self.stack.pop().Int);
                const lower: usize = @intCast(self.stack.pop().Int);
                const list = self.stack.pop().List;

                const slice = list.items[lower..upper];

                var new_slice = std.ArrayList(Type).init(self.alloc);
                try new_slice.appendSlice(slice);

                try lists.getLast().append(new_slice);

                try self.stack.append(Type{ .List = lists.getLast().getLast() });
            },

            .STORE_SUBSCR => {
                const index = self.stack.pop();
                const obj = self.stack.pop();
                switch (obj) {
                    .List => |list| {
                        const uind: usize = @intCast(index.Int);
                        list.items[uind] = self.stack.pop();
                    },
                    .Dict => |dict| {
                        try dict.put(index, self.stack.pop());
                    },
                    else => unreachable,
                }
            },

            // .POP_TOP => _ = self.stack.pop(),
            .PUSH_NULL => try self.stack.append(.NULL),

            else => {},
        }
    }
}

fn BinaryAdd(lval: Type, rval: Type) Type {
    return switch (lval) {
        .Int => |int| switch (rval) {
            .Int => |rint| Type{ .Int = int + rint },
            .Float => |float| Type{ .Float = float + @as(AST.FloatType, @floatFromInt(int)) },
            else => unreachable,
        },

        .Float => |float| switch (rval) {
            .Int => |rint| Type{ .Float = float + @as(AST.FloatType, @floatFromInt(rint)) },
            .Float => |rfloat| Type{ .Float = float + rfloat },
            else => unreachable,
        },

        else => unreachable,
    };
}

fn BinarySub(lval: Type, rval: Type) Type {
    return switch (lval) {
        .Int => |int| switch (rval) {
            .Int => |rint| Type{ .Int = int - rint },
            .Float => |float| Type{ .Float = float - @as(AST.FloatType, @floatFromInt(int)) },
            else => unreachable,
        },

        .Float => |float| switch (rval) {
            .Int => |rint| Type{ .Float = float - @as(AST.FloatType, @floatFromInt(rint)) },
            .Float => |rfloat| Type{ .Float = float - rfloat },
            else => unreachable,
        },

        else => unreachable,
    };
}

fn BinaryMul(lval: Type, rval: Type) Type {
    return Type{ .Int = lval.Int * rval.Int };
}

fn BinaryDiv(lval: Type, rval: Type) Type {
    return Type{ .Int = @divFloor(lval.Int, rval.Int) };
}

fn BinaryCmpEq(lval: Type, rval: Type) Type {
    const cmp = std.meta.eql(lval, rval);

    return Type{ .Int = @intFromBool(cmp) };
}

fn BinaryGt(lval: Type, rval: Type) Type {
    return Type{ .Int = @intFromBool(lval.Int > rval.Int) };
}

fn BinaryLt(lval: Type, rval: Type) Type {
    return Type{ .Int = @intFromBool(lval.Int < rval.Int) };
}

fn BinaryLte(lval: Type, rval: Type) Type {
    return Type{ .Int = @intFromBool(lval.Int <= rval.Int) };
}

fn BinaryGte(lval: Type, rval: Type) Type {
    return Type{ .Int = @intFromBool(lval.Int >= rval.Int) };
}

fn get_int_from_const(self: *Self, obj: Inst.Const) usize {
    return switch (obj) {
        .String => |str| @intCast(self.symbol_table.getLast().get(str).?.Int),
        .Int => |int| @intCast(int),
        //  @compileError("cant use float as a constant in iter"),
        else => unreachable,
    };
}

pub const PrintConfig = struct {
    sep: []const u8 = " ",
    end: []const u8 = "\n",
    kwargs: ?*SymbolTable.SymbolFrame = null,
};

fn fprint(args: []Type, config: PrintConfig) !void {
    var sep: []const u8 = " ";
    var end: []const u8 = "\n";

    if (config.kwargs) |kwargs| {
        if (kwargs.get("sep")) |val|
            sep = val.Str;

        if (kwargs.get("end")) |val|
            end = val.Str;

        defer kwargs.deinit();
    }

    // std.debug.print("args: {any}\n", .{args});
    for (args) |arg| {
        try switch (arg) {
            .Int => |int| stdOut.print("{d}", .{int}),
            .Float => |float| stdOut.print("{d}", .{float}),
            .Str => |str| stdOut.print("{s}", .{str}),
            .List => |list| print_list(list.items),
            .Dict => |dict| print_dict(dict),
            else => |unknown| stdOut.print("{s}", .{@tagName(unknown)}),
        };

        try stdOut.writeAll(sep);
    }

    try stdOut.writeAll(end);
}

fn fprint_ex(arg: Type, config: PrintConfig) anyerror!void {
    // std.debug.print("args: {any}\n", .{args});
    try switch (arg) {
        .Int => |int| stdOut.print("{d}", .{int}),
        .Str => |str| stdOut.print("{s}", .{str}),
        .List => |list| print_list(list.items),
        .Dict => |dict| print_dict(dict),
        else => |unknown| stdOut.print("{s}", .{@tagName(unknown)}),
    };

    try stdOut.writeAll(config.end);
}

fn print_list(list: []Type) !void {
    try stdOut.writeByte('[');
    if (list.len > 0) {
        for (0..list.len - 1) |ind| {
            try fprint_ex(list[ind], .{ .end = ", " });
        }
        try fprint_ex(list[list.len - 1], .{ .end = "" });
    }
    try stdOut.writeByte(']');
}

fn print_dict(dict: *Builtin.HashM) !void {
    try stdOut.writeAll("{ ");

    const it = dict.iterator();
    const keys = it.keys;
    const values = it.values;

    for (0..it.len - 1) |ind| {
        try fprint_ex(keys[ind], .{ .end = "" });
        try stdOut.print(" : ", .{});
        try fprint_ex(values[ind], .{ .end = "" });
        try stdOut.writeAll(", ");
    }

    try fprint_ex(keys[it.len - 1], .{ .end = "" });
    try stdOut.print(" : ", .{});
    try fprint_ex(values[it.len - 1], .{ .end = "" });

    try stdOut.writeAll(" }");
}

pub fn dump_symbol(self: Self) !void {
    var it = self.symbol_table.iterator();

    while (it.next()) |entry| {
        std.debug.print("{any} : {any}\n", .{ entry.key_ptr.*, entry.value_ptr });
    }
}
