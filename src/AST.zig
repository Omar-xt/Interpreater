const std = @import("std");
const assert = std.debug.assert;
const Tokenizer = @import("Tokenizer.zig");
const Compiler = @import("Compiler.zig");
const log = @import("log");

const eql = Tokenizer.eql;

const Token = Tokenizer.Token;
pub const NodeArray = std.ArrayList(Node);
pub const IntType = i32;
pub const FloatType = f32;
const PrecisionTable = std.ArrayHashMap(Node, usize, Context, false);

tokens: TokenList,
gStack: NodeArray,
/// Store the indexs of those tokens from which nodes are created.
/// Generally all the tokens are not used to create node.
token_map: std.ArrayList(usize),
alloc: std.mem.Allocator,
compiler: *Compiler,
precision_table: PrecisionTable,
const Self = @This();

const Context = struct {
    pub fn hash(_: Context, _: Node) u32 {
        return 0;
    }
    pub fn eql(_: Context, a: Node, b: Node, _: usize) bool {
        // return std.meta.eql(a, b);
        const ac = std.meta.activeTag(a);
        const bc = std.meta.activeTag(b);
        return ac == bc;
    }
};

const ReservedKeyword = enum {
    IF,
    FN,
    FOR,
    STRUCT,
    DICTONARY,
    RETURN,
    UNKNOWN,
    DYNLIB,
    IMPORT,
    BREAK,
};

pub const OP = enum {
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,

    LT,
    GT,
    LTE,
    GTE,

    /// Range
    RANGE,

    ASSIGN,
    LPARAN,
    RPARAN,
    LCURLY,
    RCURLY,
    LSQUARE,
    RSQUARE,

    DOT,
    COMMA,

    COLON,
};

const BinOp = struct {
    left: *Node,
    right: *Node,
    op: OP,
    ind: usize,
};

const Assign = struct {
    target: *Node,
    value: *Node,
};

const Attribute = struct {
    name: *Node,
    attr: *Node,
};

const Call = struct {
    func: *Node,
    args: []Node,
    kwargs: []Node,
    kw_names: [][]const u8,
};

const Compare = struct {
    left: *Node,
    right: *Node,
    option: OP,
};

const Pair = struct {
    left: *Node,
    right: *Node,
};

const Range = struct {
    start: *Node,
    end: *Node,
};

const IfBlock = struct {
    tests: *Node,
    body: []Node,
};

const ForBlock = struct {
    target: *Node,
    iter: *Node,
    body: []Node,
};

pub const FunctionDef = struct {
    name: *Node,
    args: []Node,
    kwargs: []Node,
    body: []Node,
};

const StructDef = struct {
    name: []const u8,
    body: []Node,
};

const Module = struct {
    name: *Node,
    body: []Node,
};

const Keyword = struct {
    arg: *Node,
    value: *Node,
};

const Subscript = struct {
    value: *Node,
    slice: *Node,
};

const Return = struct {
    value: *Node,
};

const Name = struct {
    str: []const u8,
    ind: usize,
};

pub const Constant = union(enum) {
    Int: IntType,
    Float: FloatType,
    // String: []const u8,
};

pub const Node = union(enum) {
    Name: Name,
    String: []const u8,
    Constant: Constant,

    Assign: Assign,

    //-- operators
    ASS,
    ADD: usize,
    SUB: usize,
    MUL: usize,
    DIV: usize,
    EQ,

    LT,
    GT,
    LTE,
    GTE,

    /// Range
    RANGE,

    ASSIGN,
    LPARAN,
    RPARAN,
    LCURLY,
    RCURLY,
    LSQUARE,
    RSQUARE,

    DOT,
    COMMA,

    COLON,
    //------

    // Sequence: Sequence,
    Range: Range,

    BinOp: BinOp,
    Compare: Compare,

    Pair: Pair,

    Call: Call,
    FunctionDef: FunctionDef,
    StructDef: StructDef,

    Module: Module,
    DynLib: []const u8,

    Keyword: Keyword,

    Attribute: Attribute,

    LParan,
    RParan,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    If: IfBlock,
    For: ForBlock,

    List: []Node,
    Dict: []Node,
    Subscript: Subscript,

    Comma,

    Nop,
    Newline,
    Return: Return,

    Comment,

    None,

    NoDelimiter,

    pub fn get_def_return_node(alloc: std.mem.Allocator) !Node {
        const val = try alloc.create(Node);
        val.* = Node{ .None = {} };
        return Node{ .Return = .{ .value = val } };
    }

    pub fn format(self: Node, comptime _: []const u8, _: std.fmt.FormatOptions, fwriter: anytype) !void {
        try switch (self) {
            .Name => |name| fwriter.print("Node: Name: {s} Ind: {d}", .{ name.str, name.ind }),
            else => |node| fwriter.print("Node: {s}", .{@tagName(node)}),
        };
    }
};

pub fn init(list: []Token, compiler: *Compiler, alloc: std.mem.Allocator) !Self {
    var pc = PrecisionTable.init(alloc);

    try pc.put(.{ .SUB = 0 }, 1);
    try pc.put(.{ .ADD = 0 }, 1);
    try pc.put(.{ .DIV = 0 }, 2);
    try pc.put(.{ .MUL = 0 }, 3);
    try pc.put(.LSquare, 4);

    return Self{
        .compiler = compiler,
        .tokens = TokenList.init(list),
        .gStack = NodeArray.init(alloc),
        .token_map = std.ArrayList(usize).init(alloc),
        .precision_table = pc,
        .alloc = alloc,
    };
}

pub fn deinit_(self: *Self, tree: []Node) void {
    self.token_map.deinit();
    self.gStack.deinit();

    for (tree) |node| {
        self.destroy(node);
    }
    self.alloc.free(tree);
}

pub fn deinit(self: *Self) void {
    self.token_map.deinit();

    for (self.gStack.items) |node| {
        self.destroy(node);
    }
    self.gStack.deinit();
    self.precision_table.deinit();
}

pub fn destroy(self: Self, node: Node) void {
    switch (node) {
        .Assign => |ass| {
            self.destroy(ass.target.*);
            self.alloc.destroy(ass.target);
            self.destroy(ass.value.*);
            self.alloc.destroy(ass.value);
        },
        .BinOp => |op| {
            self.destroy(op.left.*);
            self.alloc.destroy(op.left);
            self.destroy(op.right.*);
            self.alloc.destroy(op.right);
        },
        .Compare => |cmp| {
            self.destroy(cmp.left.*);
            self.alloc.destroy(cmp.left);
            self.destroy(cmp.right.*);
            self.alloc.destroy(cmp.right);
        },
        .Range => |ran| {
            self.destroy(ran.start.*);
            self.alloc.destroy(ran.start);
            self.destroy(ran.end.*);
            self.alloc.destroy(ran.end);
        },
        .List => |list| {
            self.alloc.free(list);
        },
        .Dict => |pairs| {
            for (pairs) |pair| {
                self.destroy(pair);
            }
            self.alloc.free(pairs);
        },
        .Pair => |pair| {
            self.destroy(pair.left.*);
            self.alloc.destroy(pair.left);
            self.destroy(pair.right.*);
            self.alloc.destroy(pair.right);
        },
        .Subscript => |sbc| {
            self.destroy(sbc.value.*);
            self.alloc.destroy(sbc.value);
            self.destroy(sbc.slice.*);
            self.alloc.destroy(sbc.slice);
        },
        .Attribute => |attr| {
            self.destroy(attr.name.*);
            self.alloc.destroy(attr.name);
            self.destroy(attr.attr.*);
            self.alloc.destroy(attr.attr);
        },
        .Call => |call| {
            self.destroy(call.func.*);
            self.alloc.destroy(call.func);
            for (call.args) |arg|
                self.destroy(arg);
            self.alloc.free(call.args);
            for (call.kwargs) |kwarg|
                self.destroy(kwarg);
            self.alloc.free(call.kwargs);
            self.alloc.free(call.kw_names);
        },
        .Keyword => |kw| {
            self.destroy(kw.arg.*);
            self.alloc.destroy(kw.arg);
            self.destroy(kw.value.*);
            self.alloc.destroy(kw.value);
        },
        .If => |block| {
            self.destroy(block.tests.*);
            self.alloc.destroy(block.tests);
            for (block.body) |expr|
                self.destroy(expr);
            self.alloc.free(block.body);
        },
        .For => |block| {
            self.destroy(block.target.*);
            self.alloc.destroy(block.target);
            self.destroy(block.iter.*);
            self.alloc.destroy(block.iter);
            for (block.body) |expr|
                self.destroy(expr);
            self.alloc.free(block.body);
        },
        .FunctionDef => |def| {
            self.alloc.destroy(def.name);
            for (def.body) |expr|
                self.destroy(expr);
            self.alloc.free(def.body);
            for (def.args) |arg|
                self.destroy(arg);
            self.alloc.free(def.args);
            for (def.kwargs) |kwarg|
                self.destroy(kwarg);
            self.alloc.free(def.kwargs);
        },
        .StructDef => |def| {
            for (def.body) |expr|
                self.destroy(expr);
            self.alloc.free(def.body);
        },
        .Module => |mod| {
            self.destroy(mod.name.*);
            self.alloc.destroy(mod.name);
            //-- body auto freeied by the module struct itself
            // for (mod.body) |expr|
            //     self.destroy(expr);
            // self.alloc.free(mod.body);
        },
        .Return => |ret| {
            self.destroy(ret.value.*);
            self.alloc.destroy(ret.value);
        },
        else => {},
    }
}

fn parse_number(str: []const u8) !Node {
    const isFloat = std.mem.containsAtLeast(u8, str, 1, ".");

    return switch (isFloat) {
        true => Node{ .Constant = .{ .Float = try std.fmt.parseFloat(FloatType, str) } },
        false => Node{ .Constant = .{ .Int = try std.fmt.parseInt(IntType, str, 10) } },
    };
}

fn parse_op(str: []const u8) OP {
    assert(str.len <= 2);

    if (str.len == 2) {
        if (eql(str, "==")) return OP.EQ;
        if (eql(str, "..")) return OP.RANGE;
        if (eql(str, "<=")) return OP.LTE;
        if (eql(str, ">=")) return OP.GTE;
    }

    return switch (str[0]) {
        '+' => OP.ADD,
        '-' => OP.SUB,
        '*' => OP.MUL,
        '/' => OP.DIV,
        '=' => OP.ASSIGN,
        '(' => OP.LPARAN,
        ')' => OP.RPARAN,
        ',' => OP.COMMA,
        '{' => OP.LCURLY,
        '}' => OP.RCURLY,
        '[' => OP.LSQUARE,
        ']' => OP.RSQUARE,
        '.' => OP.DOT,
        ':' => OP.COLON,
        '<' => OP.LT,
        '>' => OP.GT,
        else => unreachable,
    };
}

fn parse_op_node(self: Self, str: []const u8) Node {
    assert(str.len <= 2);

    if (str.len == 2) {
        if (eql(str, "==")) return Node.EQ;
        if (eql(str, "..")) return Node.RANGE;
        if (eql(str, "<=")) return Node.LTE;
        if (eql(str, ">=")) return Node.GTE;
    }
    return switch (str[0]) {
        '+' => Node{ .ADD = self.tokens.pos - 1 },
        '-' => Node{ .SUB = self.tokens.pos - 1 },
        '*' => Node{ .MUL = self.tokens.pos - 1 },
        '/' => Node{ .DIV = self.tokens.pos - 1 },
        '=' => Node.ASSIGN,
        '(' => Node.LParan,
        ')' => Node.RParan,
        ',' => Node.COMMA,
        '{' => Node.LCurly,
        '}' => Node.RCurly,
        '[' => Node.LSquare,
        ']' => Node.RSquare,
        '.' => Node.DOT,
        ':' => Node.COLON,
        '<' => Node.LT,
        '>' => Node.GT,
        else => unreachable,
    };
}

fn general_op_parse(self: *Self, op: Node, stack: *NodeArray) !Node {
    const left = try self.alloc.create(Node);
    const right = try self.alloc.create(Node);
    left.* = stack.pop();
    right.* = try self.parse_node(self.tokens.next().?, stack);

    const operator = parse_op(self.tokens.from_prev(2).str);
    return switch (op) {
        .ADD, .SUB, .MUL, .DIV => |ind| Node{ .BinOp = .{ .left = left, .right = right, .op = operator, .ind = ind } },
        .EQ, .LT, .GT, .LTE, .GTE => Node{ .Compare = .{ .left = left, .right = right, .option = operator } },
        .RANGE => Node{ .Range = .{ .start = left, .end = right } },
        .COLON => Node{ .Pair = .{ .left = left, .right = right } },
        else => unreachable,
    };
}

// fn general_op_parse_ex(self: *Self, op: OP, stack: *NodeArray) !Node {
//     log.info("{s}\n", .{self.tokens.from_prev(2).str});
//     log.info("{any}\n", .{op});
//     log.info("{any}\n", .{stack.getLast()});
//     // log.info("{s}\n", .{self.tokens.prev().str});
//     switch (parse_op(self.tokens.from_prev(2).str)) {
//         .ASSIGN => {
//             const right = try self.parse_node(self.tokens.next().?, stack);
//             log.info("{any}\n", .{right});
//             return Node{ .Number = 5 };
//             // switch (op) {
//             //     .SUB => {
//             //         right.Number *= 10;
//             //         log.info("{d}\n", .{right.Number});
//             //         return Node{ .Number = -1 };
//             //     },
//             //     else => unreachable,
//             // }
//         },

//         else => return try self.general_op_parse(op, stack),
//     }
// }

fn get_kwargs(self: *Self, args: *NodeArray) !NodeArray {
    var kwargs = NodeArray.init(self.alloc);

    std.debug.print("ag: {any}\n", .{args.items});

    //-- validate that keyword arguments are only at the end
    var kwargs_started: ?usize = null;
    for (args.items, 0..) |item, ind| {
        switch (item) {
            .Assign => if (kwargs_started == null) {
                kwargs_started = ind;
            },
            else => {
                if (kwargs_started != null) @panic("positional argument follows keyword argument");
            },
        }
    }
    if (kwargs_started == null) kwargs_started = args.items.len;
    //--------------------------------

    for (args.items.len - kwargs_started.?) |_| {
        const ass = args.pop().Assign;
        try kwargs.append(.{ .Keyword = .{ .arg = ass.target, .value = ass.value } });
    }

    //-- reversing the arr as it was popped from args array
    std.mem.reverse(Node, kwargs.items);

    return kwargs;
}

fn parse_fn_call(self: *Self, stack: *NodeArray) anyerror!Node {
    var args = try self.parse_while(.RParan);
    var kw_names = std.ArrayList([]const u8).init(self.alloc);

    const name = try self.alloc.create(Node);
    name.* = stack.pop();

    var kwargs = try self.get_kwargs(&args);

    for (kwargs.items) |kw| {
        try kw_names.append(kw.Keyword.arg.Name.str);
    }

    return Node{ .Call = .{
        .func = name,
        .args = try args.toOwnedSlice(),
        .kwargs = try kwargs.toOwnedSlice(),
        .kw_names = try kw_names.toOwnedSlice(),
    } };
}

fn check_keyword(str: []const u8) ReservedKeyword {
    if (eql(str, "if")) return .IF;
    if (eql(str, "for")) return .FOR;
    if (eql(str, "fn")) return .FN;
    if (eql(str, "struct")) return .STRUCT;
    if (eql(str, "return")) return .RETURN;
    if (eql(str, "return")) return .RETURN;
    if (eql(str, "break")) return .BREAK;
    if (eql(str, "import")) return .IMPORT;
    if (eql(str, "@dict")) return .DICTONARY;
    if (eql(str, "@DynLib")) return .DYNLIB;
    return .UNKNOWN;
}

fn parse_if_block(self: *Self, _: *NodeArray) !Node {
    const nodes = try self.parse_while(.LCurly);
    defer nodes.deinit();

    assert(nodes.items.len == 1);
    const @"test" = try self.alloc.create(Node);
    @"test".* = nodes.items[0];

    var body = try self.parse_while(.RCurly);

    return Node{ .If = .{ .tests = @"test", .body = try body.toOwnedSlice() } };
}

fn parse_for_block(self: *Self) !Node {
    const nodes = try self.parse_while_ex(Node{ .Name = .{ .str = "in", .ind = 0 } }, null);
    defer nodes.deinit();

    //-- parsed the target
    assert(nodes.items.len == 1);
    const target = try self.alloc.create(Node);
    target.* = nodes.items[0];

    //-- parse sequence
    const seq_nodes = try self.parse_while(.LCurly);
    defer seq_nodes.deinit();
    assert(nodes.items.len == 1);
    const sequence = try self.alloc.create(Node);
    sequence.* = seq_nodes.items[0];

    //-- parse body
    var body = try self.parse_while(.RCurly);

    return Node{ .For = .{
        .target = target,
        .iter = sequence,
        .body = try body.toOwnedSlice(),
    } };
}

fn parse_subscript(self: *Self, val: Node) anyerror!Node {
    const nodes = try self.parse_while(.RSquare);
    defer nodes.deinit();

    assert(nodes.items.len == 1);

    const value = try self.alloc.create(Node);
    value.* = val;

    const slice = try self.alloc.create(Node);
    slice.* = nodes.items[0];

    return Node{ .Subscript = .{ .value = value, .slice = slice } };
}

fn parse_args(self: *Self) !NodeArray { //-- unoptimized (used: 1)
    var stack = NodeArray.init(self.alloc);
    defer stack.deinit();

    const lparan = try self.parse_node(self.tokens.next().?, &stack);
    assert(lparan == .LParan);

    const args = try self.parse_while(.RParan);

    return args;
}

fn parse_block(self: *Self) !NodeArray {
    var stack = NodeArray.init(self.alloc);
    defer stack.deinit();

    const lcurly = try self.parse_node(self.tokens.next().?, &stack);
    assert(lcurly == .LCurly);

    const block = try self.parse_while(.RCurly);

    return block;
}

fn parse_fn_block(self: *Self) !Node {
    var stack = NodeArray.init(self.alloc);
    defer stack.deinit();

    const name = try self.alloc.create(Node);
    name.* = try self.parse_node(self.tokens.next().?, &stack);

    var args = try self.parse_args();
    var kwargs = try self.get_kwargs(&args);
    var body = try self.parse_block();

    log.debug("args: {any}\n", .{args.items});

    //-- manually addaing return statement if not added
    if (body.items.len > 0) {
        std.debug.print("last: {any}\n", .{body.getLast()});
        switch (body.getLast()) {
            .Return => {},
            else => try body.append(try Node.get_def_return_node(self.alloc)),
        }
    } else try body.append(try Node.get_def_return_node(self.alloc));

    return Node{ .FunctionDef = .{
        .name = name,
        .args = try args.toOwnedSlice(),
        .kwargs = try kwargs.toOwnedSlice(),
        .body = try body.toOwnedSlice(),
    } };
}

fn parse_struct_block(self: *Self, stack: *NodeArray) !Node {
    var name: []const u8 = "annonymous";
    const next = try self.parse_node(self.tokens.next().?, stack);

    switch (next) {
        .Name => |name_node| name = name_node.str,
        else => self.tokens.backStep(),
    }

    var nodes = try self.parse_block();
    return Node{ .StructDef = .{ .name = name, .body = try nodes.toOwnedSlice() } };
}

fn parse_distionary(self: *Self) !Node {
    var nodes = NodeArray.init(self.alloc);
    const next = try self.parse_node(self.tokens.next().?, &nodes);

    assert(next == .LCurly);

    // const dict = try self.parse_while(.RCurly, null);
    try self.parse_while_ip(.RCurly, &nodes);
    // try d.append(Node{ .ADD = 0 });
    log.debug("{any}\n", .{nodes.items});
    return Node{ .Dict = try nodes.toOwnedSlice() };
}

fn parse_attribute_access(self: *Self, stack: *NodeArray) !Node {
    const obj_name = stack.pop();
    const func_name = try self.parse_node(self.tokens.next().?, stack);
    const next = try self.parse_node(self.tokens.next().?, stack);

    switch (next) {
        .LParan => {
            try stack.append(func_name);
            var call = try self.parse_fn_call(stack);

            const name = try self.alloc.create(Node);
            name.* = obj_name;

            const attr = try self.alloc.create(Node);
            attr.* = Node{ .Attribute = .{ .name = name, .attr = call.Call.func } };

            call.Call.func = attr;

            return call;
        },
        else => {
            log.warn("{any}\n", .{next});
            unreachable;
        },
    }
}

fn parse_attribute_access_ex(self: *Self, stack: *NodeArray) !Node {
    const obj_name = stack.pop();
    const attr_node = try self.parse_node(self.tokens.next().?, stack);

    const name = try self.alloc.create(Node);
    name.* = obj_name;

    const attr = try self.alloc.create(Node);
    attr.* = attr_node;

    return Node{ .Attribute = .{ .name = name, .attr = attr } };
}

fn parse_module(self: *Self) !Node {
    var nodes = NodeArray.init(self.alloc);

    const name_node = try self.parse_node(self.tokens.next().?, &nodes);
    const name = try self.alloc.create(Node);
    name.* = name_node;

    var buf: [128]u8 = undefined;
    const file_name = try std.fmt.bufPrint(&buf, "scripts/{s}.sm", .{name.Name.str});
    std.debug.print("file_name: {s}\n", .{file_name});

    const mod = try self.compiler.get_compiled(file_name, true);
    try self.compiler.stack.append(mod);

    return Node{ .Module = .{ .name = name, .body = mod.tree } };
}

fn parse_dynlib(self: *Self) !Node {
    var nodes = NodeArray.init(self.alloc);
    defer nodes.deinit();

    const next = try self.parse_node(self.tokens.next().?, &nodes);
    assert(next == .LParan);

    const arg = try self.parse_node(self.tokens.next().?, &nodes);

    const right = try self.parse_node(self.tokens.next().?, &nodes);
    assert(right == .RParan);

    return Node{ .DynLib = arg.String };
}

fn parse_node_ex(self: *Self, token: Token, stack: *NodeArray) anyerror!Node {
    return switch (token.type) {
        .NAME => blk: {
            const keyword = check_keyword(token.str);

            try self.token_map.append(self.tokens.pos - 1);

            break :blk switch (keyword) {
                .IF => try self.parse_if_block(stack),
                .FN => try self.parse_fn_block(),
                .FOR => try self.parse_for_block(),
                .STRUCT => try self.parse_struct_block(),
                .RETURN => rblk: {
                    const val = try self.alloc.create(Node);
                    // val.* = try self.parse_expr(stack);
                    val.* = vblk: {
                        var rel = try self.relational_parse_ex();
                        defer rel.deinit();
                        try self.marge_all(&rel);
                        assert(rel.items.len == 1);
                        break :vblk rel.items[0];
                    };
                    break :rblk Node{ .Return = .{ .value = val } };
                },
                .BREAK => rblk: {
                    const val = try self.alloc.create(Node);
                    val.* = .None;
                    break :rblk Node{ .Return = .{ .value = val } };
                },
                .IMPORT => self.parse_module(),
                .UNKNOWN => Node{ .Name = token.str },
            };
        },
        .STRING => Node{ .String = token.str },
        .NUMBER => Node{ .Number = try parse_number(token.str) },
        .OP => blk: {
            const op = parse_op(token.str);

            break :blk switch (op) {
                .ASSIGN, .SUB, .ADD, .MUL, .DIV, .EQ, .RANGE, .COLON, .LT, .GT, .LTE, .GTE => |op_typ| try self.general_op_parse(op_typ, stack),

                .DOT => try self.parse_attribute_access_ex(stack),

                // .ASSIGN => .AssOP,
                // .SUB => .SubOP,

                // .ASSIGN => .AssignOP,

                .LPARAN => Node{ .LParan = {} },
                .RPARAN => Node{ .RParan = {} },

                .LCURLY => Node{ .LCurly = {} },
                .RCURLY => Node{ .RCurly = {} },

                .LSQUARE => Node{ .LSquare = {} },
                .RSQUARE => Node{ .RSquare = {} },

                .COMMA => Node{ .Comma = {} },
            };
        },
        .NEWLINE => .Newline,
        .COMMENT => .Comment,
        .RETURN => try Node.get_def_return_node(self.alloc),
        else => unreachable,
    };
}

fn parse_node(self: *Self, token: Token, stack: *NodeArray) anyerror!Node {
    return switch (token.type) {
        .NAME => blk: {
            const keyword = check_keyword(token.str);

            try self.token_map.append(self.tokens.pos - 1);

            break :blk switch (keyword) {
                .IF => try self.parse_if_block(stack),
                .FN => try self.parse_fn_block(),
                .FOR => try self.parse_for_block(),
                .STRUCT => try self.parse_struct_block(stack),
                .DICTONARY => try self.parse_distionary(),
                .RETURN => rblk: {
                    const val = try self.alloc.create(Node);
                    // val.* = try self.parse_expr(stack);
                    val.* = vblk: {
                        var rel = try self.relational_parse_ex();
                        defer rel.deinit();
                        try self.marge_all(&rel);
                        assert(rel.items.len == 1);
                        break :vblk rel.items[0];
                    };
                    break :rblk Node{ .Return = .{ .value = val } };
                },
                .BREAK => rblk: {
                    const val = try self.alloc.create(Node);
                    val.* = .None;
                    break :rblk Node{ .Return = .{ .value = val } };
                },
                .IMPORT => self.parse_module(),
                .DYNLIB => self.parse_dynlib(),
                .UNKNOWN => Node{ .Name = .{ .str = token.str, .ind = self.tokens.pos - 1 } },
            };
        },
        .STRING => Node{ .String = token.str },
        // .NUMBER => Node{ .Number = try parse_number(token.str) },
        .NUMBER => try parse_number(token.str),
        .OP => self.parse_op_node(token.str),
        .NEWLINE => .Newline,
        .COMMENT => .Comment,
        .RETURN => try Node.get_def_return_node(self.alloc),
        else => unreachable,
    };
}

fn relational_parse(self: *Self) !Node {
    var nodes = NodeArray.init(self.alloc);
    defer nodes.deinit();

    while (self.tokens.next()) |token| {
        const node = try self.parse_node(token, &nodes);

        switch (node) {
            .LSquare => switch (nodes.getLast()) {
                .Name => {
                    const subscript = try self.parse_subscript(nodes.pop());
                    return subscript;
                },
                else => {
                    var list = try self.parse_while(.RSquare, null);
                    return Node{ .List = try list.toOwnedSlice() };
                },
            },
            .LParan => { //-- parse fn call
                const call = try self.parse_fn_call(&nodes);
                try nodes.append(call);
                continue;
            },
            else => {},
        }

        if (nodes.items.len > 0 and is_relational_node(node) == false) {
            self.tokens.backStep();
            break;
        }

        try nodes.append(node);
    }

    assert(nodes.items.len == 1);

    return nodes.items[0];
}

fn is_relational_node(node: Node) bool {
    return switch (node) {
        .ASSIGN, .ADD, .SUB, .MUL, .DIV, .LParan, .LSquare, .RSquare, .COMMA => true,
        else => false,
    };
}

fn relational_parse_ex(self: *Self) anyerror!NodeArray {
    var nodes = NodeArray.init(self.alloc);

    var count: usize = 1;

    while (self.tokens.next()) |token| {
        var node = try self.parse_node(token, &nodes);

        switch (node) {
            .Newline => break,
            .DOT => {
                node = try self.parse_attribute_access_ex(&nodes);
                count += 1;
            },
            .LParan => {
                node = try self.parse_fn_call(&nodes);
                count += 1;
            },
            .LSquare => {
                var list = try self.parse_while(.RSquare);
                node = Node{ .List = try list.toOwnedSlice() };
            },
            .COMMA => continue,
            else => {},
        }

        if (is_relational_node(node)) {
            switch (node) {
                .RSquare => count += 1,
                .LSquare => count = 2,
                else => count += 2,
            }
        }

        log.info("node: {any} count: {d}\n", .{ node, count });

        if (count > 0) {
            count -= 1;
        } else break;

        try nodes.append(node);
    }

    self.tokens.backStep();
    return nodes;
}

//-- organize later |utility|
fn union_eql(a: Node, b: Node) bool {
    return switch (a) {
        .Name => std.mem.eql(u8, a.Name.str, b.Name.str),
        else => false,
    };
}

fn parse_while_ex(self: *Self, comptime delemiter_node: Node, stack: ?*NodeArray) !NodeArray {
    var nodes = if (stack) |st| st.* else NodeArray.init(self.alloc);

    var braces_count: usize = 0;
    const opposite_brace = switch (delemiter_node) {
        .RCurly => Node{ .LCurly = {} },
        else => Node{ .NoDelimiter = {} },
    };

    while (self.tokens.next()) |token| {
        const node = try self.parse_node(token, &nodes);
        // std.debug.print("{any}\n", .{node});

        switch (node) {
            .LParan => { //-- parse fn call
                const call = try self.parse_fn_call(&nodes);
                try nodes.append(call);
            },
            .Comma => continue,
            delemiter_node => {
                if (braces_count == 0 and union_eql(delemiter_node, node)) {
                    // self.tokens.backStep();
                    break;
                } else {
                    braces_count -|= 1;
                    try nodes.append(node);
                }
            },
            opposite_brace => braces_count += 1,
            else => try nodes.append(node),
        }
    }

    return nodes;
}

fn marge_all(self: *Self, nodes: *NodeArray) !void {
    // var out = NodeArray.init(self.alloc);

    while (nodes.items.len > 1) {
        var high_pricion_ind: usize = 0;
        var high_pricion: usize = 0;
        for (nodes.items, 0..) |node, ind| {
            if (self.precision_table.get(node)) |pc| {
                if (pc > high_pricion) {
                    high_pricion = pc;
                    high_pricion_ind = ind;
                }
            }
        }

        const node = nodes.items[high_pricion_ind];

        log.debug("{any}\n", .{node});

        switch (node) {
            .ADD, .SUB, .MUL, .DIV => |ind| {
                const left = try self.alloc.create(Node);
                const right = try self.alloc.create(Node);
                right.* = nodes.orderedRemove(high_pricion_ind + 1);
                _ = nodes.orderedRemove(high_pricion_ind);
                left.* = nodes.orderedRemove(high_pricion_ind - 1);

                const operator: OP = switch (node) {
                    .ADD => .ADD,
                    .SUB => .SUB,
                    .DIV => .DIV,
                    .MUL => .MUL,
                    else => unreachable,
                };

                const binOp = Node{ .BinOp = .{ .left = left, .right = right, .op = operator, .ind = ind } };
                try nodes.insert(high_pricion_ind - 1, binOp);
            },

            .LSquare => {
                switch (high_pricion_ind > 0) {
                    true => switch (nodes.items[high_pricion_ind - 1]) {
                        .Name => {
                            const value = try self.alloc.create(Node);
                            const slice = try self.alloc.create(Node);

                            slice.* = nodes.orderedRemove(high_pricion_ind + 1);
                            _ = nodes.orderedRemove(high_pricion_ind + 1);
                            _ = nodes.orderedRemove(high_pricion_ind);
                            value.* = nodes.orderedRemove(high_pricion_ind - 1);

                            const subscript = Node{ .Subscript = .{ .value = value, .slice = slice } };
                            try nodes.insert(high_pricion_ind - 1, subscript);
                        },
                        else => {},
                    },
                    false => {
                        var list = NodeArray.init(self.alloc);
                        while (true) {
                            const next = nodes.orderedRemove(high_pricion_ind + 1);
                            switch (next) {
                                .COMMA => continue,
                                .RSquare => break,
                                else => try list.append(next),
                            }
                        }
                        _ = nodes.orderedRemove(high_pricion_ind);
                        const list_node = Node{ .List = try list.toOwnedSlice() };
                        try nodes.insert(high_pricion_ind, list_node);
                    },
                }
            },

            else => {
                log.warn("{any}\n", .{node});
                log.warn("{any}\n", .{nodes.items});
                unreachable;
            },
        }
    }
}

fn parse_while(self: *Self, delemiter_node: anytype) !NodeArray {
    var nodes = NodeArray.init(self.alloc);
    try self.parse_while_ip(delemiter_node, &nodes);
    return nodes;
}

/// parse in place
fn parse_while_ip(self: *Self, delemiter_node: anytype, stack: *NodeArray) !void {
    var nodes = stack;

    var braces_count: usize = 0;
    const opposite_brace = switch (delemiter_node) {
        .RCurly => Node{ .LCurly = {} },
        else => Node{ .NoDelimiter = {} },
    };

    while (self.tokens.next()) |token| {
        const node = try self.parse_node(token, nodes);

        // log.debug("{any}\n", .{node});

        switch (node) {
            .SUB, .ADD, .MUL, .DIV, .EQ, .RANGE, .COLON, .LT, .GT, .LTE, .GTE => {
                const n = try self.general_op_parse(node, nodes);
                try nodes.append(n);
            },

            .DOT => {
                const attr_acc = try self.parse_attribute_access_ex(nodes);
                try nodes.append(attr_acc);
            },

            .ASSIGN => {
                const left = try self.alloc.create(Node);
                const right = try self.alloc.create(Node);

                // const next = try self.parse_node(self.tokens.next().?, nodes);
                // switch (next) {
                //     // .LSquare => {
                //     //     var list = try self.parse_while(.RSquare, null);
                //     //     right.* = Node{ .List = try list.toOwnedSlice() };
                //     // },
                //     else => {
                //         self.tokens.backStep();
                //         var a = try self.relational_parse_ex();
                //         defer a.deinit();
                //         log.info("{any}\n", .{a.items});

                //         try self.marge_all(&a);

                //         log.warn("{any}\n", .{a.items});

                //         assert(a.items.len == 1);
                //         right.* = a.items[0];
                //     },
                // }

                var a = try self.relational_parse_ex();
                defer a.deinit();
                log.info("{any}\n", .{a.items});

                try self.marge_all(&a);

                log.warn("{any}\n", .{a.items});

                assert(a.items.len == 1);
                right.* = a.items[0];

                left.* = nodes.pop();
                try nodes.append(Node{ .Assign = .{ .target = left, .value = right } });
            },
            .LParan => { //-- parse fn call
                const call = try self.parse_fn_call(nodes);
                try nodes.append(call);
            },
            .LSquare => {
                switch (nodes.getLast()) { //-- parse subscript
                    .Name => {
                        const subscript = try self.parse_subscript(nodes.pop());
                        try nodes.append(subscript);
                    },
                    else => |err| {
                        std.debug.print("invalid {any}\n", .{err});
                        @panic("Invalid subscript operatiom");
                    },
                }
            },
            .Comma, .Newline, .COMMA => continue,
            delemiter_node => if (braces_count == 0) break else {
                braces_count -= 1;
            },
            opposite_brace => braces_count += 1,
            else => try nodes.append(node),
        }
    }
}

fn parse_while_ex2(self: *Self, delemiter_node: anytype, stack: ?*NodeArray) !NodeArray {
    var nodes = if (stack) |st| st.* else NodeArray.init(self.alloc);

    var braces_count: usize = 0;
    const opposite_brace = switch (delemiter_node) {
        .RCurly => Node{ .LCurly = {} },
        else => Node{ .NoDelimiter = {} },
    };

    while (self.tokens.next()) |token| {
        const node = try self.parse_node(token, &nodes);

        switch (node) {
            // .AssOP => {
            //     const left = try self.alloc.create(Node);
            //     const right = try self.alloc.create(Node);

            //     const next = try self.parse_node(self.tokens.next().?, &nodes);
            //     switch (next) {
            //         .SubOP => {
            //             var val = try self.parse_node(self.tokens.next().?, &nodes);
            //             val.Number *= -1;

            //             right.* = val;
            //         },
            //         .LSquare => {
            //             var list = try self.parse_while(.RSquare, null);
            //             right.* = Node{ .List = try list.toOwnedSlice() };
            //         },
            //         else => unreachable,
            //     }

            //     left.* = nodes.pop();
            //     try nodes.append(Node{ .Assign = .{ .target = left, .value = right } });
            // },
            .LParan => { //-- parse fn call
                const call = try self.parse_fn_call(&nodes);
                try nodes.append(call);
            },
            .LSquare => {
                switch (nodes.getLast()) { //-- parse subscript
                    .Name => {
                        const subscript = try self.parse_subscript(nodes.pop());
                        try nodes.append(subscript);
                    },
                    else => |err| {
                        std.debug.print("invalid {any}\n", .{err});
                        @panic("Invalid subscript operatiom");
                    },
                }
            },
            .Comma, .Newline => continue,
            delemiter_node => if (braces_count == 0) break else {
                braces_count -= 1;
            },
            opposite_brace => braces_count += 1,
            else => try nodes.append(node),
        }
    }

    return nodes;
}

fn parse_nodes(self: *Self) !void {
    // while (self.tokens.next()) |token| {
    //     const node = try self.parse_node(token, &self.gStack);

    //     switch (node) {
    //         .LParan => { //-- parse fn call
    //             const call = try self.parse_fn_call(&self.gStack);
    //             try self.gStack.append(call);
    //         },
    //         .Comma => continue,
    //         else => try self.gStack.append(node),
    //     }
    // }
    const nodes = try self.parse_while(.Nop);
    defer nodes.deinit();
    try self.gStack.appendSlice(nodes.items);
}

pub fn get_ast(self: *Self) ![]Node {
    try self.parse_nodes();
    return self.gStack.toOwnedSlice();
}

pub fn get_ast_ex(self: *Self) ![]Node {
    try self.parse_nodes();
    return self.gStack.items;
}

pub fn get_ast_ex2(self: *Self) !void {
    try self.parse_nodes();
}

pub fn get_token_map(self: Self) []usize {
    return self.token_map.items;
}

const TokenList = struct {
    list: []Token,
    pos: usize,

    pub fn init(list: []Token) TokenList {
        return TokenList{
            .list = list,
            .pos = 0,
        };
    }

    pub fn next(self: *TokenList) ?Token {
        if (self.pos >= self.list.len) return null;

        const token = self.list[self.pos];
        self.pos += 1;
        return token;
    }

    pub fn prev(self: *TokenList) Token {
        return self.list[self.pos - 1];
    }

    pub fn from_prev(self: *TokenList, step: usize) Token {
        return self.list[self.pos - step];
    }

    pub fn backStep(self: *TokenList) void {
        self.pos -= 1;
    }

    pub fn last(self: TokenList) Token {
        return self.list[self.pos];
    }
};

const stdOut = std.io.getStdOut().writer();
var buffered_writer = std.io.bufferedWriter(stdOut);
const writer = buffered_writer.writer();
const prefix = "  ";
// var indent: usize = 2;
pub fn dump(tree: []Node) !void {
    for (tree) |node| {
        try fprint(node, 0);
    }

    try buffered_writer.flush();
}

fn fprint(node: Node, indent: usize) !void {
    switch (node) {
        .Assign => |ass| {
            try writer.print("{s}: \n", .{@tagName(node)});
            try recursive_print(prefix, indent + 1);
            try writer.print("target: ", .{});
            try fprint(ass.target.*, indent + 1);
            try writer.writeByte('\n');
            try recursive_print(prefix, indent + 1);
            try writer.print("value: ", .{});
            try fprint(ass.value.*, indent + 1);
            try writer.writeByte('\n');
        },
        .BinOp => |binop| {
            try writer.print("{s}: \n", .{@tagName(node)});
            try recursive_print(prefix, indent + 1);
            try writer.print("left: ", .{});
            try fprint(binop.left.*, indent + 1);
            try writer.writeByte('\n');
            try recursive_print(prefix, indent + 1);
            try writer.print("right: ", .{});
            try fprint(binop.right.*, indent + 1);
            try writer.writeByte('\n');
        },
        .Call => |call| {
            try writer.print("{s}: \n", .{@tagName(node)});
            try recursive_print(prefix, indent + 1);
            try writer.print("name: ", .{});
            try fprint(call.func.*, indent + 1);
            try writer.writeByte('\n');
            try recursive_print(prefix, indent + 1);
            try writer.print("args: ", .{});
            try writer.writeByte('\n');
            for (call.args) |arg| {
                try recursive_print(prefix, indent + 2);
                try fprint(arg, indent + 2);
                try writer.writeByte('\n');
            }
        },
        .If => |block| {
            try writer.print("{s}: \n", .{@tagName(node)});
            try recursive_print(prefix, indent + 1);
            try writer.print("test: ", .{});
            try fprint(block.tests.*, indent + 1);
            // try writer.writeByte('\n');
            try recursive_print(prefix, indent + 1);
            try writer.print("body: ", .{});
            try writer.writeByte('\n');
            for (block.body) |expr| {
                if (ignore_case(expr)) continue;
                try recursive_print(prefix, indent + 2);
                try fprint(expr, indent + 2);
                try writer.writeByte('\n');
            }
        },
        .Compare => |cmp| {
            try writer.print("{s}: \n", .{@tagName(node)});
            try recursive_print(prefix, indent + 1);
            try writer.print("left: ", .{});
            try fprint(cmp.left.*, indent + 1);
            try writer.writeByte('\n');
            try recursive_print(prefix, indent + 1);
            try writer.print("right: ", .{});
            try fprint(cmp.right.*, indent + 1);
            try writer.writeByte('\n');
            try recursive_print(prefix, indent + 1);
            try writer.print("op: {s}\n", .{@tagName(cmp.option)});
        },
        .FunctionDef => |def| {
            try writer.print("{s}: \n", .{@tagName(node)});
            try recursive_print(prefix, indent + 1);
            try writer.print("name: ", .{});
            try fprint(def.name.*, indent + 1);
            try writer.writeByte('\n');
            try recursive_print(prefix, indent + 1);
            try writer.print("args: ", .{});
            try writer.writeByte('\n');
            for (def.args) |arg| {
                try recursive_print(prefix, indent + 2);
                try fprint(arg, indent + 2);
                try writer.writeByte('\n');
            }
            try recursive_print(prefix, indent + 1);
            try writer.print("body: ", .{});
            try writer.writeByte('\n');
            for (def.body) |expr| {
                if (ignore_case(expr)) continue;
                try recursive_print(prefix, indent + 2);
                try fprint(expr, indent + 2);
                try writer.writeByte('\n');
            }
        },
        .Name, .String => |name| {
            try writer.print("{s} -> {s}", .{ @tagName(node), name.str });
        },
        .Number => |number| {
            try writer.print("{s} -> {d}", .{ @tagName(node), number });
        },
        .Return => {
            try writer.print("{s}: None\n", .{@tagName(node)});
        },
        else => {},
    }
}

fn recursive_print(str: []const u8, count: usize) !void {
    for (0..count) |_| {
        _ = try writer.write(str);
    }
}

fn ignore_case(node: Node) bool {
    return switch (node) {
        .Newline => true,
        else => false,
    };
}
