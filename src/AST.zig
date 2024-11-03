const std = @import("std");
const assert = std.debug.assert;
const Tokenizer = @import("Tokenizer.zig");

const eql = Tokenizer.eql;

const Token = Tokenizer.Token;
pub const NodeArray = std.ArrayList(Node);
pub const NumberType = i32;

tokens: TokenList,
gStack: NodeArray,
alloc: std.mem.Allocator,
const Self = @This();

const ReservedKeyword = enum {
    IF,
    FN,
    RETURN,
    UNKNOWN,
};

pub const OP = enum {
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    ASSIGN,
    LPARAN,
    RPARAN,
    LCURLY,
    RCURLY,
    COMMA,
};

const BinOp = struct {
    left: *Node,
    right: *Node,
    op: OP,
};

const Assign = struct {
    target: *Node,
    value: *Node,
};

const Call = struct {
    name: *Node,
    args: []Node,
};

const Compare = struct {
    left: *Node,
    right: *Node,
    option: OP,
};

const IfBlock = struct {
    tests: *Node,
    body: []Node,
};

pub const FunctionDef = struct {
    name: *Node,
    args: []Node,
    body: []Node,
};

const Return = struct {
    value: *const Node,
};

pub const Node = union(enum) {
    Name: []const u8,
    String: []const u8,
    Number: NumberType,
    Assign: Assign,

    BinOp: BinOp,
    Compare: Compare,

    Call: Call,
    FunctionDef: FunctionDef,

    LParan,
    RParan,
    LCurly,
    RCurly,

    If: IfBlock,
    Body: []Node,

    Comma,

    Nop,
    Newline,
    Return: Return,

    None,

    NoDelimiter,
};

const U = union(enum) {
    Int: i32,
    Str: []const u8,
};

test "hmm" {
    const a = U{ .Int = 10 };
    // const b = U{ .Int = 12 };

    // const ac = std.meta.activeTag(a);
    // const bc = std.meta.activeTag(b);

    const cmp = std.meta.activeTag(a);
    const val = @field(a, @tagName(cmp));

    std.debug.print("{any}\n", .{val});
}

pub fn init(list: []Token, alloc: std.mem.Allocator) Self {
    return Self{ .tokens = TokenList.init(list), .gStack = NodeArray.init(alloc), .alloc = alloc };
}

pub fn deinit(self: Self, tree: []Node) void {
    self.gStack.deinit();
    defer self.alloc.free(tree);

    for (tree) |node| {
        self.destroy(node);
    }
}

pub fn destroy(self: Self, node: Node) void {
    switch (node) {
        .Assign => |ass| {
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
        .Call => |call| {
            self.alloc.destroy(call.name);
            for (call.args) |arg|
                self.destroy(arg);
            self.alloc.free(call.args);
        },
        .If => |block| {
            self.destroy(block.tests.*);
            self.alloc.destroy(block.tests);
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
        },
        .Return => |ret| {
            self.destroy(ret.value.*);
            self.alloc.destroy(ret.value);
        },
        else => {},
    }
}

fn parse_number(str: []const u8) !NumberType {
    return try std.fmt.parseInt(NumberType, str, 10);
}

fn parse_op(str: []const u8) OP {
    assert(str.len <= 2);

    if (str.len == 2) {
        if (eql(str, "==")) return OP.EQ;
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
        else => unreachable,
    };
}

fn parse_expr(self: *Self) !Node {
    var nodes = try self.parse_while(.Newline, null);
    defer nodes.deinit();

    assert(nodes.items.len <= 2);
    if (nodes.items.len == 2) {
        if (nodes.items[1] != .Return) {
            @panic("Invalid expression.!");
        } else self.tokens.backStep();
    }

    const out = nodes.items[0];
    return out;
}

fn general_op_parse(self: *Self, op: OP, stack: *NodeArray) !Node {
    const left = try self.alloc.create(Node);
    const right = try self.alloc.create(Node);
    left.* = stack.pop();
    right.* = try switch (op) {
        .ASSIGN => self.parse_expr(),
        else => self.parse_node(self.tokens.next().?, stack),
    };

    return switch (op) {
        OP.ASSIGN => Node{ .Assign = .{ .target = left, .value = right } },
        OP.ADD, OP.SUB, OP.MUL, OP.DIV => Node{ .BinOp = .{ .left = left, .right = right, .op = op } },
        OP.EQ => Node{ .Compare = .{ .left = left, .right = right, .option = op } },
        else => unreachable,
    };
}

fn parse_fn_call(self: *Self, stack: *NodeArray) anyerror!Node {
    var args = try self.parse_while(.RParan, null);

    const name = try self.alloc.create(Node);
    name.* = stack.pop();

    return Node{ .Call = .{ .name = name, .args = try args.toOwnedSlice() } };
}

fn check_keyword(str: []const u8) ReservedKeyword {
    if (eql(str, "if")) return .IF;
    if (eql(str, "fn")) return .FN;
    if (eql(str, "return")) return .RETURN;
    return .UNKNOWN;
}

fn parse_if_block(self: *Self, _: *NodeArray) !Node {
    const nodes = try self.parse_while(.LCurly, null);
    defer nodes.deinit();

    assert(nodes.items.len == 1);
    const @"test" = try self.alloc.create(Node);
    @"test".* = nodes.items[0];

    var body = try self.parse_while(.RCurly, null);

    return Node{ .If = .{ .tests = @"test", .body = try body.toOwnedSlice() } };
}

fn parse_args(self: *Self) ![]Node {
    var stack = NodeArray.init(self.alloc);
    defer stack.deinit();

    const lparan = try self.parse_node(self.tokens.next().?, &stack);
    assert(lparan == .LParan);

    var args = try self.parse_while(.RParan, null);

    std.debug.print("Args: {any}\n", .{args.items});

    return args.toOwnedSlice();
}

fn parse_block(self: *Self) ![]Node {
    var stack = NodeArray.init(self.alloc);
    defer stack.deinit();

    const lcurly = try self.parse_node(self.tokens.next().?, &stack);
    assert(lcurly == .LCurly);

    var block = try self.parse_while(.RCurly, null);

    return block.toOwnedSlice();
}

fn parse_fn_block(self: *Self) !Node {
    var stack = NodeArray.init(self.alloc);
    defer stack.deinit();

    const name = try self.alloc.create(Node);
    name.* = try self.parse_node(self.tokens.next().?, &stack);

    const args = try self.parse_args();
    const body = try self.parse_block();

    return Node{ .FunctionDef = .{ .name = name, .args = args, .body = body } };
}

fn parse_node(self: *Self, token: Token, stack: *NodeArray) anyerror!Node {
    return switch (token.type) {
        .NAME => blk: {
            const keyword = check_keyword(token.str);

            break :blk switch (keyword) {
                .IF => try self.parse_if_block(stack),
                .FN => try self.parse_fn_block(),
                .RETURN => rblk: {
                    const val = try self.alloc.create(Node);
                    val.* = try self.parse_expr();
                    const ret = Node{ .Return = .{ .value = val } };
                    break :rblk ret;
                },
                .UNKNOWN => Node{ .Name = token.str },
            };
        },
        .STRING => Node{ .String = token.str },
        .NUMBER => Node{ .Number = try parse_number(token.str) },
        .OP => blk: {
            const op = parse_op(token.str);

            break :blk switch (op) {
                .ASSIGN, .ADD, .SUB, .MUL, .DIV, .EQ => |op_typ| try self.general_op_parse(op_typ, stack),

                .LPARAN => Node{ .LParan = {} },
                .RPARAN => Node{ .RParan = {} },

                .LCURLY => Node{ .LCurly = {} },
                .RCURLY => Node{ .RCurly = {} },

                .COMMA => Node{ .Nop = {} },
            };
        },
        .NEWLINE => .Newline,
        .RETURN => blk: {
            const val = try self.alloc.create(Node);
            val.* = Node{ .None = {} };
            break :blk Node{ .Return = .{ .value = val } };
        },
        else => unreachable,
    };
}

fn parse_while(self: *Self, delemiter_node: anytype, stack: ?*NodeArray) !NodeArray {
    var nodes = if (stack) |st| st.* else NodeArray.init(self.alloc);

    var braces_count: usize = 0;
    const opposite_brace = switch (delemiter_node) {
        .RCurly => Node{ .LCurly = {} },
        else => Node{ .NoDelimiter = {} },
    };

    while (self.tokens.next()) |token| {
        const node = try self.parse_node(token, &nodes);

        // if (node == .Nop) continue;
        // if (node == delemiter_node) break;
        // try nodes.append(node);

        switch (node) {
            .LParan => { //-- parse fn call
                const call = try self.parse_fn_call(&nodes);
                try nodes.append(call);
            },
            .Nop => continue,
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
    while (self.tokens.next()) |token| {
        const node = try self.parse_node(token, &self.gStack);

        switch (node) {
            .LParan => { //-- parse fn call
                const call = try self.parse_fn_call(&self.gStack);
                try self.gStack.append(call);
            },
            .Nop => continue,
            else => try self.gStack.append(node),
        }
    }
}

pub fn get_ast(self: *Self) ![]Node {
    try self.parse_nodes();
    return self.gStack.toOwnedSlice();
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

    pub fn backStep(self: *TokenList) void {
        self.pos -= 1;
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
            try fprint(call.name.*, indent + 1);
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
            try writer.print("{s} -> {s}", .{ @tagName(node), name });
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
