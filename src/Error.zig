const std = @import("std");
const log = @import("log");
const stdOut = std.io.getStdOut().writer();
var buffered_writer = std.io.bufferedWriter(stdOut);
const writer = buffered_writer.writer();

const Token = @import("Tokenizer.zig").Token;
const AST = @import("AST.zig");
const Node = AST.Node;

const FileName = "scr1.sm";

const activeTag = std.meta.activeTag;

tokens: []Token,
config: std.io.tty.Config,
inst_token_map: std.AutoArrayHashMap(usize, usize),
const Self = @This();

pub fn init(tokens: []Token, inst_token_map: std.AutoArrayHashMap(usize, usize)) Self {
    return Self{
        .tokens = tokens,
        .config = .escape_codes,
        .inst_token_map = inst_token_map,
    };
}

pub fn flush(_: Self) !void {
    try buffered_writer.flush();
}

//----------------
const FunArgs = struct {
    name: []const u8,
    takes: usize,
    given: usize,
};

const TypeError = union(enum) {
    func_args: FunArgs,
    BinOP: Node,
};

// const AttrError = struct {
//     name:
// };

const RunError = union(enum) {
    NameError,
    AttrError: []const u8,
    TypeError: TypeError,
};
//--------------------

fn general_dump(self: Self, token: Token) !void {
    try self.config.setColor(writer, .bold);
    try self.config.setColor(writer, .green);
    try writer.print("File \"{s}\", line no: {d}\n", .{ FileName, token.line_no });
    try self.config.setColor(writer, .reset);
    try writer.print(" {d:>5} |\n", .{token.line_no - 1});
    try writer.print(" {d:>5} |  {s}\x1b[1;91m{s}\x1b[0m{s}\n", .{ token.line_no, token.line[0..token.start], token.str, token.line[token.start + token.str.len ..] });
    try writer.print(" {d:>5} |  ", .{token.line_no + 1});
    try writer.print("\x1b[1;91m", .{});

    try writer.writeByteNTimes('-', token.start);

    try writer.print("^\x1b[0m\n", .{});
}

fn raise_type_error(self: Self, token: Token, err: TypeError) !void {
    try self.general_dump(token);
    try self.config.setColor(writer, .bold);
    try self.config.setColor(writer, .bright_red);
    try writer.writeAll("TypeError: ");
    try self.config.setColor(writer, .reset);

    switch (err) {
        .func_args => |er| {
            try writer.print("{s}() takes {d} positional argument but {d} were given.\n", .{ er.name, er.takes, er.given });
        },
        .BinOP => |op| {
            const binOp = op.BinOp;
            const lv = activeTag(binOp.left.*);
            const rv = activeTag(binOp.right.*);

            const op_type = switch (binOp.op) {
                .ADD => "add",
                .MUL => "multiplay",
                .DIV => "devide",
                .SUB => "subtract",
                else => unreachable,
            };
            try writer.print("Can't {s} {s} with {s}\n", .{ op_type, @tagName(lv), @tagName(rv) });
        },
    }
}

fn raise_name_error(self: Self, token: Token) !void {
    try self.general_dump(token);
    try writer.print("\x1b[0;31mNameError:\x1b[0;m {s} is not definied.!\n", .{token.str});
}

fn raise_attr_error(self: Self, token: Token, obj_name: []const u8) !void {
    try self.general_dump(token);
    try self.config.setColor(writer, .bold);
    try self.config.setColor(writer, .bright_red);
    try writer.writeAll("AttributeError: ");
    try self.config.setColor(writer, .reset);
    try writer.print("'{s}' object has no attribute '{s}'\n", .{ obj_name, token.str });
}

pub fn raise_ex(self: *Self, err: RunError, ind: usize) !void {
    const token = self.tokens[ind];

    try switch (err) {
        .NameError => self.raise_name_error(token),
        .TypeError => |type_err| self.raise_type_error(token, type_err),
        .AttrError => |obj_name| self.raise_attr_error(token, obj_name),
    };
    return error.runError;
}

pub fn raise(self: *Self, err: RunError, ind: usize) !void {
    const token_ind = self.inst_token_map.get(ind) orelse return;
    const token = self.tokens[token_ind];

    log.err("{any}\n", .{token});

    try switch (err) {
        .NameError => self.raise_name_error(token),
        .TypeError => |type_err| self.raise_type_error(token, type_err),
        .AttrError => |obj_name| self.raise_attr_error(token, obj_name),
    };
    return error.runError;
}
