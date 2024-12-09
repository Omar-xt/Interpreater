const std = @import("std");
const log = @import("log");
const AST = @import("AST.zig");
const Token = @import("Tokenizer.zig").Token;
const Error = @import("Error.zig");
const Node = AST.Node;

const activeTag = std.meta.activeTag;

tokens: []Token,
err: Error,
const Self = @This();

pub fn init(tokens: []Token) Self {
    return Self{
        .err = Error.init(tokens, undefined),
        .tokens = tokens,
    };
}

pub fn analyze_all(self: *Self, tree: []Node) !void {
    for (tree) |node| {
        try self.analyze(node);
    }
}

fn analyze(self: *Self, node: Node) !void {
    switch (node) {
        .BinOp => |op| {
            const lv = activeTag(op.left.*);
            const rv = activeTag(op.right.*);

            if (lv != rv) {
                try self.err.raise_ex(.{ .TypeError = .{ .BinOP = node } }, op.ind);
                // log.print("in line number {d}\n", .{token.line_no});
                // log.debug("{s}\n", .{token.line});
                log.err("Cant add {s} with {s}\n", .{ @tagName(lv), @tagName(rv) });
            }
        },
        .Assign => |ass| {
            try self.analyze(ass.value.*);
        },
        else => {},
    }
}
