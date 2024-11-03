const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const AST = @import("AST.zig");
const CodeGen = @import("CodeGen.zig");
const Instruction = @import("Instruction.zig");
const VM = @import("VM.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    // const stdin = std.io.getStdIn();

    // var readBuf: [32]u8 = undefined;
    // var len: usize = 0;
    // try stdin.read(&readBuf);

    const zsrc =
        \\const a = 10;
        // \\var b = 20;
    ;
    var zast = try std.zig.Ast.parse(alloc, zsrc, .zig);
    defer zast.deinit(alloc);
    const ztokens = zast.tokens;
    for (0..ztokens.len) |tok| {
        std.debug.print("{any}\n", .{ztokens.get(tok)});
    }

    const ren = try zast.render(alloc);
    defer alloc.free(ren);
    std.debug.print("{s}\n", .{ren});

    const znods = zast.nodes;
    for (0..znods.len) |nod| {
        std.debug.print("{any}\n", .{znods.get(nod)});
    }

    const cwd = std.fs.cwd();
    const file = try cwd.openFile("scripts/src1.sm", .{});
    const src = try alloc.alloc(u8, try file.getEndPos());
    defer alloc.free(src);
    _ = try file.readAll(src);

    var tokenizer = try Tokenizer.init(src, alloc);
    defer tokenizer.deinit();
    const tokens = try tokenizer.tokenize();
    defer alloc.free(tokens);
    try Tokenizer.dump(tokens);

    var ast = AST.init(tokens, alloc);
    const tree = try ast.get_ast();
    defer ast.deinit(tree);

    for (tree) |node| {
        std.debug.print("n: {any}\n", .{node});
    }

    std.debug.print("--------------------> AST\n", .{});
    try AST.dump(tree);
    std.debug.print("------------------------\n", .{});

    var codegen = try CodeGen.init(alloc);
    defer codegen.deinit();
    const program = try codegen.get_instructions(tree);

    try Instruction.dump(program);

    std.debug.print("\n----------Running Programe---------------\n", .{});

    var vm = try VM.init(alloc);
    defer vm.deinit();

    try vm.run(program);

    // try vm.dump_symbol();
}

const U = union(enum) {
    a: i32,
    b: []const u8,
};

test "ff" {
    const a = U{ .a = 10 };
    const b = U{ .a = 10 };

    const cmp = std.meta.eql(a, b);

    std.debug.print("{any}\n", .{cmp});
}
