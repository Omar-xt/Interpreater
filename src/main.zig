const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const AST = @import("AST.zig");
const CodeGen = @import("CodeGen.zig");
const Instruction = @import("Instruction.zig");
const VM = @import("VM.zig");
const Compiler = @import("Compiler.zig");
const Dump = @import("Dump.zig");
const Semantic = @import("Semantic.zig");
const log = @import("log");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa_alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa_alloc);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    var lib = try std.DynLib.open("zig-out/lib/libabc.so");
    defer lib.close();

    const pr = lib.lookup(*const fn ([*c]const u8) void, "print").?;
    pr("print");
    // _ = pr;

    const path = "scripts/src1.sm";

    var compiler = Compiler.init(arena_alloc);
    // defer compiler.deinit();
    try compiler.modules.ensureTotalCapacity(10);

    const module = try compiler.compile_ex(path, false);

    // //-- Tokens
    try Tokenizer.dump(module.tokens);

    // //-- Ast
    for (module.tree) |node| {
        std.debug.print("n: {any}\n", .{node});
    }

    //-- Semantic analysis
    var semantic = Semantic.init(module.tokens);
    semantic.analyze_all(module.tree) catch |err| switch (err) {
        error.runError => try semantic.err.flush(), //-- dump the errors
        else => return err,
    };

    var inst_token_map = module.codegen.get_inst_token_map();
    defer inst_token_map.deinit();

    const dis = module.codegen.get_dis();
    const tokens = module.tokens;
    const program = module.instructions;

    try Instruction.dump(program);

    // try Dump.dump(program);

    std.debug.print("\n----------Running Programe---------------\n", .{});

    //--------- VM -----------------------
    var vm = try VM.init(inst_token_map, tokens, dis, gpa_alloc);
    defer vm.deinit();

    try vm.run(program, .{});
    //------------------------------------

    // log.print("comptime fmt: []const u8\n", .{});
    // l("comptime fmt: []const u8\n", .{});

    // std.debug.print("\n------------------Extra Data----------------\n", .{});

    // std.debug.print("{any}\n", .{VM.lists.array.items});

    // for (vm.stack_tracker.items) |t| {
    //     std.debug.print("st: {s} : {d}\n", .{ t.name, t.pos });
    // }

    // for (tm.items) |t| {
    //     std.debug.print("{d} : {s}\n", .{ t, tokens[t].str });
    // }

    // var it = inst_token_map.iterator();
    // while (it.next()) |entry| {
    //     std.debug.print("key: {d} | val: {s}\n", .{ entry.key_ptr.*, tokens[entry.value_ptr.*].str });
    // }

}
