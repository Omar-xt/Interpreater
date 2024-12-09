const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

const Inst = @import("Instruction.zig");
const Instruction = Inst.Instruction;

const AST = @import("AST.zig");
const Node = AST.Node;

const CodeGen = @import("CodeGen.zig");

pub const Module = struct {
    src: []u8,
    tokens: []Tokenizer.Token,
    ast: *AST,
    tree: []Node,
    tm: *std.ArrayList(usize),
    codegen: *CodeGen,
    instructions: []Instruction,
    name: []const u8,
    alloc: std.mem.Allocator,

    pub fn deinit(self: *Module) void {
        self.codegen.deinit();
        self.tm.deinit();
        self.ast.deinit();
        self.alloc.free(self.tokens);

        self.alloc.destroy(self.codegen);
        self.alloc.destroy(self.tm);
        self.alloc.destroy(self.ast);
        self.alloc.free(self.src);
    }
};

modules: std.ArrayList(*Module),
stack: std.ArrayList(*Module),
alloc: std.mem.Allocator,
const Self = @This();

pub fn init(alloc: std.mem.Allocator) Self {
    return Self{
        .modules = std.ArrayList(*Module).init(alloc),
        .stack = std.ArrayList(*Module).init(alloc),
        .alloc = alloc,
    };
}

pub fn deinit(self: *Self) void {
    var ind: usize = self.modules.items.len - 1;
    std.debug.print("\n", .{});
    while (true) : (ind -= 1) {
        const module = self.modules.pop();
        std.debug.print("->freeing: {s}\n", .{module.name});
        self.alloc.free(module.name);
        module.deinit();
        self.alloc.destroy(module);

        if (ind == 0) break;
    }
    self.modules.deinit();
    self.stack.deinit();
}

/// returned slice should be freed by the user
fn get_src(self: Self, path: []const u8) ![]u8 {
    var cwd = std.fs.cwd();
    const file = try cwd.openFile(path, .{});
    const src = try self.alloc.alloc(u8, try file.getEndPos());
    _ = try file.readAll(src);
    file.close();

    return src;
}

pub fn get_compiled(self: *Self, path: []const u8, flag: bool) !*Module {
    const src = try self.get_src(path);

    var tokenizer = try Tokenizer.init(src, self.alloc);
    const token_slice = try tokenizer.tokenize();
    tokenizer.deinit();

    const tokens = if (flag == true) token_slice[0 .. token_slice.len - 2] else token_slice;

    const ast = try self.alloc.create(AST);
    ast.* = try AST.init(tokens, self, self.alloc);
    const tree = try ast.get_ast_ex();
    const token_map = ast.get_token_map();

    const tm = try self.alloc.create(std.ArrayList(usize));
    tm.* = std.ArrayList(usize).init(self.alloc);

    for (token_map) |ind| {
        const token = tokens[ind];

        var not_found = true;

        for (tm.items) |tind| {
            const ttok = tokens[tind];

            if (std.mem.eql(u8, ttok.str, token.str)) {
                not_found = false;
                break;
            }
        }

        if (not_found) {
            try tm.append(ind);
        }
    }

    var codegen = try self.alloc.create(CodeGen);
    codegen.* = try CodeGen.init(tm, self.alloc);
    const program = try codegen.get_instructions(tree);

    const module = try self.alloc.create(Module);
    module.* = Module{
        .name = try self.alloc.dupe(u8, path),
        .src = src,
        .tokens = token_slice,
        .ast = ast,
        .tree = tree,
        .tm = tm,
        .codegen = codegen,
        .instructions = program,
        .alloc = self.alloc,
    };

    return module;
}

pub fn compile(self: *Self, path: []const u8, flag: bool) !void {
    const module = try self.get_compiled(path, flag);
    try self.modules.append(module);

    if (self.stack.items.len > 0) {
        for (self.stack.items) |mod|
            try self.modules.append(mod);
        self.stack.clearRetainingCapacity();
    }
}

pub fn compile_ex(self: *Self, path: []const u8, flag: bool) !*Module {
    const module = try self.get_compiled(path, flag);
    try self.modules.append(module);

    if (self.stack.items.len > 0) {
        for (self.stack.items) |mod|
            try self.modules.append(mod);
        self.stack.clearRetainingCapacity();
    }

    return module;
}

pub fn getLast(self: *Self) *Module {
    return self.modules.items[self.modules.items.len - 1];
    // return self.modules.getLast();
}
