const std = @import("std");

const Self = @This();
const TokenArray = std.ArrayList(Token);

pos: usize,
line_start_pos: usize,
input: []const u8,
lines: std.ArrayList([]const u8),
line_no: usize,
alloc: std.mem.Allocator,

pub const TokenType = enum {
    OP,
    NAME,
    NUMBER,
    STRING,
    ASSIGN,
    COMMENT,
    NEWLINE,
    NOP,
    RETURN,
};

pub const Token = struct {
    type: TokenType,
    str: []const u8,
    start: usize,
    line_no: usize,
    line: []const u8,

    pub fn get_default() Token {
        return Token{ .type = .NOP, .start = 0, .line = "", .line_no = 0, .str = "" };
    }

    pub fn format(self: Token, _: []const u8, _: std.fmt.FormatOptions, fwriter: anytype) !void {
        try fwriter.print("Token: {s}", .{@tagName(self.type)});
    }
};

pub fn init(src: []const u8, alloc: std.mem.Allocator) !Self {
    var lines = std.ArrayList([]const u8).init(alloc);

    var it = std.mem.splitScalar(u8, src, '\n');
    while (it.next()) |line| {
        try lines.append(line);
        std.debug.print("{s}\n", .{line});
    }

    return Self{
        .pos = 0,
        .line_start_pos = 0,
        .input = src,
        .lines = lines,
        .line_no = 1,
        .alloc = alloc,
    };
}

pub fn deinit(self: Self) void {
    self.lines.deinit();
}

pub fn eql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

fn valid_number(c: u8) bool {
    return switch (c) {
        '0'...'9', '-', '+' => true,
        else => false,
    };
}

fn valid_name(c: u8) bool {
    return switch (c) {
        '0'...'9', 'A'...'Z', 'a'...'z', '-', '_', '@' => true,
        else => false,
    };
}

fn valid_op(c: u8) bool {
    return switch (c) {
        '-', '+', '*', '/', '=', '(', ')', '{', '}', '[', ']', ',', '.', ':', '<', '>' => true,
        else => false,
    };
}

fn valid_token(c: u8) bool {
    return switch (c) {
        '0'...'9', 'A'...'Z', 'a'...'z', '-', '_', '+', '=' => true,
        else => false,
    };
}

fn valid_new_line(c: u8) bool {
    return c == '\n';
}

fn valid_comment(c: u8) bool {
    return c != '\n';
}

fn is_dQuote(c: u8) bool {
    return c != '"';
}

fn get_test_fn(typ: TokenType) *const fn (u8) bool {
    return switch (typ) {
        .OP => valid_op,
        .NAME => valid_name,
        .NUMBER => valid_number,
        .NEWLINE => valid_new_line,
        .COMMENT => valid_comment,
        .STRING => is_dQuote,
        else => @panic("NO valid test function found..!"),
    };
}

fn general_parse(self: *Self, typ: TokenType) !Token {
    const start = self.pos - self.line_start_pos;

    const testFn = get_test_fn(typ);
    var str = try self.consume_while(testFn);

    //-- manually making \n(92,110) str to escape sequence (\n)(10) because both are not same
    if (eql(str, &[2]u8{ 92, 110 })) {
        str = "\n";
    }

    return Token{
        .type = typ,
        .start = start,
        .line = self.lines.items[self.line_no - 1],
        .line_no = self.line_no,
        .str = str,
    };
}

fn general_parse_upto(self: *Self, typ: TokenType, lim: usize) !Token {
    const start = self.pos - self.line_start_pos;

    const testFn = get_test_fn(typ);
    const str = try self.consume_upto(testFn, lim);

    return Token{
        .type = typ,
        .start = start,
        .line = self.lines.items[self.line_no - 1],
        .line_no = self.line_no,
        .str = str,
    };
}

fn parse_token(self: *Self) !Token {
    std.debug.print("nc: {c}\n", .{self.next_char()});
    return try switch (self.next_char()) {
        '0'...'9' => self.general_parse(.NUMBER),
        'A'...'Z', 'a'...'z', '_', '@' => self.general_parse(.NAME),
        '*', '/', '(', ')', '{', '}', '[', ']', ',', ':' => self.general_parse_upto(.OP, 1),
        // '-', '+' => blk: {
        //     break :blk if (self.sec_next()) |c| switch (self.sec_next() orelse return) {
        //         '0'...'9' => self.general_parse(.NUMBER),
        //         else => self.general_parse_upto(.OP, 1),
        //     } else self.general_parse_upto(.OP, 1);
        // },
        '-', '+' => switch (self.sec_next() orelse return error.eof) {
            '0'...'9' => self.general_parse(.NUMBER),
            else => self.general_parse_upto(.OP, 1),
        },
        '.' => switch (self.sec_next() orelse return error.eof) {
            // '0'...'9' => self.general_parse(.NUMBER),
            else => self.general_parse_upto(.OP, 2),
        },
        '=', '<', '>' => self.general_parse_upto(.OP, 2),
        '"' => blk: {
            _ = self.consume_char();
            const str = self.general_parse(.STRING);
            _ = self.consume_char();
            break :blk str;
        },
        '#' => self.general_parse(.COMMENT),
        '\n' => self.general_parse_upto(.NEWLINE, 1),
        else => @panic("Invalid token..!"),
    };
}

fn parse_tokens(self: *Self) !TokenArray {
    var tokens = TokenArray.init(self.alloc);

    while (!self.eof()) {
        try self.consume_whitespace();

        const token = self.parse_token() catch |err| switch (err) {
            error.eof => break,
        };

        std.debug.print("token: {s}\n", .{token.str});
        try tokens.append(token);

        if (token.type == .NEWLINE) {
            self.line_no += 1;
            self.line_start_pos = self.pos;
        }
    }

    const new_line_token = Token{
        .line = self.lines.items[self.line_no - 1],
        .line_no = self.line_no,
        .start = self.pos - self.line_start_pos,
        .str = "\n",
        .type = .NEWLINE,
    };

    try tokens.append(new_line_token);

    const return_token = Token{
        .line = "",
        .line_no = self.line_no + 1,
        .start = 0,
        .str = "",
        .type = .RETURN,
    };

    try tokens.append(return_token);
    return tokens;
}

pub fn tokenize(self: *Self) ![]Token {
    var tokens = try self.parse_tokens();
    return tokens.toOwnedSlice();
}

//-- all the persing logic
//-- read the current carecter without consuming it
pub fn next_char(self: Self) u8 {
    return self.input[self.pos];
}

pub fn sec_next(self: Self) ?u8 {
    if (self.pos + 1 > self.input.len) return null;
    return self.input[self.pos + 1];
}

pub fn starts_with(self: Self, s: []const u8) bool {
    return std.mem.startsWith(u8, self.input[self.pos..], s);
}

pub fn expect(self: *Self, s: []const u8) !void {
    if (self.starts_with(s)) {
        self.pos += s.len;
    } else {
        var buf: [128]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Expected {{ {s} }} at byte {d} but not found", .{ s, self.pos });
        std.debug.print("{s}\n", .{self.input[self.pos - 100 .. self.pos]});
        @panic(msg);
    }
}

pub fn eof(self: Self) bool {
    return self.pos >= self.input.len;
}

//----

pub fn consume_char(self: *Self) u8 {
    const c = self.next_char();
    self.pos += 1;
    return c;
}

pub fn consume_chars(self: *Self, count: usize) void {
    self.pos += count;
}

pub fn consume_while(self: *Self, testFn: *const fn (u8) bool) ![]const u8 {
    const start = self.pos;
    while (!self.eof() and testFn(self.next_char())) {
        _ = self.consume_char();
    }
    return self.input[start..self.pos];
}

pub fn consume_upto(self: *Self, testFn: *const fn (u8) bool, lim: usize) ![]const u8 {
    const start = self.pos;
    var ind: usize = 0;
    while (!self.eof() and testFn(self.next_char()) and ind < lim) {
        _ = self.consume_char();
        ind += 1;
    }
    return self.input[start..self.pos];
}

fn valid_whitespace(c: u8) bool {
    return c == ' ' or c == '\t';
}

pub fn consume_whitespace(self: *Self) !void {
    _ = try self.consume_while(valid_whitespace);
}

fn consume_new_line(self: *Self) !void {
    const count = try self.consume_while(valid_new_line);
    self.line_no += count.len;
}

//----
pub fn dump(tokens: []Token) !void {
    for (tokens) |token| {
        var str = token.str;
        if (eql(token.str, "\n")) str = "\\n";
        std.debug.print("{s:<10} str:   {s:<10} start: {d:<6} line_no: {d:<6} line: {s}\n", .{
            @tagName(token.type), str, token.start, token.line_no, token.line,
        });
    }
}
