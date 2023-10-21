const std = @import("std");

pub const opcodes = enum {
    FETCH_LITERAL,
    LOOKUP_VARIABLE,
    BIND,
    BRANCH_IF_FALSE,
    CALL_PROCEDURE,
    UNBIND,
    BRANCH,
    SAVE_CONTINUATION,
    APPLY,
};
