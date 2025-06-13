#link "libc"

malloc  :: proc_decl(u64) -> ^s64
calloc  :: proc_decl(u64, u64) -> ^s64
realloc :: proc_decl(^s64, u64) -> ^s64
free    :: proc_decl(^s64)
