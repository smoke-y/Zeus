#link "libc"

malloc  :: proc_decl(u64) -> ptr
calloc  :: proc_decl(u64, u64) -> ptr
realloc :: proc_decl(ptr, u64) -> ptr
free    :: proc_decl(ptr)

memcpy :: proc_decl(ptr, ptr, s64) -> ptr
memcmp :: proc_decl(ptr, ptr, s64) -> s32
strcpy :: proc_decl(^char, ^char) -> ^char
strcmp :: proc_decl(^char, ^char) -> ^char
